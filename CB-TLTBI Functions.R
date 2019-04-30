# The set of Define* functions implement lazy evaluation and create respective objects.
# Ultimately a singular model object will have multiple strategies with multiple transition matrices and states.


DefineTransition <- function(..., state.names) {
    # Define an unevaluated transmission matrix, for use in model running later

    # Extract transition matrix from first arguments, of any number, and assign names
    unevaluated.transition.matrix <- lazyeval::lazy_dots(...)
    n.expected.states <- sqrt(length(unevaluated.transition.matrix))
    names(unevaluated.transition.matrix) <-
      sprintf("cell_%i_%i",
              rep(seq_len(n.expected.states), each = n.expected.states),
              rep(seq_len(n.expected.states), n.expected.states))

    # Perform checks
    if (length(state.names) != n.expected.states) {
        stop("Transition matrix is not the square of the number of states")
    }
    CheckComplement(unevaluated.transition.matrix, n.expected.states)

    # Define attributes of the unevaluated transmission matrix
    structure(unevaluated.transition.matrix,
              class = c("uneval_matrix", class(unevaluated.transition.matrix)),
              state.names = as.vector(state.names))
}


DefineParameters <- function(...) {
    # Define an unevaluated parameters list, for use in model running later

    unevaluated.parameter.list <- lazyeval::lazy_dots(...)
    structure(unevaluated.parameter.list, class = c("uneval_parameters", class(unevaluated.parameter.list)))
}

DefineStates <- function(...) {

    state.values <- lazyeval::lazy_dots(...)
    structure(state.values, class = c("state", class(state.values)))

}


DefineStrategy <- function(..., transition.matrix) {
    # TODO - find a way to pass the state.names vector and create the state objects
    # using CreateStates and DefineStates.
    # Have a list of named states per state.names

    state.list <- list(...)
    states <- structure(state.list, class = c("uneval_state_list", class(state.list)))
    names(states) <- state.names
    structure(list(transition = transition.matrix, states = states),
              class = "uneval_model")

}





# The following functions perform various calculations at model runtime.
#------------------------------------------------------------------------#

CheckComplement <- function(transition.matrix, dimension) {
    # Used by DefineTransition to verify only one CMP (complement) parameter per row in the transition matrix

    # Interested in expression only, so disregard the environment (which is the second element)
    cmp.positions <- sapply(transition.matrix, function(x) x[1], simplify = TRUE)

    # Find the positions that are CMPs, converting to a logical vector
    cmp.positions <- cmp.positions == quote(CMP)

    # Reshape from list to array
    dim(cmp.positions) <- c(dimension, dimension)

    # Sum by columns because cmp.positions because is filled column-wise and so is transposed
    if (any(colSums(cmp.positions) > 1)) {
        stop("Only a maximum of one 'CMP' is allowed per matrix row.")
    }
}

CreateArgumentList <- function(state.names, state.number) {


    # Create and initialise a list
    arglist <- rep(list(NA), state.number ^ 2)
    dim(arglist) <- c(state.number, state.number)

    # Return a list of functions attached to the calling object.
    list(

    update.list = function(listvalues) { arglist[] <<- listvalues },

    update.row = function(row, rowvalues) {
        #because it is column-wise
        arglist[, row] <<- rowvalues
    },
    update.cell = function(row, col, value) { arglist[[col, row]] <<- value },
    show.list = function() aperm(arglist, c(2, 1)),
    add.state.name = function(state.names) {
        arglist[[state.number ^ 2 + 1]] <<- state.names
        names(arglist)[state.number ^ 2 + 1] <<- "state.names"
    },
    show.row = function(row) {

        arglist[, row]

    },
    drop.state.name = function() {
        arglist[state.number ^ 2 + 1] <<- NULL
        dim(arglist) <<- c(state.number, state.number)
    },

    save.list = function(list.name) {
        # TODO: need to figure out how to reference the calling object
        # or pass the object name as a parameter 
        saveRDS(arglist, paste("Data/", list.name, ".rds", sep = ""))
    },

    load.list = function(list.name) {
        arglist <<- readRDS(paste("Data/", list.name, ".rds", sep = ""))

    }

    )


}

# Look up the mortality rate from vic.mortality
Get.MR <- function(DT, year, rate.assumption = "High") {

    vic.mortality[Year == year & mrate == rate.assumption][DT, Prob, on = .(Age = AGEP, Sex = SEXP)]

}

# Look up the Reactivation rate
Get.RR <- function(DT, year) {

    RRates[DT[, .(AGERP, SEXP, ST = year - YARP)], Rate, on = .(Age = AGERP, Sex = SEXP, statetime = ST)]

}

# look up TB mortality rate
Get.TBMR <- function(xDT, year) {
    
    DT <- copy(xDT[,.(AGEP, SEXP)])

    # To lookup all ages beyond 95 & 97
    DT[AGEP > 95 & SEXP == "Male", AGEP := 95]
    DT[AGEP > 97 & SEXP == "Female", AGEP := 97]

    vic.tb.mortality[DT[, .(AGEP, SEXP)], Prob, on = .(age = AGEP, sex = SEXP)]

}

# look up test sensitivity / specificity 
Get.TEST <- function(S, testing) {

    as.numeric(tests.dt[tests == testing, ..S])

}

# look up treatment completion rate
Get.TREAT <- function(S, treat) {

    as.numeric(treatment.dt[treatment == treat, ..S])

}

# look up target population percentage
Get.POP <- function(DT, strategy) {

    
    ifelse(DT$YARP < 2020,
           switch(strategy$myname,
                  BO = 0,
                  S1 = 1,
                  S2 = 0,
                  S3 = 0.05,
                  S4 = 0.10,
                  S5 = 0.15,
                  0
                  ),
                  0.6
                  )

}


Get.UTILITY <- function(t) {

    as.numeric(utility.dt[treatment == t][, 2:24])

}


Get.DISCOUNT <- function() {

    .03

}





# Calculates the CMP value after evaluation of the promise objects in parameter and transition matrix.
CalculateCMP <- function(tM, l, z) {
    
    y <- unlist(tM)

    dim(y) <- c(z, l, l)

    # permutate the array
    y <- aperm(y, perm = c(1, 3, 2))

    # 3D logical array of -pi locations
    posC <- y == -pi

    # now set -pi's to 0
    y[posC] <- 0

    # dropping the 3rd dimension in order to select the rowSums in sequence.
    valC <- 1 - rowSums(y, dims = 2)[which(posC, arr.ind = TRUE)[, -3]]

    if (any(valC < 0)) {
        stop("Negative CMP value")

    }

    # inserting the values back in
    y[posC] <- valC

    # permutate again to get original shape
    y <- aperm(y, perm = c(1, 3, 2))

    dim(y) <- c(z, l * l)

    colnames(y) <- names(tM)

    data.table(y)
}


# Performs matrix multiplication on each row (cohort) with the evaluated transition matrix for that row.
PerformMatrixMultiplication <- function(dM, tM, l, z, markov.cycle, flow.cost, state.cost, utility) {

    # Make the current data matrix a list
    bar <- unlist(dM)

    # Make it an array then permute it
    dim(bar) <- c(z, l, 1)
    bar <- aperm(bar, perm = c(3, 2, 1))

    # Do the same for the transition matrix
    foo <- unlist(tM)
    dim(foo) <- c(z, l, l)
    foo <- aperm(foo, perm = c(3, 2, 1))

    # Carry out the matrix multiplication within the dM data.table frame.
    # By using .I it enables iteration and sub-setting the 3D arrays bar and foo.  
    # This results in a 2D array and enables matrix multiplication.

    flows <- dM[, as.list(bar[,, .I] %*% (foo[,, .I] - diag(diag(foo[,, .I])))), by = seq_len(z)]

    counts <- dM[, as.list(matrix(bar[,, .I], ncol = l) %*% foo[,, .I]), by = seq_len(z)]

    flows <- flows[, - c("seq_len")]
    counts <- counts[, - c("seq_len")]

    #TODO include discount

    if (markov.cycle == 0) {
        discount <- 1
    } else {
        discount <- (1 - discount) ^ markov.cycle
    }

    flow.cost <- discount * flow.cost
    state.cost <- discount * state.cost

    flow.cost <- flows[, Map("*", flow.cost, .SD)]
    count.cost <- counts[, Map("*", state.cost, .SD)]


    state.QALY <- counts[, Map("*", utility, .SD)]

    return(list(counts, flows, count.cost, flow.cost, state.QALY))

}


# The primary function RunModel() calls to perform row by row transition calculations.
GetStateCounts <- function(DT, year, strategy, testing, treatment, markov.cycle) {
    
    # collapsing the promise object 
    testing
    treatment

    transMatrix <- strategy$transition


    z <- nrow(DT)
    l <- sqrt(length(strategy$transition))



    # assign the current environment for evaluation. 
    # TODO - create a function for multiple parameters
    parameters$MR$env <- environment()
    parameters$RR$env <- environment()
    parameters$TBMR$env <- environment()
    parameters$TESTSN$env <- environment()
    parameters$TESTSP$env <- environment()
    parameters$TESTC$env <- environment()
    parameters$TREATR$env <- environment()
    parameters$TREATC$env <- environment()
    parameters$POP$env <- environment()
    parameters$UTILITY$env <- environment()
    parameters$TBCOST$env <- environment()

    unevaluated.flow.cost$env <- environment()
    unevaluated.state.cost$env <- environment()




    # evaluate parameters 
    # NOTE: at this point both Get.MR() and Get.RR() functions are called by the evaluator.
    # Use param$* when using DefineTransition() 
    param <- lazy_eval(parameters)


    flow.cost <- lazy_eval(unevaluated.flow.cost)
    state.cost <- lazy_eval(unevaluated.state.cost)
    utility <- param$UTILITY

    # a Hack for YARP < 2016, vic mortality doesn’t have data to look up
    
    param$MR[is.na(param$MR)] <- 0.01
    param$RR[is.na(param$RR)] <- 0.0013
    param$TBMR[is.na(param$TBMR)] <- 0.01

    # TODO (Milinda) - extract param$* and re assign to variables of same name.
    # As a result we can stop using param$ inside DefineTransition().
    #for (i in names(param)) {
    #assign(i, param[[i]])
    #}


    # assign the current environment for evaluation. 
    for (i in 1:length(transMatrix)) {

        transMatrix[[i]]$env <- environment()

    }

    # Evaluates the transition matrix and insert a '-pi' place-holder for CMP.
    tM <- lazy_eval(transMatrix, data = list(CMP = -pi))


    # Scalar values don't get evaluated into vectors
    # loop and manually expand to vectors
    for (i in 1:length(transMatrix)) {

        if (length(tM[[i]]) == 1) {

            tM[[i]] <- rep(tM[[i]], times = z)
        }
    }
    
    # Manipulates the tM to calculate the CMP
    tM <- CalculateCMP(tM, l, z)

    # Select the numeric state value columns in preparation for multiplication.
    dM <- DT[, ..state.names]

    print("PMM Start")
    print(Sys.time())
    results <- PerformMatrixMultiplication(dM, tM, l, z, markov.cycle, flow.cost, state.cost, utility)
    print("PMM End")
    print(Sys.time())


    names(results[[2]]) <- paste("V.", state.names, sep = "")
    names(results[[3]]) <- paste("SC.", state.names, sep = "")
    names(results[[4]]) <- paste("FC.", state.names, sep = "")
    names(results[[5]]) <- paste("SQ.", state.names, sep = "")
    # browser() # uncomment for testing

    results <- cbind(results[[1]], results[[2]], results[[3]], results[[4]], results[[5]])
         
}


#------------------------------------------------------------------------#

# The main model runtime loop 
RunModel <- function(pop.output, strategy, testing, treatment, start.year, cycles) {

    #To keep track of the current strategy name
    strategy$myname <- deparse(substitute(strategy))

    # initialise the calculation object
    pop.calculated <- copy(pop.output)

    year <- start.year # Initialise year with start.year
    markov.cycle <- 0 # Tracks the current cycle

    while (markov.cycle < cycles) {

        writeLines(sprintf("\nCommencing Markov cycle %i", markov.cycle))
        writeLines(sprintf("Current number of populations in the working matrix is %i", nrow(pop.calculated)))
        print(pop.calculated[1:10, .N, by = .(AGEP, cycle)])
        
        # The vectorised solution where the entire table is passed to GetStateCounts
        pop.calculated[, c(new.state.names) := GetStateCounts(pop.calculated, year, strategy, testing, treatment, markov.cycle)]

        # Update counters
        markov.cycle <- markov.cycle + 1
        year <- year + 1

        # Inflows for next cycle. 
        # A conditional flag use this for testing.
        modelinflow <- TRUE

        if (modelinflow) {
            pop.inflow <- pop.master[YARP == year,][, cycle := NA]
        }
        else {
            pop.inflow <- NULL # not convinced this is needed
        }


        # Aging the population in the calculation object
        pop.calculated[, AGEP := AGEP + 1]

        pop.calculated <- rbind(pop.calculated, pop.inflow)
        pop.calculated[, cycle := markov.cycle]


        # Calculate state values
        # CalculateStateValues(pop.calculated)

        # Saving state in pop.output
        pop.output <- rbind(pop.output, pop.calculated)
    }

    pop.output
}
