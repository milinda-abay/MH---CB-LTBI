# The set of Define* functions implement lazy evaluation and create respective objects.
# Ultimately a singular model object will have multiple strategies with multiple transition matrices and states.


DefineTransition <- function(..., state.names) {
    # Define an unevaluated transmission matrix, for use in model running later

    # Extract transition matrix from first arguments, of any number, and assign names
    unevaluated.transition.matrix <- lazyeval::lazy_dots(...)
    n.states <- sqrt(length(unevaluated.transition.matrix))
    names(unevaluated.transition.matrix) <- sprintf("cell_%i_%i", rep(seq_len(n.states), each = n.states), rep(seq_len(n.states), n.states))

    # Perform checks
    CheckSquare(n.states, state.names)
    CheckComplement(unevaluated.transition.matrix, n.states)

    # Define attributes of the unevaluated transmission matrix
    structure(unevaluated.transition.matrix, class = c("uneval_matrix", class(unevaluated.transition.matrix)), state.names = as.vector(state.names))
}


DefineParameters <- function(...) {
    # Define an unevaluated parameters list, for use in model running later

    unevaluated.parameter.list <- lazyeval::lazy_dots(...)
    structure(unevaluated.parameter.list, class = c("uneval_parameters", class(unevaluated.parameter.list)))
}


# The following functions perform various calculations at model runtime.
#------------------------------------------------------------------------#

CheckSquare <- function(root, states) {
    # Simple function to ensure that the second argument's number of elements is the square of the first argument

    if (length(states) != root) {
        stop("Transition matrix is not square of number of states")
    }
}

CheckComplement <- function(transition.matrix, dimension) {
    # Used by DefineTransition to verify only one CMP (complement) parameter per row in the transition matrix

    cmp.positions <- sapply(transition.matrix, function(x) x[1], simplify = TRUE) # interested in expression only, not environment
    cmp.positions <- cmp.positions == quote(CMP) # find the positions that are CMPs, converting to a logical vector
    dim(cmp.positions) <- c(dimension, dimension) # reshape list to logical array

    # Sum by columns because cmp.positions because is filled column-wise and so is transposed
    if (any(colSums(cmp.positions) > 1)) {
        stop("Only one 'CMP' is allowed per matrix row.")
    }
}

CreateArgumentList <- function(state.names, state.number) {

    
    # Create and initialise a list
    arglist <- rep(list(NA), state.number ^ 2)
    dim(arglist) <- c(state.number, state.number)

    # Returns a list of functions and attaches it to the calling object.
    list(

    update.list = function(listvalues) { arglist[] <<- listvalues },

    update.row = function(row, rowvalues) {
        #because it is column-wise
        arglist[, row] <<- rowvalues
    },
    update.cell = function(row, col, value) { arglist[[row, col]] <<- value },
    show.list = function() arglist,
    add.state.name = function(state.names) {
        arglist[[state.number ^ 2 + 1]] <<- state.names
        names(arglist)[state.number ^ 2 + 1] <<- "state.names"
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

    vic.mortality[Year == year & mrate == rate.assumption][DT, Rate, on = .(Age = AGEP, Sex = SEXP)]

}

# Look up the Reactivation rate
Get.RR <- function(DT, year) {

    RRates[DT[, .(AGERP, SEXP, ST = year - YARP)], Rate, on = .(Age = AGERP, Sex = SEXP, statetime = ST)]

}


Get.TBMR <- function(DT, year) {

    vic.tb.mortality[DT[, .(AGEP, SEXP)], rate, on = .(age = AGEP, sex = SEXP)]

}


Get.TEST <-  function(S) {
    
    as.numeric(tests.dt[tests == testing, ..S])
      
}

Get.TREATR <- function() {

    0.6818
    
}

Get.POP <- function() {

    .6

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

    # inserting the values back in
    y[posC] <- valC

    # permutate again to get original shape
    y <- aperm(y, perm = c(1, 3, 2))

    dim(y) <- c(z, l * l)

    colnames(y) <- names(tM)

    data.table(y)
}


# Performs matrix multiplication on each row (cohort) with the evaluated transition matrix for that row.
PerformMatrixMultiplication <- function(dM, tM, l, z) {

    # Make the current data matrix a list
    bar <- unlist(dM)

    # Make it an array then permute it
    dim(bar) <- c(z, l, 1)
    bar <- aperm(bar, perm = c(3, 2, 1))

    # Do the same for the transition matrix
    foo <- unlist(tM)
    dim(foo) <- c(z, l, l)
    foo <- aperm(foo, perm = c(3, 2, 1))

    # Carry out the matrix multiplication with a data.table frame.
    # Enables iteration and subsetting by using .I
    dM[, as.list(matrix(bar[,, .I], ncol = l) %*% foo[,, .I]), by = seq_len(z)]

}


# The primary function RunModel() calls to performs row by row transition calculations.
GetStateCounts <- function(DT, year) {

    z <- nrow(DT)
    l <- sqrt(length(transMatrix))

    # assign the current environment for evaluation. 
    # TODO - create a function for multiple parameters
    parameters$MR$env <- environment()
    parameters$RR$env <- environment()
    parameters$TBMR$env <- environment()
    parameters$TESTSN$env <- environment()
    parameters$TESTSP$env <- environment()
    parameters$TREATR$env <- environment()
    parameters$POP$env <- environment()

    

    # evaluate parameters 
    # NOTE: at this point both Get.MR() and Get.RR() functions are called by the evaluator.
    # Use param$* when using DefineTransition() 
    param <- lazy_eval(parameters)

    # a Hack for YARP < 2016, vic mortality doesnt have data to look up 
    param$MR[is.na(param$MR)] <- 0.01
    param$RR[is.na(param$RR)] <- 0.0013
    param$TBMR[is.na(param$TBMR)] <- 0.01


    # assign the current environment for evaluation. 
    for (i in 1:length(transMatrix)) {

        transMatrix[[i]]$env <- environment()

    }

    

    # Evaluates the transition matrix and insert a '-pi' placeholder for CMP.
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
    results <- PerformMatrixMultiplication(dM, tM, l, z)
    print("PMM End")
    print(Sys.time())

    #browser() # uncomment for testing

    results[, - c("seq_len")]

}

#------------------------------------------------------------------------#

# The main model runtime loop 
RunModel <- function(pop.output) {

    # initialise the calculation object
    pop.calculated <- copy(pop.output)

    while (markov.cycle != cycles) {

        print(nrow(pop.calculated))
        print(pop.calculated[1:10, .N, by = .(AGEP, cycle)])

        # The vectorised solution where the entire table is passed to GetStateCounts
        pop.calculated[, c(state.names) := GetStateCounts(pop.calculated, year)]

        # Update counters
        markov.cycle <- markov.cycle + 1
        year <- year + 1

        # Inflows for next cycle. 
        # A conditional flag use this for testing.
        modelinflow <- TRUE

        if (modelinflow == TRUE) {
            pop.inflow <- pop.master[YARP == year,][, cycle := NA]
        } else {
            pop.inflow <- NULL
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

