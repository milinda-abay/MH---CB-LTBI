# The primary function run_model() calls to perform row by row transition calculations.
get_state_counts <- function(DT, DT_state_count, year, strategy, markov_cycle) {

    # collapsing the promise object 
    testing <- lazy_eval(strategy$properties$test)
    treatment <- lazy_eval(strategy$properties$treatment)


    # Now access the HDF5 dataset
    DT <- setDT(DT[markov_cycle+1, 1,])

    transition_matrix <- strategy$transition

    z <- nrow(DT)
    l <- sqrt(length(transition_matrix))


    # assign the current environment for evaluation. 
    # TODO - create a function for multiple parameters
    parameters$mr$env <- environment()
    parameters$rr$env <- environment()
    parameters$tbmr$env <- environment()
    parameters$testsn$env <- environment()
    parameters$testsp$env <- environment()
    parameters$testc$env <- environment()
    parameters$treatr$env <- environment()
    parameters$treatc$env <- environment()
    parameters$treatsae$env <- environment()
    parameters$pop$env <- environment()
    parameters$utility$env <- environment()
    parameters$tbcost$env <- environment()

    unevaluated_flow_cost$env <- environment()
    unevaluated_state_cost$env <- environment()



    # evaluate parameters 
    # NOTE: at this point both get_mr() and get_rr() functions are called by the evaluator.
    # Use param$* when using define_transition() 
    param <- lazy_eval(parameters)

    # Extract param$* and re assign to variables of same name.
    # As a result we can stop using param$ inside define_transition().
    for (i in names(param)) {
        assign(i, param[[i]])
    }

    flow_cost <- lazy_eval(unevaluated_flow_cost)
    state_cost <- lazy_eval(unevaluated_state_cost)
    utility <- param$utility

    browser()
    if (dsa == TRUE) {

        # replace parameters  by values return form defined_dsa

    }


    # a Hack for YARP < 2016, vic mortality doesn?t have data to look up
    mr[is.na(mr)] <- 0.01
    rr[is.na(rr)] <- 0.0013
    tbmr[is.na(tbmr)] <- 0.01


    # assign the current environment for evaluation. 
    for (i in 1:length(transition_matrix)) {

        transition_matrix[[i]]$env <- environment()

    }

    # Evaluates the transition matrix and insert a '-pi' place-holder for CMP.
    tM <- lazy_eval(transition_matrix, data = list(CMP = -pi))


    # Scalar values don't get evaluated into vectors
    # loop and manually expand to vectors
    for (i in 1:length(transition_matrix)) {

        if (length(tM[[i]]) == 1) {

            tM[[i]] <- rep(tM[[i]], times = z)
        }
    }

    # Manipulates the tM to calculate the CMP
    tM <- calculate_complement(tM, l, z)

    # Select the numeric state value columns in preparation for multiplication.

    print("PMM Start")
    print(Sys.time())
    results <- perform_matrix_multiplication(DT_state_count, tM, l, z, markov_cycle, flow_cost, state_cost, utility, current_rows)
    print("PMM End")
    print(Sys.time())


    names(results[[1]]) <- state_names
    names(results[[2]]) <- state_names
    names(results[[3]]) <- state_names
    names(results[[4]]) <- state_names
    names(results[[5]]) <- state_names
    # browser() # uncomment for testing

    results
}
