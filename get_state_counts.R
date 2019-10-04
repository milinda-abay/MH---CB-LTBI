# The primary function run_model() calls to perform row by row transition calculations.
get_state_counts <- function(index_ds, output_ds, year, strategy, markov_cycle, dsa, a_run=NULL) {

    # collapsing the promise object 
    testing <- lazy_eval(strategy$properties$test)
    treatment <- lazy_eval(strategy$properties$treatment)

    # Now access the HDF5 dataset
    DT <- setDT(index_ds[markov_cycle + 1, 1,])

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

    
    this_env <- environment()

    if (dsa == TRUE) {

        # replace parameters  by values return form defined_dsa
        dsa_param <- dimnames(strategy$dsa$dsa)[[2]]
        dsa_value <- strategy$dsa$dsa[a_run,]

        lapply(dsa_param, function(p) {
                        
            assign(p, dsa_value[[p]], this_env)
            
        })
    }

    # write parameters to output_ds
    # TODO - replace hard coded parameter(s) and values
    
    if (markov_cycle == 0) {

        pn <- names(param)
        h5attr(output_ds, 'dsa') <- dsa
        lapply(pn, function(pn) {

            
            h5attr(output_ds, pn) <- eval(as.symbol(pn), this_env)
        })

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
    # results <- perform_matrix_multiplication(output_ds, tM, l, z, markov_cycle, flow_cost, state_cost, utility, current_rows)
    perform_matrix_multiplication(output_ds, tM, l, z, markov_cycle, flow_cost, state_cost, utility)
    print("PMM End")
    print(Sys.time())

    NULL

}
