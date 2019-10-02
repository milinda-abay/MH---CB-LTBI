# Performs matrix multiplication on each row (cohort) with the evaluated transition matrix for that row.
perform_matrix_multiplication <- function(dM, tM, l, z, markov_cycle, flow_cost, state_cost, utility, current_rows) {



    # using the HDF5 
    bar <- unlist(dM[markov_cycle+1,1,])

    dM <- data.table(seq(1:z))

    # Make the current data matrix a list
    # bar <- unlist(dM)

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
    discount <- 0.3

    if (markov_cycle == 0) {
        discount <- 1
    } else {
        discount <- (1 - discount) ^ markov_cycle
    }

    

    flow_cost <- discount * flow_cost
    state_cost <- discount * state_cost
    utility <- discount * utility

    flow_cost <- flows[, Map("*", flow_cost, .SD)]
    count.cost <- counts[, Map("*", state_cost, .SD)]

    state.QALY <- counts[, Map("*", utility, .SD)]

    return(list(counts, flows, count.cost, flow_cost, state.QALY))

}
