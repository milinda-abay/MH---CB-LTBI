# Initialise HDF5
initialise_hfd5 <- function(strategy.grp,strategy_p1, strategy_p2, my_name,strategy) {

    # Create the scenario groups in HFD5
    scenario.grp <- strategy.grp$create_group(paste(strategy_p1, strategy_p2, sep = "_"))

    
    # Creating a 3D array for the index data set
    # TODO - change all hard coded values to refer winter_input_dt for attributes
    compound_index <- H5T_COMPOUND$new(c('AGEP', 'ISO3', 'YARP', 'NUMP', 'LTBP', 'AGERP', 'SEXP'),
                                   dtypes = list(h5types$H5T_NATIVE_INT, H5T_STRING$new(size = 3), h5types$H5T_NATIVE_INT,
                                   h5types$H5T_IEEE_F32LE, h5types$H5T_IEEE_F32LE, h5types$H5T_NATIVE_INT, H5T_STRING$new(size = 3)))

    # Above should be implemented like this.
    compound_output <- H5T_COMPOUND$new(names(strategy$states),
                                   dtypes = rep(list(eval(Quote(h5types$H5T_IEEE_F32LE))), strategy$state_number))
 


    index_space <- H5S$new(dim = c(cycles + 1, 1, nrow(init)), maxdims = c(cycles + 1, 1, Inf))

    output_space <- H5S$new(dim = c(cycles + 1, 5, nrow(init)), maxdims = c(cycles + 1, 5, Inf))
       

    scenario.grp$create_dataset(name = 'index', space = index_space,
                                                        dtype = compound_index)

    scenario.grp$create_dataset(name = 'output', space = output_space, dtype = compound_output)

    index_ds <- scenario.grp[['index']]
    x <- copy(init[, .(AGEP, ISO3, YARP, NUMP, LTBP, AGERP, SEXP)])

    index_ds[1,1, ] <- x

    output_ds <- scenario.grp[['output']]

    output_ds[1, 1,] <- init[, ..state_names]
    
    scenario.grp

}