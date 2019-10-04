
initialise_hfd5 <- function(..., strategy) {

    UseMethod('initialise_hfd5', strategy)

}


# Initialise HDF5
initialise_hfd5.NULL <- function(strategy.grp,strategy_p1, strategy_p2, my_name,strategy) {

    # Create the scenario groups in HFD5

    if (!(strategy$properties$my_name$expr %in% names(winter_h5))) {
        index_grp <- winter_h5$create_group(paste(strategy$properties$my_name$expr))

    } else {

        index_grp <- winter_h5[[strategy$properties$my_name$expr]]
    }
        
    scenario_grp <- winter_h5$create_group(paste(strategy$properties$my_name$expr, paste(strategy_p1, strategy_p2, sep = "_"), sep = '/'))

    # Creating a 3D array for the index data set
    # TODO - change all hard coded values to refer winter_input_dt for attributes
    compound_index <- H5T_COMPOUND$new(c('AGEP', 'ISO3', 'YARP', 'NUMP', 'LTBP', 'AGERP', 'SEXP'),
                                   dtypes = list(h5types$H5T_NATIVE_INT, H5T_STRING$new(size = 3), h5types$H5T_NATIVE_INT,
                                   h5types$H5T_IEEE_F32LE, h5types$H5T_IEEE_F32LE, h5types$H5T_NATIVE_INT, H5T_STRING$new(size = 3)))

    index_space <- H5S$new(dim = c(cycles + 1, 1, nrow(init)), maxdims = c(cycles + 1, 1, Inf))

    # Above should be implemented like this.
    compound_output <- H5T_COMPOUND$new(names(strategy$states),
                                   dtypes = rep(list(eval(Quote(h5types$H5T_IEEE_F32LE))), strategy$state_number))
 
    #index_space <- H5S$new(dim = c(cycles + 1, 1, nrow(init)), maxdims = c(cycles + 1, 1, Inf))

    output_space <- H5S$new(dim = c(cycles + 1, 5, nrow(init)), maxdims = c(cycles + 1, 5, Inf))
       

    if ('index' %in% names(winter_h5[[strategy$properties$my_name$expr]])) {

        index_grp$link_delete('index')
        index_grp$create_dataset(name = 'index', space = index_space,
                                                        dtype = compound_index)
    } else {

        index_grp$create_dataset(name = 'index', space = index_space,
                                                        dtype = compound_index)
    }
    
    scenario_grp$create_dataset(name = 'output', space = output_space, dtype = compound_output)

    index_ds <- index_grp[['index']]
    
    x <- copy(init[, .(AGEP, ISO3, YARP, NUMP, LTBP, AGERP, SEXP)])

    index_ds[1,1, ] <- x

    output_ds <- scenario_grp[['output']]

    output_ds[1, 1,] <- init[, ..state_names]

    list(scenario_grp,index_grp)

}



# Initialise HDF5
initialise_hfd5.dsa <- function(strategy.grp, strategy_p1, strategy_p2, my_name, a_run, strategy) {

    
    dsa <- paste('dsa', a_run, sep = '_')
    group_name <- paste(strategy$properties$my_name$expr, paste(strategy_p1, strategy_p2, sep = "_"), sep = '/')


    # Create the scenario groups in HFD5

    if (!(strategy$properties$my_name$expr %in% names(winter_h5))) {
        index_grp <- winter_h5$create_group(paste(strategy$properties$my_name$expr))

    } else {

        index_grp <- winter_h5[[strategy$properties$my_name$expr]]
    }

    if (a_run == '1') {

        scenario_grp <- winter_h5$create_group(group_name)

    } else {

        scenario_grp <- winter_h5[[group_name]]

    }

    # Creating a 3D array for the index data set
    # TODO - change all hard coded values to refer winter_input_dt for attributes
    compound_index <- H5T_COMPOUND$new(c('AGEP', 'ISO3', 'YARP', 'NUMP', 'LTBP', 'AGERP', 'SEXP'),
                                   dtypes = list(h5types$H5T_NATIVE_INT, H5T_STRING$new(size = 3), h5types$H5T_NATIVE_INT,
                                   h5types$H5T_IEEE_F32LE, h5types$H5T_IEEE_F32LE, h5types$H5T_NATIVE_INT, H5T_STRING$new(size = 3)))

    index_space <- H5S$new(dim = c(cycles + 1, 1, nrow(init)), maxdims = c(cycles + 1, 1, Inf))

    # Above should be implemented like this.
    compound_output <- H5T_COMPOUND$new(names(strategy$states),
                                   dtypes = rep(list(eval(Quote(h5types$H5T_IEEE_F32LE))), strategy$state_number))

    #index_space <- H5S$new(dim = c(cycles + 1, 1, nrow(init)), maxdims = c(cycles + 1, 1, Inf))

    output_space <- H5S$new(dim = c(cycles + 1, 5, nrow(init)), maxdims = c(cycles + 1, 5, Inf))

    if ('index' %in% names(winter_h5[[strategy$properties$my_name$expr]])) {

        index_grp$link_delete('index')
        index_grp$create_dataset(name = 'index', space = index_space,
                                                        dtype = compound_index)
    } else {

        index_grp$create_dataset(name = 'index', space = index_space,
                                                        dtype = compound_index)
    }

    scenario_grp$create_dataset(name = dsa, space = output_space, dtype = compound_output)

    index_ds <- index_grp[['index']]

    x <- copy(init[, .(AGEP, ISO3, YARP, NUMP, LTBP, AGERP, SEXP)])

    index_ds[1, 1,] <- x

    output_ds <- scenario_grp[[dsa]]

    output_ds[1, 1,] <- init[, ..state_names]

    list(scenario_grp, index_grp)

}