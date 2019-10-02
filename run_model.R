
# The main model runtime loop 
run_model <- function(...,
                     parameters = NULL,
                     init = NULL,
                     cycles = NULL,
                     scenario_env = NULL,
                     method = NULL,
                     cost = NULL,
                     effect = NULL,
                     inflow = TRUE) {


    uneval_strategy_list <- list(...)
    strategy <- uneval_strategy_list[[1]]

    # copies the scenario environment in order to evaluate test & treatment
    strategy$properties$test$env <- scenario_env
    strategy$properties$treatment$env <- scenario_env

    eval_strategy <- strategy

    year <- start_year # Initialise year with start_year
    markov_cycle <- 0 # Tracks the current cycle


    # HDF5
    if (!(lazy_eval(eval_strategy$properties$my_name) %in% names(winter_h5))) {
        strategy.grp <- winter_h5$create_group(lazy_eval(eval_strategy$properties$my_name))

    } else {

        strategy.grp <- winter_h5[[lazy_eval(eval_strategy$properties$my_name)]]

    }

    # The names test and treatment are specific.
    # TODO -  Find a generic method to extract the name value
    strategy_p1 <- lazy_eval(eval_strategy$properties$test)
    strategy_p2 <- lazy_eval(eval_strategy$properties$treatment)

    # A conditional flag used for inflows.
    my_name <- lazy_eval(strategy$properties$my_name)


    scenario.grp <- initialise_hfd5(strategy.grp,strategy_p1, strategy_p2, my_name,strategy)
               
    # Create names for datasets
    index_ds <- scenario.grp[["index"]]
    output_ds <- scenario.grp[['output']]

    # TODO - replace hard coded parameter(s) and values
    h5attr(scenario.grp[["index"]], "testsn") <- 0.5


    while (markov_cycle < cycles) {

        #current_rows <- which(index_ds[]$cycle == markov_cycle)
        current_rows <- nrow(index_ds[markov_cycle+1,1,])

        writeLines(sprintf("\nCommencing Markov cycle %i", markov_cycle))
        writeLines(sprintf("Current number of populations in the working matrix is %i", current_rows))
        print(setDT(index_ds[markov_cycle+1,1,])[1:10, .N, by = .(AGEP)])


        results <- get_state_counts(index_ds, output_ds, year, strategy, markov_cycle)

        # Update counters
        markov_cycle <- markov_cycle + 1
        year <- year + 1

        # Aging the population in the calculation object
        index_ds[markov_cycle+1, 1,] <- index_ds[markov_cycle,1,]
        index_ds[markov_cycle+1, 1,][, 1] <- index_ds[markov_cycle, 1,][, 1] + 1


        size <- nrow(index_ds[markov_cycle + 1,,])

        output_ds[markov_cycle + 1, 1, 1:size] <- results[[1]]
        output_ds[markov_cycle + 1, 2, 1:size] <- results[[2]]
        output_ds[markov_cycle + 1, 3, 1:size] <- results[[3]]
        output_ds[markov_cycle + 1, 4, 1:size] <- results[[4]]
        output_ds[markov_cycle + 1, 5, 1:size] <- results[[5]]

        # Inflows for next cycle. 

        if ((my_name == "S1" || my_name == "S0_1"
            || my_name == "S2" || my_name == "S0_12") && markov_cycle > 5) {

            inflow <- FALSE

        }

        if (inflow) {
            pop_inflow <- winter_input_dt[YARP == year,][, cycle := NA]
        }
        else {
            pop_inflow <- NULL # not convinced this is needed
        }

        
        if (inflow) {

            # index test
            start <- nrow(index_ds[markov_cycle+1,1,]) + 1
            end <- start + nrow(pop_inflow) - 1
            x <- pop_inflow[, !..state_names][,1:7]
            index_ds[markov_cycle + 1, 1, start:end] <- x

            # update state count in output_ds with new arrivals
            output_ds[markov_cycle + 1, 1, start:end] <- pop_inflow[, ..state_names]
           
        }

       
    }

    
}




list_values <- as.list(c("CMP", "pop * (1 - testsp) * treatr", "pop * (1 - testsp )* (1 - treatr)", "0", "pop * testsp", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "mr",
"0", "0", "0", "CMP", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "mr+treatsae",
"0", "0", "CMP", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "mr",
"0", "0", "0", "CMP", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "mr",
"0", "0", "0", "0", "CMP", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "mr",
"0", "0", "0", "0", "0", "CMP", "pop * testsn * treatr", "0", "pop * testsn * (1 - treatr)", "0", "0", "pop * (1 - testsn)", "0", "0", "rr", "0", "0", "0", "0", "mr",
"0", "0", "0", "0", "0", "0", "0", "CMP", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "mr+treatsae",
"0", "0", "0", "0", "0", "0", "0", "CMP", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "mr",
"0", "0", "0", "0", "0", "0", "0", "0", "CMP", "rr", "0", "0", "0", "0", "0", "0", "0", "0", "0", "mr",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "CMP", "0", "0", "0", "0", "0", "tbmr", "0", "0", "mr",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "CMP", "0", "0", "0", "0", "0", "0", "0", "0", "mr",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "CMP", "rr", "0", "0", "0", "0", "0", "0", "mr",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "CMP", "0", "0", "0", "tbmr", "0", "mr",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "CMP", "0", "0", "0", "0", "0", "mr",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "CMP", "0", "0", "tbmr", "mr",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "CMP", "0", "0", "0", "mr",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1"


))
