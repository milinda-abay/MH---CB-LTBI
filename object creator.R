# Contains the Model object creation functions

create_objects <- function(state_names) {

    define_transition <- function(...) {
        # Define an unevaluated transmission matrix, for use in model running later

        # Extract transition matrix from first arguments, of any number, and assign names
        unevaluated_transition_matrix <- lazyeval::lazy_dots(...)
        n.expected.states <- sqrt(length(unevaluated_transition_matrix))
        n.states <- length(state_names)
        names(unevaluated_transition_matrix) <-
        sprintf("cell_%i_%i",
              rep(seq_len(n.expected.states), each = n.expected.states),
              rep(seq_len(n.expected.states), n.expected.states))

        # Perform checks
        if (n.states != n.expected.states) {
            stop("Transition matrix is not the square of the number of states")
        }

        check_complement(unevaluated_transition_matrix, n.expected.states)

        # Define attributes of the unevaluated transmission matrix
        structure(unevaluated_transition_matrix,
              class = c("uneval_matrix", class(unevaluated_transition_matrix)),
              state_names = as.vector(state_names))
    }

    define_parameters <- function(...) {
        # Define an unevaluated parameters list, for use in model running later

        unevaluated_parameter_list <- lazyeval::lazy_dots(...)
        structure(unevaluated_parameter_list, class = c("uneval_parameters", class(unevaluated_parameter_list)))
    }

    define_state <- function(...) {
        # Defines state values

        state_values <- lazyeval::lazy_dots(...)
        structure(state_values, class = c("state", class(state_values)))

    }

    define_strategy <- function(..., states, transition_matrix, dsa = NULL) {
        # TODO - find a way to pass the state_names vector and create the state objects
        # using create_states and define_state.
        # Have a list of named states per state_names

        strategy_properties <- lazyeval::lazy_dots(...)

        structure(list(transition = transition_matrix, states = states, properties = strategy_properties,
              state_number = length(states), dsa = dsa),
              class = c("uneval_model", class(dsa)))

    }

    define_inflow <- function() {

        #TODO: Implement inflow logic
        print(start_year)
    }

    define_initialisation <- function(strategy, start_year) {

        switch(lazy_eval(strategy$properties$my_name),
               "S2" = winter_input_dt[YARP == start_year][, cycle := 0],
               "S3" = winter_input_dt[YARP < start_year][, cycle := 0]
    )
    }


    define_dsa <- function(..., sample = 1) {
        # input should be of the form
        # param, lower limit, upper limit

        uneval_paramspace <- lazy_dots(...)

        if (!length(uneval_paramspace) %% 3 == 0) {
            stop("Incorrect number of elements")
        }

        number_of_parameters <- nrow(length(uneval_paramspace))

        param_name <- character()
        low <- lazyeval::lazy_dots()
        high <- lazyeval::lazy_dots()

        for (i in seq_along(uneval_paramspace)) {

            if (i %% 3 == 1) {
                param_name <- c(param_name, deparse(uneval_paramspace[[i]]$expr))
            } else if (i %% 3 == 2) {
                low <- c(low, list(uneval_paramspace[[i]]))
            } else if (i %% 3 == 0) {
                high <- c(high, list(uneval_paramspace[[i]]))
            }
        }

        names(low) <- param_name
        names(high) <- param_name

        # hard-coding 4 samples of two parameters
        param_sample <- randomLHS(sample, length(param_name))
        dimnames(param_sample) <- list(NULL, param_name)

        lapply(param_name, function(param_name) {

            param_sample[, param_name] <<- qunif(param_sample[, param_name],
                                        lazy_eval(low[[param_name]]), lazy_eval(high[[param_name]]))
        })


        structure(list(
                  dsa = param_sample,
                  parameters = param_name,
                  low = low,
                  high = high
                  ),
        class = "dsa"
        )

    }

    # Creates a default set of states and values
    create_states <- function(state_names, ...) {

        state_list <- list()

        for (i in state_names) {

            state_list[i] <- list(define_state(...))

        }

        structure(state_list, class = c(class(state_list), class(state_names)))

    }

    create_argument_list <- function(input_list = NULL) {

        state_number <- length(state_names)

        # If no values are give then create place holders
        if (is.null(input_list)) {

            input_list <- rep(list("NA"), state_number)

        }

        # Create and initialise a list
        list_values <- unlist(lapply(input_list, function(x) { parse(text = x) }))

        visualise_arglist <- function(list_values) {

            dim(list_values) <- c(state_number, state_number)
            dimnames(list_values) <- list(state_names, state_names)
            data.table(t(list_values), keep.rownames = TRUE)

        }

        # Return a list of functions attached to the calling object.
        list(

        list_values = function() list_values,

        update_list = function(newlist_values) {

            if (length(list_values) == length(newlist_values)) {

                list_values <<- unlist(lapply(newlist_values, function(x) { parse(text = x) }))
                edit(visualise_arglist(list_values))

            } else {

                stop("lengths don't match")
            }

        },

        edit_list = function() {

            templist <- edit(visualise_arglist(list_values))
            setDT(templist)

            rn <- templist[, rn]
            templist[, rn := NULL]

            templist <- lapply(templist, function(x) { parse(text = x) })
            templist <- unlist(templist)
            dim(templist) <- c(state_number, state_number)
            list_values <<- t(templist)

        },

        save_list = function(list_name) {
            # TODO: need to figure out how to reference the calling object
            # or pass the object name as a parameter 

            saveRDS(list_values, paste("Data/", list_name, ".rds", sep = ""))
        },

        load_list = function(list_name) {

            list_values <<- readRDS(paste("Data/", list_name, ".rds", sep = ""))

        }
        )

    }

    check_complement <- function(transition_matrix, dimension) {
        # Used by define_transition to verify only one CMP (complement) parameter per row in the transition matrix

        # Interested in expression only, so disregard the environment (which is the second element)
        cmp_positions <- sapply(transition_matrix, function(x) x[1], simplify = TRUE)

        # Find the positions that are CMPs, converting to a logical vector
        cmp_positions <- cmp_positions == quote(CMP)

        # Reshape from list to array
        dim(cmp_positions) <- c(dimension, dimension)

        # Sum by columns because cmp_positions because is filled column-wise and so is transposed
        if (any(colSums(cmp_positions) > 1)) {
            stop("Only a maximum of one 'CMP' is allowed per matrix row.")
        }
    }

    list(

    create_argument_list = function(...) {
        create_argument_list(...)
    },

    define_transition = function(...) {
        define_transition(...)
    },

    define_parameters = function(...) {
        define_parameters(...)
    },

    define_state = function(...) {
        define_state(...)

    },

    define_strategy = function(...) {
        define_strategy(...)

    },

    define_inflow = function(...) {
        define_inflow(...)

    },

    define_initialisation = function(...) {
        define_initialisation(...)
    },

    define_outputs = function(...) {
        define_outputs(...)
    },

    define_dsa = function(...) {
        define_dsa(...)
    },

    # Creates a default set of states and values
    create_states = function(...) {
        create_states(...)
    }

    )


}


create_model <- function(strategy = NULL, tmatrix = NULL, x = state_names) {

    initialise <- create_objects(state_names)


    states <- initialise$create_states(state_names, a = x, b = y, c = z)

    arglist <- initialise$create_argument_list(list_values)

    transition_matrix <- do.call(initialise$define_transition, arglist$list_values())

    strategy <- initialise$define_strategy(test = test, treatment = treatment, transition_matrix = transition_matrix)


    list(

        Showstate = function() states,
        states = states,
        transition_matrix = transition_matrix,
        strategy = strategy,
        initialise = initialise
        )

}






define_dsa <- function(..., sample = 1) {
    # input should be of the form
    # param, lower limit, upper limit

    uneval_paramspace <- lazy_dots(...)

    if (!length(uneval_paramspace) %% 3 == 0) {
        stop("Incorrect number of elements")
    }

    number_of_parameters <- nrow(length(uneval_paramspace))

    param_name <- character()
    low <- lazyeval::lazy_dots()
    high <- lazyeval::lazy_dots()

    for (i in seq_along(uneval_paramspace)) {

        if (i %% 3 == 1) {
            param_name <- c(param_name, deparse(uneval_paramspace[[i]]$expr))
        } else if (i %% 3 == 2) {
            low <- c(low, list(uneval_paramspace[[i]]))
        } else if (i %% 3 == 0) {
            high <- c(high, list(uneval_paramspace[[i]]))
        }
    }

    names(low) <- param_name
    names(high) <- param_name

    # hard-coding 4 samples of two parameters
    param_sample <- randomLHS(sample, length(param_name))
    dimnames(param_sample) <- list(NULL, param_name)

    lapply(param_name, function(param_name) {

        param_sample[, param_name] <<- qunif(param_sample[, param_name],
                                        lazy_eval(low[[param_name]]), lazy_eval(high[[param_name]]))
    })


    structure(
              list(
                   dsa = param_sample,
                   parameters = param_name,
                   low = low,
                   high = high
                   )
                   )


}






#?set.seed
#set.seed(1)

#param_sample <- randomLHS(4, 2)

#param_sample <- data.table(param_sample)

#plot(param_sample)

#testsn
#treatr
#set.seed(1111)
## a design with 5 samples from 4 parameters
#randomLHS(5, 4)
