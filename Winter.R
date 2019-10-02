
library(lazyeval) # required
library(data.table) # required
library(hdf5r)
library(lhs)

state_names <- c("p.sus", "p.sus.fp.t", "p.sus.fp.nt", "p.sus.fp.tc", "p.sus.tn",
                 "p.ltbi", "p.ltbi.tp.t", "p.ltbi.tp.tc", "p.ltbi.tp.nt", "p.ltbi.tp.nt.tb",
                 "p.ltbi.tp.nt.tbr", "p.ltbi.fn", "p.ltbi.fn.tb", "p.ltbi.fn.tbr", "p.ltbi.tb",
                 "p.ltbi.tbr", "p.ltbi.tp.nt.tb.death", "p.ltbi.fn.tb.death", "p.ltbi.tb.death", "p.death")

# Dummy values
test <- c("TST15")
treatment <- c("4R")
start_year <- 2020
cycles <- 10

# create HDF5
winter_h5 <- H5File$new("Winter.h5", mode = "w")

# source files
source("calculate_complement.R")
source("perform_matrix_multiplication.R")
source("get_state_counts.R")
source("run_model.R")
source("parameters.R")
source("object creator.R")
source("initialise_hfd5.R")

load_data_files()

winter_input_dt <- create_population_master()
object_creator <- create_objects(state_names)

#Create a list of values for the transition matrix
s2_arglist <- object_creator$create_argument_list(list_values)

#Create the transition matrix
s2_matrix <- do.call(object_creator$define_transition, s2_arglist$list_values())

# Create a list of model states
s2_state_list <- object_creator$create_states(state_names, a=x, b=y, c=z)

# Creates an unevaluated set of parameters
parameters <- object_creator$define_parameters(mr = get_mr(DT, year, rate_assumption = "Med"),
                                              rr = get_rr(DT, year),
                                              tbmr = get_tbmr(DT, year),
                                              testsn = get_test(S = "SN", testing),
                                              testsp = get_test(S = "SP", testing),
                                              testc = get_test(S = "cost.primary", testing),
                                              treatr = get_treat(S = "rate", treatment),
                                              treatc = get_treat(S = "cost.primary", treatment),
                                              treatsae = get_treat(S = "sae", treatment),
                                              pop = get_pop(DT, strategy, markov_cycle),
                                              utility = get_utility(treatment),
                                              tbcost = 11408.84
                                              )


# TODO - deparse the name of the arguments using (...) and create the property label. Test, treatment or xyz shouldn't
# matter, e.g. s2_strategey$properties$...
s2_strategy <- object_creator$define_strategy(test = test,
                                              treatment = treatment,
                                              my_name = "S2",
                                              states = s2_state_list,
                                              transition_matrix = s2_matrix)

# TODO - define_initialisation is extremely bespoke. More thought must be given on how to make it
# a generic method
init <- object_creator$define_initialisation(s2_strategy, start_year)


do_scenario <- function (list_of_tests, list_of_treatments) {


    do_test <- function(test) {

        do_treatment <- function(treatment) {

            
            scenario_env <- environment()

            run_model(strategy = s2_strategy, init = init, cycles = cycles, scenario_env = scenario_env)

        }

        lapply(list_of_treatments, do_treatment)

    }

    lapply(list_of_tests, do_test)

}

do_scenario(test,treatment)


