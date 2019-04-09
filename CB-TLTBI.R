# Coding style
# https://google.github.io/styleguide/Rguide.xml

# Change working directory as required
# setwd("M:/Documents/@Projects/MH - CB LTBI/")
options(prompt = "R> ")


# Load libraries. (not needed if using the *.rds data files objects)
#library(tidyverse)
#library(reshape2)
#library(zoo) # used for filling empty AGEP values
#library(readxl)
#library(ggplot2)
#library(rlang)
#library(diagram)
#library(heemod)

library(lazyeval) # required
library(data.table) # required


# Model setup located within this file.
# It defines all the states, transition matrices, strategies, costs and parameters.
source("CB-TLTBI_DataPreparation.R")
source("CB-TLTBI functions.R")


# This function uses the above three Fix* functions. 
# Run once to create the *.rds objects (vic.fertility, vic.mortality, vic.migration)
# based on ABS's population porjection data
# CreateRDSDataFiles()

# Read the data files (if required)

# aust <- readRDS("Data/aust.rds")
aust.LGA <- readRDS("Data/aust.LGA.rds") # this is required
# prob.Inf <- readRDS("Data/prob.Inf.rds") 
# tbhaz.200rep <- readRDS("Data/tbhaz.200rep.rds")
# tbhaz.5000rep <- readRDS("Data/tbhaz.5000rep.rds")
# vic.fertility <- readRDS("Data/vic.fertility.rds")
vic.mortality <- readRDS("Data/vic.mortality.rds") # this is also required
# vic.migration <- readRDS("Data/vic.migration.rds")
# vic.pop <- readRDS("Data/vic.pop.rds")
RRates <- readRDS("Data/RRates.rds") # this is also required
vic.tb.mortality <- readRDS("Data/vic.tb.mortality.rds") # this is also required

# Creating a vector of state names
state.names <- c("p.sus", "p.sus.fp.t", "p.sus.fp.nt", "p.sus.fp.tc", "p.sus.tn",
                 "p.ltbi", "p.ltbi.tp.t", "p.ltbi.tp.tc", "p.ltbi.tp.tc.tb", "p.ltbi.tp.tc.tbr",
                 "p.ltbi.tp.nt", "p.ltbi.tp.nt.tb", "p.ltbi.tp.nt.tbr", "p.ltbi.fn", "p.ltbi.fn.tb",
                 "p.ltbi.fn.tbr", "p.ltbi.tb", "p.ltbi.tbr", "p.ltbi.tp.tc.tb.death", "p.ltbi.tp.nt.tb.death",
                 "p.ltbi.fn.tb.death", "p.ltbi.tb.death", "p.death")

# Number of states
state.number <- length(state.names)

# a hack to manage the flows, state.cost and flow.cost values.
new.state.names <- c(state.names, paste("V.", state.names, sep = ""), paste("SC.", state.names, sep = ""), paste("FC.", state.names, sep = ""))


# Create a sample data table of test sensitivity & specificity
tests.dt <- data.table(tests = c("QTFGIT", "TST5", "TST10", "TST15"), SN = c(0.5915, 0.80, 0.84, 0.80),
    SP = c(0.93, 0.5011, 0.79, 0.87))

# Create a sample treatment data table
treatment.dt <- data.table(treatment = c("4R"), rate = c(.6818))

# Create a sample utility data table
# TODO: fix hard coded data table. It should take state.names and create the columns.
utility.dt <- data.table(p.sus = 1, p.sus.fp.t = 1, p.sus.fp.nt = 1, p.sus.fp.tc = 1, p.sus.tn = 1,
                 p.ltbi = 1, p.ltbi.tp.t = 1, p.ltbi.tp.tc = 1, p.ltbi.tp.tc.tb = 1, p.ltbi.tp.tc.tbr = 1,
                 p.ltbi.tp.nt = 1, p.ltbi.tp.nt.tb = 1, p.ltbi.tp.nt.tbr = 1, p.ltbi.fn = 1, p.ltbi.fn.tb = 1,
                 p.ltbi.fn.tbr = 1, p.ltbi.tb = 1, p.ltbi.tbr = 1, p.ltbi.tp.tc.tb.death = 1, p.ltbi.tp.nt.tb.death = 1,
                 p.ltbi.fn.tb.death = 1, p.ltbi.tb.death = 1, p.death = 0)


# Sample commands demonstrating the functional argument list. 
arglist <- CreateArgumentList(state.names, state.number)

# updates a row. Note: unevaluated parameter must be wrapped in a quote()
# arglist$update.row(9, c(0, 0, 0, 0, 0, 0, 0, 0, 0, quote(CMP), 0, 0, 0, 0, 0, 0, 0, 0, quote(param$TBMR), 0, 0, 0, quote(param$MR)))
# arglist$update.list(listvalues) # For passing a entire list
# arglist$update.cell(14, 6, 0.6 * 0.20) # update on cell

# Show list with N x N state dimensions (note: column-wise layout)
# arglist$show.list() # aperm(arglist$show.list(), c(2,1))

# Add the state names as the final argument
# arglist$add.state.name(state.names)

# Drop the state name and reset the dimension.
# arglist$drop.state.name()

# Save the argument list. 
# arglist$save.list("S1.TM")

# Load the argument list
# S1.TM.QTFGIT.4R
# S1.TM.TST10.4R
# S1.TM.TST15.4R
# S2.TM
# BASELINE.TM


arglist.S1.TM.QTFGIT.4R <- arglist$load.list("S1.TM.QTFGIT.4R")
arglist.S1.TM.TST10.4R <- arglist$load.list("S1.TM.TST10.4R")
arglist.S1.TM.TST15.4R <- arglist$load.list("S1.TM.TST15.4R")
arglist.S2.TM <- arglist$load.list("S2.TM")
arglist.BASELINE.TM <- arglist$load.list("BASELINE.TM")


CreateStates(state.names) # --- not used --- instantiates a set of states objects with default vaules


# Create a set of strategies
S1.QTFGIT.4R <- DefineStrategy(p.sus, p.sus.fp.t, p.sus.fp.nt, p.sus.fp.tc, p.sus.tn,
                 p.ltbi, p.ltbi.tp.t, p.ltbi.tp.tc, p.ltbi.tp.tc.tb, p.ltbi.tp.tc.tbr,
                 p.ltbi.tp.nt, p.ltbi.tp.nt.tb, p.ltbi.tp.nt.tbr, p.ltbi.fn, p.ltbi.fn.tb,
                 p.ltbi.fn.tbr, p.ltbi.tb, p.ltbi.tbr, p.ltbi.tp.tc.tb.death, p.ltbi.tp.nt.tb.death,
                 p.ltbi.fn.tb.death, p.ltbi.tb.death, p.death,
                 transition.matrix = do.call(DefineTransition, arglist.S1.TM.QTFGIT.4R))

S1.TST10.4R <- DefineStrategy(p.sus, p.sus.fp.t, p.sus.fp.nt, p.sus.fp.tc, p.sus.tn,
                 p.ltbi, p.ltbi.tp.t, p.ltbi.tp.tc, p.ltbi.tp.tc.tb, p.ltbi.tp.tc.tbr,
                 p.ltbi.tp.nt, p.ltbi.tp.nt.tb, p.ltbi.tp.nt.tbr, p.ltbi.fn, p.ltbi.fn.tb,
                 p.ltbi.fn.tbr, p.ltbi.tb, p.ltbi.tbr, p.ltbi.tp.tc.tb.death, p.ltbi.tp.nt.tb.death,
                 p.ltbi.fn.tb.death, p.ltbi.tb.death, p.death,
                 transition.matrix = do.call(DefineTransition, arglist.S1.TM.TST10.4R))

S1.TST15.4R <- DefineStrategy(p.sus, p.sus.fp.t, p.sus.fp.nt, p.sus.fp.tc, p.sus.tn,
                 p.ltbi, p.ltbi.tp.t, p.ltbi.tp.tc, p.ltbi.tp.tc.tb, p.ltbi.tp.tc.tbr,
                 p.ltbi.tp.nt, p.ltbi.tp.nt.tb, p.ltbi.tp.nt.tbr, p.ltbi.fn, p.ltbi.fn.tb,
                 p.ltbi.fn.tbr, p.ltbi.tb, p.ltbi.tbr, p.ltbi.tp.tc.tb.death, p.ltbi.tp.nt.tb.death,
                 p.ltbi.fn.tb.death, p.ltbi.tb.death, p.death,
                 transition.matrix = do.call(DefineTransition, arglist.S1.TM.TST15.4R))

S2 <- DefineStrategy(p.sus, p.sus.fp.t, p.sus.fp.nt, p.sus.fp.tc, p.sus.tn,
                 p.ltbi, p.ltbi.tp.t, p.ltbi.tp.tc, p.ltbi.tp.tc.tb, p.ltbi.tp.tc.tbr,
                 p.ltbi.tp.nt, p.ltbi.tp.nt.tb, p.ltbi.tp.nt.tbr, p.ltbi.fn, p.ltbi.fn.tb,
                 p.ltbi.fn.tbr, p.ltbi.tb, p.ltbi.tbr, p.ltbi.tp.tc.tb.death, p.ltbi.tp.nt.tb.death,
                 p.ltbi.fn.tb.death, p.ltbi.tb.death, p.death,
                 transition.matrix = do.call(DefineTransition, arglist.S2.TM))


BO <- DefineStrategy(p.sus, p.sus.fp.t, p.sus.fp.nt, p.sus.fp.tc, p.sus.tn,
                 p.ltbi, p.ltbi.tp.t, p.ltbi.tp.tc, p.ltbi.tp.tc.tb, p.ltbi.tp.tc.tbr,
                 p.ltbi.tp.nt, p.ltbi.tp.nt.tb, p.ltbi.tp.nt.tbr, p.ltbi.fn, p.ltbi.fn.tb,
                 p.ltbi.fn.tbr, p.ltbi.tb, p.ltbi.tbr, p.ltbi.tp.tc.tb.death, p.ltbi.tp.nt.tb.death,
                 p.ltbi.fn.tb.death, p.ltbi.tb.death, p.death,
                 transition.matrix = do.call(DefineTransition, arglist.BASELINE.TM))




# Creates an unevaluated set of parameters
parameters <- DefineParameters(MR = Get.MR(DT, year, rate.assumption = "High"),
                               RR = Get.RR(DT, year),
                               TBMR = Get.TBMR(DT, year),
                               TESTSN = Get.TEST(S = "SN", testing),
                               TESTSP = Get.TEST(S = "SP", testing),
                               TREATR = Get.TREATR(treatment),
                               POP = Get.POP(),
                               UTILITY = Get.UTILITY(),
                               DISCOUNT = Get.DISCOUNT(),
                               COST = Get.Cost(testing, treatment)
                               )


# Model parameters
discount <- 0.0
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles

# Placeholder vectors for state costs and flow costs
# TODO - Milinda: implement this using data.tables.
state.cost <- c(p.sus = 0, p.sus.fp.t = 490, p.sus.fp.nt = 0, p.sus.fp.tc = 0, p.sus.tn = 0,
                 p.ltbi = 0, p.ltbi.tp.t = 490, p.ltbi.tp.tc = 0, p.ltbi.tp.tc.tb = 9415, p.ltbi.tp.tc.tbr = 0,
                 p.ltbi.tp.nt = 0, p.ltbi.tp.nt.tb = 9415, p.ltbi.tp.nt.tbr = 0, p.ltbi.fn = 0, p.ltbi.fn.tb = 9415,
                 p.ltbi.fn.tbr = 0, p.ltbi.tb = 9415, p.ltbi.tbr = 0, p.ltbi.tp.tc.tb.death = 0, p.ltbi.tp.nt.tb.death = 0,
                 p.ltbi.fn.tb.death = 0, p.ltbi.tb.death = 0, p.death = 0)
flow.cost <- c(rep(10, 23))


# Uses aust.LGA.rds file to create a sample input
pop.master <- CreatePopulationMaster()

# Run only for Strategy #1 population master
arglist$load.list("S1.TM.QTFGIT.4R")
pop.master <- ModifyPop(pop.master, arglist)



#n_cohorts_to_evaluate <- nrow(pop.master) # Can be adjusted to save running time if you don't want to evaluate the entire population
#n_cohorts_to_evaluate <- 10

# Creates and initialises the population output table for the model (markov.cycle = 0)
pop.output <- pop.master[YARP == year][, cycle := 0] #[1: n_cohorts_to_evaluate] 

# Toggle to reduce number of cohorts to evaluate to speed running time
# cohorts_to_track <- nrow(pop.output)
# cohorts_to_track <- 1e2

# TODO - If start.year != 2016 then recalculate AGEP at start.year!

pop.output <- RunModel(pop.output, strategy = S1.QTFGIT.4R, testing = "QTFGIT", treatment = "4R", start.year = 2020, cycles = 10)
# pop.output <- RunModel(pop.output[1: cohorts_to_track])

# Saves output, chage file name as required
saveRDS(pop.output, "Data/S1.QTFGIT.4R.rds")


# Repeat for all remaining 
arglist$load.list("S1.TM.TST10.4R")
pop.master <- CreatePopulationMaster()
pop.master <- ModifyPop(pop.master, arglist)
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1.TST10.4R, testing = "TST10", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S1.TST10.4R.rds")


arglist$load.list("S1.TM.TST15.4R")
pop.master <- CreatePopulationMaster()
pop.master <- ModifyPop(pop.master, arglist)
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1.TST15.4R, testing = "TST15", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S1.TST15.4R.rds")


pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "QTFGIT", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S2.QTFGIT.4R.rds")


pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST10", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S2.TST10.4R.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST15", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S2.TST15.4R.rds")

# Finally Baseline
state.cost <- c(p.sus = 0, p.sus.fp.t = 0, p.sus.fp.nt = 0, p.sus.fp.tc = 0, p.sus.tn = 0,
                 p.ltbi = 0, p.ltbi.tp.t = 0, p.ltbi.tp.tc = 0, p.ltbi.tp.tc.tb = 0, p.ltbi.tp.tc.tbr = 0,
                 p.ltbi.tp.nt = 0, p.ltbi.tp.nt.tb = 0, p.ltbi.tp.nt.tbr = 0, p.ltbi.fn = 0, p.ltbi.fn.tb = 0,
                 p.ltbi.fn.tbr = 0, p.ltbi.tb = 9415, p.ltbi.tbr = 0, p.ltbi.tp.tc.tb.death = 0, p.ltbi.tp.nt.tb.death = 0,
                 p.ltbi.fn.tb.death = 0, p.ltbi.tb.death = 0, p.death = 0)

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = BO, testing = "", treatment = "", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/BO.rds")



# Create output files for PowerBI i.e splitting each output into four files
CreateOutput(BO, "BO", "No Test")
CreateOutput(S1.QTFGIT.4R, "S1", "QTFGIT")
CreateOutput(S1.TST10.4R, "S1", "TST10")
CreateOutput(S1.TST15.4R, "S1", "TST15")
CreateOutput(S2.QTFGIT.4R, "S2", "QTFGIT")
CreateOutput(S2.TST10.4R, "S2", "TST10")
CreateOutput(S2.TST15.4R, "S2", "TST15")


StateCount <- rbind(readRDS("Data/BO_No Test_4R_S.rds"), readRDS("Data/S1_TST10_4R_S.rds"),
                    readRDS("Data/S1_TST15_4R_S.rds"), readRDS("Data/S1_QTFGIT_4R_S.rds"),
                    readRDS("Data/S2_TST10_4R_S.rds"), readRDS("Data/S2_TST15_4R_S.rds"),
                    readRDS("Data/S2_QTFGIT_4R_S.rds"))

saveRDS(StateCount, "Data/StateCount.rds")


FlowCount <- rbind(readRDS("Data/BO_No Test_4R_F.rds"), readRDS("Data/S1_TST10_4R_F.rds"),
                    readRDS("Data/S1_TST15_4R_F.rds"), readRDS("Data/S1_QTFGIT_4R_F.rds"),
                    readRDS("Data/S2_TST10_4R_F.rds"), readRDS("Data/S2_TST15_4R_F.rds"),
                    readRDS("Data/S2_QTFGIT_4R_F.rds"))

saveRDS(FlowCount, "Data/FlowCount.rds")

StateCost <- rbind(readRDS("Data/BO_No Test_4R_SC.rds"), readRDS("Data/S1_TST10_4R_SC.rds"),
                    readRDS("Data/S1_TST15_4R_SC.rds"), readRDS("Data/S1_QTFGIT_4R_SC.rds"),
                    readRDS("Data/S2_TST10_4R_SC.rds"), readRDS("Data/S2_TST15_4R_SC.rds"),
                    readRDS("Data/S2_QTFGIT_4R_SC.rds"))

saveRDS(StateCost, "Data/StateCost.rds")


FlowCost <- rbind(readRDS("Data/BO_No Test_4R_FC.rds"), readRDS("Data/S1_TST10_4R_FC.rds"),
                    readRDS("Data/S1_TST15_4R_FC.rds"), readRDS("Data/S1_QTFGIT_4R_FC.rds"),
                    readRDS("Data/S2_TST10_4R_FC.rds"), readRDS("Data/S2_TST15_4R_FC.rds"),
                    readRDS("Data/S2_QTFGIT_4R_FC.rds"))

saveRDS(FlowCost, "Data/FlowCost.rds")





# Creates an unevaluated transition matrix
# Use 'CMP' for complement and 'param$*' for parameters.
# Each parameter must be a pairlist argument in DefineParameters().
#transMatrix4R <- DefineTransition(CMP, param$POP * (1 - param$TESTSP) * param$TREATR, param$POP * (1 - param$TESTSP) * (1 - param$TREATR), 0, param$POP * param$TESTSP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
#0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, CMP, param$POP * param$TESTSN * param$TREATR, 0, 0, 0, param$POP * param$TESTSN * (1 - param$TREATR), 0, 0, param$POP * (1 - param$TESTSN), 0, 0, param$RR, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, CMP, 0.04 * param$RR, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, param$TBMR, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, param$RR, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, param$TBMR, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, param$RR, 0, 0, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, param$TBMR, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, param$TBMR, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, state.names = state.names)

## Baseline transition matrix
#transMatrixBaseline <- DefineTransition(CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$RR, 0, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, param$TBMR, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, param$MR,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
#0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
#state.names = state.names)

