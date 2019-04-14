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
new.state.names <- c(state.names, paste("V.", state.names, sep = ""),
                     paste("SC.", state.names, sep = ""),
                     paste("FC.", state.names, sep = ""))


# Create a sample data table of test sensitivity & specificity
tests.dt <- data.table(tests = c("QTFGIT", "TST5", "TST10", "TST15"), SN = c(0.76, 0.74, 0.72, 0.4),
                       SP = c(0.97, 0.56, 0.58, 0.87), cost.primary = c(79.75, 46.10, 46.10, 46.10),
                       cost.tertiary = c(122.71, 164.5, 164.5, 164.5))


# Create a sample treatment data table
treatment.dt <- data.table(treatment = c("4R", "9H", "3HP", "6H"),
                           rate = c(.83, .78, .82, .63),
                           cost.primary = c(361.93, 323.62, 440.34, 309.06),
                           cost.tertiary = c(479.08, 479.82, 596.54, 543.36)) 
                            
# Create a sample utility data table
# TODO: fix hard coded data table. It should take state.names and create the columns.
utility.dt <- data.table(treatment = c("", "4R", "9H", "3HP", "6H"))
utility.dt[, c(state.names) := as.numeric(NA)]

utility.dt[treatment == "6H", c(state.names) := .(1, 0.99949, 1, 1, 1, 1, 0.99949,
                                1, 0.75, 0.94, 1, 0.75, 0.94, 1, 0.75, 0.94, 0.75,
                                0.94, 0, 0, 0, 0, 0)]

utility.dt[treatment == "9H", c(state.names) := .(1, 0.9993625, 1, 1, 1, 1, 0.9993625,
                                1, 0.75, 0.94, 1, 0.75, 0.94, 1, 0.75, 0.94, 0.75,
                                0.94, 0, 0, 0, 0, 0)]

utility.dt[treatment == "4R", c(state.names) := .(1, 0.9997705, 1, 1, 1, 1, 0.9997705,
                                1, 0.75, 0.94, 1, 0.75, 0.94, 1, 0.75, 0.94, 0.75,
                                0.94, 0, 0, 0, 0, 0)]

utility.dt[treatment == "3HP", c(state.names) := .(1, 0.999592, 1, 1, 1, 1, 0.999592,
                                1, 0.75, 0.94, 1, 0.75, 0.94, 1, 0.75, 0.94, 0.75,
                                0.94, 0, 0, 0, 0, 0)]

utility.dt[treatment == "", c(state.names) := .(1, NA, NA, NA, NA, 1, NA, NA, NA, NA,
                 NA, NA, NA, NA, NA, NA, 0.75, 0.94, NA, NA, NA, 0, 0)]



unevaluated.flow.cost <- lazy(c(0, param$TESTC, param$TESTC, 0, param$TESTC,
                 0, param$TESTC, 0, 0, 0,
                 param$TESTC, 0, 0, param$TESTC, 0,
                 0, 0, 0, 0, 0,
                 0, 0, 0))


unevaluated.state.cost <- lazy(c(0, param$TREATC, 0, 0, 0,
                 0, param$TREATC, 0, param$TBCOST, 0,
                 0, param$TBCOST, 0, 0, param$TBCOST,
                 0, param$TBCOST, 0, 0, 0,
                 0, 0, 0))




#Sample commands demonstrating the functional argument list.


arglist <- CreateArgumentList(state.names, state.number)

# updates a row. Note: unevaluated parameter must be wrapped in a quote()
# arglist$update.row(9, c(0, 0, 0, 0, 0, 0, 0, 0, 0, quote(CMP), 0, 0, 0, 0, 0, 0, 0, 0, quote(param$TBMR), 0, 0, 0, quote(param$MR)))
# arglist$update.list(listvalues) # For passing a entire list
# arglist$update.cell(1, 1, 0) # update on cell


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

arglist.S1.TM <- arglist$load.list("S1.TM")
#arglist.S1.TM.QTFGIT.4R <- arglist$load.list("S1.TM.QTFGIT.4R")
#arglist.S1.TM.TST10.4R <- arglist$load.list("S1.TM.TST10.4R")
#arglist.S1.TM.TST15.4R <- arglist$load.list("S1.TM.TST15.4R")
arglist.S2.TM <- arglist$load.list("S2.TM")
arglist.BASELINE.TM <- arglist$load.list("BASELINE.TM")


CreateStates(state.names) # --- not used --- instantiates a set of states objects with default vaules


# Create a set of strategies
S1 <- DefineStrategy(p.sus, p.sus.fp.t, p.sus.fp.nt, p.sus.fp.tc, p.sus.tn,
                 p.ltbi, p.ltbi.tp.t, p.ltbi.tp.tc, p.ltbi.tp.tc.tb, p.ltbi.tp.tc.tbr,
                 p.ltbi.tp.nt, p.ltbi.tp.nt.tb, p.ltbi.tp.nt.tbr, p.ltbi.fn, p.ltbi.fn.tb,
                 p.ltbi.fn.tbr, p.ltbi.tb, p.ltbi.tbr, p.ltbi.tp.tc.tb.death, p.ltbi.tp.nt.tb.death,
                 p.ltbi.fn.tb.death, p.ltbi.tb.death, p.death,
                 transition.matrix = do.call(DefineTransition, arglist.S1.TM))



S2 <- DefineStrategy(p.sus, p.sus.fp.t, p.sus.fp.nt, p.sus.fp.tc, p.sus.tn,
                 p.ltbi, p.ltbi.tp.t, p.ltbi.tp.tc, p.ltbi.tp.tc.tb, p.ltbi.tp.tc.tbr,
                 p.ltbi.tp.nt, p.ltbi.tp.nt.tb, p.ltbi.tp.nt.tbr, p.ltbi.fn, p.ltbi.fn.tb,
                 p.ltbi.fn.tbr, p.ltbi.tb, p.ltbi.tbr, p.ltbi.tp.tc.tb.death, p.ltbi.tp.nt.tb.death,
                 p.ltbi.fn.tb.death, p.ltbi.tb.death, p.death,
                 transition.matrix = do.call(DefineTransition, arglist.S2.TM))

# The same transition matrix is used for scenario 3(5%) , 4(10%). The object name triggers the Get.POP function
# to return the testing percentage.

S4 <- S2 
S4 <- S2

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
                               TESTC = Get.TEST(S = "cost.primary", testing),
                               TREATR = Get.TREAT(S = "rate", treatment),
                               TREATC = Get.TREAT(S = "cost.primary", treatment),
                               POP = Get.POP(DT, strategy),
                               UTILITY = Get.UTILITY(treatment),
                               TBCOST = 11408.84
                               )




# Uses aust.LGA.rds file to create a sample input
pop.master <- CreatePopulationMaster()

# Run only for Strategy #1 population master
discount <- 0.03
start.year <- 2019
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
pop.master <- CreatePopulationMaster(Modify = TRUE)

#n_cohorts_to_evaluate <- nrow(pop.master) # Can be adjusted to save running time if you don't want to evaluate the entire population
#n_cohorts_to_evaluate <- 10

# Creates and initialises the population output table for the model (markov.cycle = 0)
pop.output <- pop.master[YARP == year][, cycle := 0] #[1: n_cohorts_to_evaluate] 

# Toggle to reduce number of cohorts to evaluate to speed running time
# cohorts_to_track <- nrow(pop.output)
# cohorts_to_track <- 1e2

# TODO - If start.year != 2016 then recalculate AGEP at start.year!

pop.output <- RunModel(pop.output, strategy = S1, testing = "QTFGIT", treatment = "4R", start.year = 2019, cycles = 11)
# pop.output <- RunModel(pop.output[1: cohorts_to_track])

# Saves output, chage file name as required
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/S1.QTFGIT.4R.rds")


# Repeat for all remaining
pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST5", treatment = "4R", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/S1.TST5.4R.rds")


pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST10", treatment = "4R", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/S1.TST10.4R.rds")


pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST15", treatment = "4R", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/S1.TST15.4R.rds")

#--------------------- S1 3HP ---------------------------#

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "QTFGIT", treatment = "3HP", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/S1.TST5.3HP.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST5", treatment = "3HP", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/S1.TST5.3HP.rds")


pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST10", treatment = "3HP", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/S1.TST10.3HP.rds")


pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST15", treatment = "3HP", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/S1.TST15.3HP.rds")


# TODO include code for 6H and 9H

#--------------------- END OF S1 ---------------------------#




#--- Model parameters for S2 --------------------------# 
discount <- 0.03
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles

#--------------------- S2 4R ---------------------------#

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "QTFGIT", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S2.QTFGIT.4R.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST5", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S2.TST5.4R.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST10", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S2.TST10.4R.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST15", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S2.TST15.4R.rds")

#--------------------- S2 3HP ---------------------------#

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "QTFGIT", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S2.QTFGIT.3HP.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST5", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S2.TST5.3HP.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST10", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S2.TST10.3HP.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST15", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S2.TST15.3HP.rds")


# TODO include code for 6H and 9H

#--------------------- END OF S2 ---------------------------#




#---------- Model parameters for STRATEGY 3 ----------------#
discount <- 0.03
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles


#--------------------- S3 4R ---------------------------#

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "QTFGIT", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S3.QTFGIT.4R.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST5", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S3.TST5.4R.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST10", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S3.TST10.4R.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST15", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S3.TST15.4R.rds")

#--------------------- S3 3HP ---------------------------#

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "QTFGIT", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S3.QTFGIT.3HP.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST5", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S3.TST5.3HP.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST10", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S3.TST10.3HP.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST15", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S3.TST15.3HP.rds")


# TODO include code for 6H and 9H

#--------------------- END OF S4 ---------------------------#



#---------- Model parameters for STRATEGY 4 ----------------#
discount <- 0.03
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles


#--------------------- S4 4R ---------------------------#

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "QTFGIT", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S4.QTFGIT.4R.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST5", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S4.TST5.4R.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST10", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S4.TST10.4R.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST15", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S4.TST15.4R.rds")

#--------------------- S4 3HP ---------------------------#

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "QTFGIT", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S4.QTFGIT.3HP.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST5", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S4.TST5.3HP.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST10", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S4.TST10.3HP.rds")

pop.master <- CreatePopulationMaster()
pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST15", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/S4.TST15.3HP.rds")


# TODO include code for 6H and 9H

#--------------------- END OF S4 ---------------------------#








# Finally Baseline


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





