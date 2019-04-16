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
aust.vic <- readRDS("Data/aust.vic.rds") # this is required
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
                     paste("FC.", state.names, sep = ""),
                     paste("SQ.", state.names, sep = ""))


# Create a sample data table of test sensitivity & specificity
tests.dt <- data.table(tests = c("QTFGIT", "TST5", "TST10", "TST15"), SN = c(0.76, 0.74, 0.72, 0.4),
                       SP = c(0.97, 0.56, 0.58, 0.87), cost.primary = c(79.75, 46.10, 46.10, 46.10),
                       cost.tertiary = c(122.71, 164.5, 164.5, 164.5))


# Create a sample treatment data table
treatment.dt <- data.table(treatment = c("4R", "9H", "3HP", "6H"),
                           rate = c(.83, .78, .82, .63),
                           cost.primary = c(437.13, 549.22, 440.34, 436.42),
                           cost.tertiary = c(632.38, 939.72, 596.54, 709.77))
                            
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

S3 <- S2 
S4 <- S2
S5 <- S2

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




# Uses aust.vic.rds file to create a sample input
pop.master <- CreatePopulationMaster()
#pop.master <- CreatePopulationMaster(Modify = TRUE)

#set.seed(10)
#pop.master <- pop.master[sample(.N, 500)]




#---------- Model parameters for STRATEGY 0 ----------------#

discount <- 0.03
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles

#--------------------- S0 ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = BO, testing = "", treatment = "", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S0.rds")

#--------------------- END OF S0 ---------------------------#



#-------- Model parameters for S2 --------#

discount <- 0.03
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles

#--------------------- S2 4R ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "QTFGIT", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.QTFGIT.4R.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST5", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.TST5.4R.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST10", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.TST10.4R.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST15", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.TST15.4R.rds")

#--------------------- S2 3HP ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "QTFGIT", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.QTFGIT.3HP.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST5", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.TST5.3HP.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST10", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.TST10.3HP.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST15", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.TST15.3HP.rds")

#--------------------- S2 9H ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "QTFGIT", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.QTFGIT.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST5", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.TST5.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST10", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.TST10.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST15", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.TST15.9H.rds")

#--------------------- S2 6H ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "QTFGIT", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.QTFGIT.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST5", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.TST5.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST10", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.TST10.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S2, testing = "TST15", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S2.TST15.6H.rds")

#--------------------- END OF S2 ---------------------------#



#---------- Model parameters for S3 ----------------#

discount <- 0.03
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles

#--------------------- S3 4R ---------------------------#

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "QTFGIT", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.QTFGIT.4R.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST5", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.TST5.4R.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST10", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.TST10.4R.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST15", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.TST15.4R.rds")

#--------------------- S3 3HP ---------------------------#

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "QTFGIT", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.QTFGIT.3HP.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST5", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.TST5.3HP.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST10", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.TST10.3HP.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST15", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.TST15.3HP.rds")

#--------------------- S3 9H ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "QTFGIT", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.QTFGIT.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST5", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.TST5.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST10", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.TST10.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST15", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.TST15.9H.rds")

#--------------------- S3 6H ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "QTFGIT", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.QTFGIT.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST5", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.TST5.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST10", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.TST10.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S3, testing = "TST15", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S3.TST15.6H.rds")

#--------------------- END OF S3 ---------------------------#



#---------- Model parameters for S4 ----------------#

discount <- 0.03
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles

#--------------------- S4 4R ---------------------------#

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "QTFGIT", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.QTFGIT.4R.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST5", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.TST5.4R.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST10", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.TST10.4R.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST15", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.TST15.4R.rds")

#--------------------- S4 3HP ---------------------------#

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "QTFGIT", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.QTFGIT.3HP.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST5", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.TST5.3HP.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST10", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.TST10.3HP.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST15", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.TST15.3HP.rds")

#--------------------- S4 9H ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "QTFGIT", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.QTFGIT.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST5", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.TST5.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST10", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.TST10.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST15", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.TST15.9H.rds")

#--------------------- S4 6H ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "QTFGIT", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.QTFGIT.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST5", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.TST5.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST10", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.TST10.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S4, testing = "TST15", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S4.TST15.6H.rds")

#--------------------- END OF S4 ---------------------------#



#---------- Model parameters for S5 ----------------#

discount <- 0.03
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles

#--------------------- S5 4R ---------------------------#

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "QTFGIT", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.QTFGIT.4R.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "TST5", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.TST5.4R.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "TST10", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.TST10.4R.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "TST15", treatment = "4R", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.TST15.4R.rds")

#--------------------- S5 3HP ---------------------------#

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "QTFGIT", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.QTFGIT.3HP.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "TST5", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.TST5.3HP.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "TST10", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.TST10.3HP.rds")

pop.output <- pop.master[YARP <= year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "TST15", treatment = "3HP", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.TST15.3HP.rds")

#--------------------- S5 9H ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "QTFGIT", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.QTFGIT.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "TST5", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.TST5.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "TST10", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.TST10.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "TST15", treatment = "9H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.TST15.9H.rds")

#--------------------- S5 6H ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "QTFGIT", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.QTFGIT.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "TST5", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.TST5.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "TST10", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.TST10.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S5, testing = "TST15", treatment = "6H", start.year = 2020, cycles = 10)
saveRDS(pop.output, "Data/Output/S5.TST15.6H.rds")

#--------------------- END OF S5 ---------------------------#


#-------- Model parameters for S1 --------#

discount <- 0.03
start.year <- 2019
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
pop.master <- CreatePopulationMaster(Modify = TRUE)

#set.seed(10)
#pop.master <- pop.master[sample(.N, 500)]
#p.copy <- copy(pop.master)


#--------------------- S1 4R ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "QTFGIT", treatment = "4R", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.QTFGIT.4R.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST5", treatment = "4R", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.TST5.4R.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST10", treatment = "4R", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.TST10.4R.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST15", treatment = "4R", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.TST15.4R.rds")

#--------------------- S1 3HP ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "QTFGIT", treatment = "3HP", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.QTFGIT.3HP.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST5", treatment = "3HP", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.TST5.3HP.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST10", treatment = "3HP", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.TST10.3HP.rds")


pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST15", treatment = "3HP", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.TST15.3HP.rds")

#--------------------- S1 9H ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "QTFGIT", treatment = "9H", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.QTFGIT.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST5", treatment = "9H", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.TST5.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST10", treatment = "9H", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.TST10.9H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST15", treatment = "9H", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.TST15.9H.rds")

#--------------------- S1 6H ---------------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "QTFGIT", treatment = "6H", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.QTFGIT.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST5", treatment = "6H", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.TST5.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST10", treatment = "6H", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.TST10.6H.rds")

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S1, testing = "TST15", treatment = "6H", start.year = 2019, cycles = 11)
pop.output <- pop.output[p.sus == 0]
pop.output[, cycle := cycle - 1]
saveRDS(pop.output, "Data/Output/S1.TST15.6H.rds")

#--------------------- END OF S1 ---------------------------#







