# Coding style
# https://google.github.io/styleguide/Rguide.xml


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
# based on ABS's population projection data
# CreateRDSDataFiles()

# Read the data files (if required)

# aust <- readRDS("Data/aust.rds")
aust.vic <- readRDS("Data/aust.vic.rds") # this is required for S1,S2,S3,S4 and S5
# aust.vic.LGA <- readRDS("Data/aust.vic.LGA.rds") # this is for S0
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
                 "p.ltbi", "p.ltbi.tp.t", "p.ltbi.tp.tc", "p.ltbi.tp.nt", "p.ltbi.tp.nt.tb",
                 "p.ltbi.tp.nt.tbr", "p.ltbi.fn", "p.ltbi.fn.tb", "p.ltbi.fn.tbr", "p.ltbi.tb",
                 "p.ltbi.tbr", "p.ltbi.tp.nt.tb.death", "p.ltbi.fn.tb.death", "p.ltbi.tb.death", "p.death")



# Number of states
state.number <- length(state.names)

# a hack to manage the flows, state.cost, flow.cost and state.qaly values.
new.state.names <- c(state.names, paste("V.", state.names, sep = ""),
                     paste("SC.", state.names, sep = ""),
                     paste("FC.", state.names, sep = ""),
                     paste("SQ.", state.names, sep = ""))


# Create a sample data table of test sensitivity & specificity
tests.dt <- data.table(tests = c("QTFGIT", "TST05", "TST10", "TST15"), SN = c(0.76, 0.74, 0.72, 0.4),
                       SP = c(0.97, 0.56, 0.58, 0.87), cost.primary = c(79.75, 67.10, 67.10, 67.10),
                       cost.tertiary = c(122.71, 164.5, 164.5, 164.5))


# Create a sample treatment data table
treatment.dt <- data.table(treatment = c("4R", "9H", "3HP", "6H"),
                           rate = c(.83, .78, .82, .63),
                           cost.primary = c(437.13, 578.87, 440.34, 436.42),
                           cost.tertiary = c(632.38, 969.37, 596.54, 709.77),
                           sae = c(0, 0.00000025, 0.00000016, 0.0000002))

#9H cost changed from 549.22 to 578.87 and 939.72 to 969.37 respectively. 



# Create a sample utility data table
# TODO: fix hard coded data table. It should take state.names and create the columns.
utility.dt <- data.table(treatment = c("", "4R", "9H", "3HP", "6H"))
utility.dt[, c(state.names) := as.numeric(NA)]


utility.dt[treatment == "6H", c(state.names) := .(1, 0.9995, 1, 1, 1, 1, 0.9995,
                                1, 1, 0.75, 0.94, 1, 0.75, 0.94, 0.75,
                                0.94, 0, 0, 0, 0)]

utility.dt[treatment == "9H", c(state.names) := .(1, 0.999375, 1, 1, 1, 1, 0.999375,
                                1, 1, 0.75, 0.94, 1, 0.75, 0.94, 0.75,
                                0.94, 0, 0, 0, 0)]

utility.dt[treatment == "4R", c(state.names) := .(1, 0.999775, 1, 1, 1, 1, 0.999775,
                                1, 1, 0.75, 0.94, 1, 0.75, 0.94, 0.75,
                                0.94, 0, 0, 0, 0)]

utility.dt[treatment == "3HP", c(state.names) := .(1, 0.9996, 1, 1, 1, 1, 0.9996,
                                1, 1, 0.75, 0.94, 1, 0.75, 0.94, 0.75,
                                0.94, 0, 0, 0, 0)]

utility.dt[treatment == "", c(state.names) := .(1, NA, NA, NA, NA, 1, NA, NA, NA,
                              NA, NA, NA, NA, NA, 0.75, 0.94, NA, NA, 0, 0)]









unevaluated.flow.cost <- lazy(c(0, param$TESTC, param$TESTC, 0, param$TESTC,
                 0, param$TESTC, 0,
                 param$TESTC, 0, 0, param$TESTC, 0,
                 0, 0, 0, 0, 0,
                 0, 0))


unevaluated.state.cost <- lazy(c(0, param$TREATC, 0, 0, 0,
                 0, param$TREATC, 0,
                 0, param$TBCOST, 0, 0, param$TBCOST,
                 0, param$TBCOST, 0, 0, 0,
                 0, 0))


unevaluated.state.utility <- lazy(c(0, param$TREATC, 0, 0, 0,
                 0, param$TREATC, 0,
                 0, param$TBCOST, 0, 0, param$TBCOST,
                 0, param$TBCOST, 0, 0, 0,
                 0, 0))

#Sample commands demonstrating the functional argument list.


arglist <- CreateArgumentList(state.names, state.number)

# updates a row. Note: unevaluated parameter must be wrapped in a quote()
# arglist$update.row(9, c(0, 0, 0, 0, 0, 0, 0, 0, 0, quote(CMP), 0, 0, 0, 0, 0, 0, 0, 0, quote(param$TBMR), 0, 0, 0, quote(param$MR)))
# arglist$update.list(listvalues) # For passing a entire list
# arglist$update.cell(7, 20, quote(param$MR+param$TREATSAE)) # update on cell


# Show list with N x N state dimensions
# arglist$show.list()[2,20]

# Add the state names as the final argument
# arglist$add.state.name(state.names)

# Drop the state name and reset the dimension.
# arglist$drop.state.name()

# Save the argument list. 
# arglist$save.list("BASELINE.S1.TM")

# Load the argument list
# S1.TM
# S2.TM
# BASELINE.TM

arglist.S1.TM <- arglist$load.list("S1.TM")
arglist.S2.TM <- arglist$load.list("S2.TM")
arglist.BASELINE.TM <- arglist$load.list("BASELINE.TM")
arglist.BASELINE.S1.TM <- arglist$load.list("BASELINE.S1.TM")

CreateStates(state.names) # instantiates a set of states objects with default values

# Create a set of strategies
S1 <- DefineStrategy(p.sus, p.sus.fp.t, p.sus.fp.nt, p.sus.fp.tc, p.sus.tn,
                     p.ltbi, p.ltbi.tp.t, p.ltbi.tp.tc, p.ltbi.tp.nt, p.ltbi.tp.nt.tb,
                     p.ltbi.tp.nt.tbr, p.ltbi.fn, p.ltbi.fn.tb, p.ltbi.fn.tbr, p.ltbi.tb,
                     p.ltbi.tbr, p.ltbi.tp.nt.tb.death, p.ltbi.fn.tb.death, p.ltbi.tb.death, p.death,
                     transition.matrix = do.call(DefineTransition, arglist.S1.TM))



S2 <- DefineStrategy(p.sus, p.sus.fp.t, p.sus.fp.nt, p.sus.fp.tc, p.sus.tn,
                     p.ltbi, p.ltbi.tp.t, p.ltbi.tp.tc, p.ltbi.tp.nt, p.ltbi.tp.nt.tb,
                     p.ltbi.tp.nt.tbr, p.ltbi.fn, p.ltbi.fn.tb, p.ltbi.fn.tbr, p.ltbi.tb,
                     p.ltbi.tbr, p.ltbi.tp.nt.tb.death, p.ltbi.fn.tb.death, p.ltbi.tb.death, p.death,
                     transition.matrix = do.call(DefineTransition, arglist.S2.TM))

# The same transition matrix is used for scenario 3(5%) , 4(10%). The object name triggers the Get.POP function
# to return the testing percentage.

S3 <- S2
S4 <- S2
S5 <- S2

S0_12 <- DefineStrategy(p.sus, p.sus.fp.t, p.sus.fp.nt, p.sus.fp.tc, p.sus.tn,
                        p.ltbi, p.ltbi.tp.t, p.ltbi.tp.tc, p.ltbi.tp.nt, p.ltbi.tp.nt.tb,
                        p.ltbi.tp.nt.tbr, p.ltbi.fn, p.ltbi.fn.tb, p.ltbi.fn.tbr, p.ltbi.tb,
                        p.ltbi.tbr, p.ltbi.tp.nt.tb.death, p.ltbi.fn.tb.death, p.ltbi.tb.death, p.death,
                        transition.matrix = do.call(DefineTransition, arglist.BASELINE.TM))

#Baselines use the same transition matrix
S0_345 <- S0_12

# New baseline for S1
S0_1 <- DefineStrategy(p.sus, p.sus.fp.t, p.sus.fp.nt, p.sus.fp.tc, p.sus.tn,
                        p.ltbi, p.ltbi.tp.t, p.ltbi.tp.tc, p.ltbi.tp.nt, p.ltbi.tp.nt.tb,
                        p.ltbi.tp.nt.tbr, p.ltbi.fn, p.ltbi.fn.tb, p.ltbi.fn.tbr, p.ltbi.tb,
                        p.ltbi.tbr, p.ltbi.tp.nt.tb.death, p.ltbi.fn.tb.death, p.ltbi.tb.death, p.death,
                        transition.matrix = do.call(DefineTransition, arglist.BASELINE.S1.TM))





# Creates an unevaluated set of parameters
parameters <- DefineParameters(MR = Get.MR(DT, year, rate.assumption = "High"),
                               RR = Get.RR(DT, year),
                               TBMR = Get.TBMR(DT, year),
                               TESTSN = Get.TEST(S = "SN", testing),
                               TESTSP = Get.TEST(S = "SP", testing),
                               TESTC = Get.TEST(S = "cost.primary", testing),
                               TREATR = Get.TREAT(S = "rate", treatment),
                               TREATC = Get.TREAT(S = "cost.primary", treatment),
                               TREATSAE = Get.TREAT(S ="sae", treatment),
                               POP = Get.POP(DT, strategy, markov.cycle),
                               UTILITY = Get.UTILITY(treatment),
                               TBCOST = 11408.84
                               )




# Uses aust.vic.rds file to create a sample input
pop.master <- CreatePopulationMaster()

#set.seed(10)
#pop.master <- pop.master[sample(.N, 1000)]




#---------- Model parameters for STRATEGY 0 ----------------#

# TODO - Need a new CreatePopulationMaster function to manage LGA column.
discount <- 0.03
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles

#--------------------- S0_12 ---------------------------#
#---------------Baseline for S2 --------------------#

pop.output <- pop.master[YARP == year][, cycle := 0]
pop.output <- RunModel(pop.output, strategy = S0_12, testing = "", treatment = "", start.year = 2020, cycles = 10, modelinflow = TRUE)
saveRDS(pop.output, "Data/Output/S0_12.rds")

#--------------------- END OF S0_12 ---------------------------#


#--------------------- S0_1 ---------------------------#
#---------------Baseline for S1 --------------------#
DoRunModel(S0_1, start.year, cycles)



#-------- Model parameters for S1 & S2 --------#

discount <- 0.03
start.year <- 2020
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles

DoRunModel(S1, start.year, cycles)
DoRunModel(S2, start.year, cycles)


#---------- Model parameters for S3 , S4 & S5----------------#

discount <- 0.03
start.year <- 2020
markov.cycle <- 0 # Tracks the current cycle
cycles <- 30 # Model run cycles

DoRunModel(S0_345, start.year, cycles)

DoRunModel(S3, start.year, cycles)
DoRunModel(S4, start.year, cycles)
DoRunModel(S5, start.year, cycles)



#------------ Manipulating output files------------- #


# output RDS files on external hard disk
setwd("D:/")



CreateOutput("S0_1")
CreateOutput("S0_12")
CreateOutput("S0_345")
CreateOutput("S1")
CreateOutput("S2")
CreateOutput("S3")
CreateOutput("S4")
CreateOutput("S5")

# Create output files for PowerBI i.e recombine each type of *.csv file into five  lookup files.

all.files <- list.files(path = "Data/Output", pattern = "S.csv")
mylist <- lapply(all.files, Readdata)
StateCount <- rbindlist(mylist, fill = TRUE)
rm(mylist)
fwrite(StateCount, "Data/Output/StateCount.csv")
rm(StateCount)

all.files <- list.files(path = "Data/Output", pattern = "SC.csv")
mylist <- lapply(all.files, Readdata)
StateCost <- rbindlist(mylist, fill = TRUE)
rm(mylist)
fwrite(StateCost, "Data/Output/StateCost.csv")
rm(StateCost)

all.files <- list.files(path = "Data/Output", pattern = "F.csv")
mylist <- lapply(all.files, Readdata)
FlowCount <- rbindlist(mylist, fill = TRUE)
rm(mylist)
fwrite(FlowCount, "Data/Output/FlowCount.csv")
rm(FlowCount)

all.files <- list.files(path = "Data/Output", pattern = "FC.csv")
mylist <- lapply(all.files, Readdata)
FlowCost <- rbindlist(mylist, fill = TRUE)
rm(mylist)
fwrite(FlowCost, "Data/Output/FlowCost.csv")
rm(FlowCost)

all.files <- list.files(path = "Data/Output", pattern = "SQ.csv")
mylist <- lapply(all.files, Readdata)
StateQALY <- rbindlist(mylist, fill = TRUE)
rm(mylist)
fwrite(StateQALY, "Data/Output/StateQALY.csv")
rm(StateQALY)
