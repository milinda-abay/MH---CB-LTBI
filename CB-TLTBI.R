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
source("CB-TLTBI functions.R")


# This function uses the above three Fix* functions. 
# Run once to create the *.rds objects (vic.fertility, vic.mortality, vic.migration)
# based on ABS's population porjection data
CreateRDSDataFiles()


# Read the data files (if required)
aust <- readRDS("Data/aust.rds")
aust.LGA <- readRDS("Data/aust.LGA.rds") # this is required
prob.Inf <- readRDS("Data/prob.Inf.rds") 
tbhaz.200rep <- readRDS("Data/tbhaz.200rep.rds")
tbhaz.5000rep <- readRDS("Data/tbhaz.5000rep.rds")
vic.fertility <- readRDS("Data/vic.fertility.rds")
vic.mortality <- readRDS("Data/vic.mortality.rds") # this is also required
vic.migration <- readRDS("Data/vic.migration.rds")
vic.pop <- readRDS("Data/vic.pop.rds")
RRates <- readRDS("Data/RRates.rds") # this is also required

# Creatinga a vector of state names
state.names <- c("p.sus", "p.sus.fp.t", "p.sus.fp.tr", "p.sus.fp.tc", "p.sus.tn",
                 "p.ltbi", "p.ltbi.tp.t", "p.ltbi.tp.tr", "p.ltbi.tp.tc", "p.ltbi.tp.tc.tb",
                 "p.ltbi.tp.tc.tbr", "p.ltbi.fn", "p.ltbi.fn.tb", "p.ltbi.fn.tbr",
                 "p.death", "p.tp.tb.death", "p.fn.tb.death")

# Baseline with reduced states
state.names <- c("p.sus", "p.ltbi", "p.ltbi.tb", "p.tb.death", "p.death")

 #CreateStates(state.names) # --- not used --- instantiates a set of states objects with default vaules

# Creates an unevaluated transition matrix
# Use 'CMP' for complement and 'param$*' for parameters.
# Each parameter must be a pairlist argument in DefineParameters().
transMatrix <- DefineTransition(CMP,0.01, 0.005, 0, 0.09, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR, 0, 0,
                                0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0, 0,
                                0,  0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, param$RR, 0, 0, 0.01, 0, 0,
                                0,  0, .2, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0, 0,
                                0,  0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0, 0,
                                0,  0, 0, 0, 0, 0.97, 0.01, 0.005, 0, 0, 0, 0.005, 0, 0, 0.01, 0, 0,
                                0,  0, 0, 0, 0, 0, 0, 0, 0.99, 0, 0, 0, 0, 0, 0.01, 0, 0,
                                0,  0, 0, 0, 0, 0, 0, 0.98, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0,
                                0,  0, 0, 0, 0, 0, 0, 0, 0.98, 0.01, 0, 0, 0, 0, 0.01, 0, 0,
                                0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0.49, 0, 0, 0, 0.01, 0.5, 0,
                                0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0.99, 0, 0, 0, 0.01, 0, 0,
                                0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.89, 0.1, 0, 0.01, 0, 0,
                                0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.49, 0.01, 0, 0.5,
                                0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.99, 0.01, 0, 0,
                                0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                                0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                                0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, state.names = state.names)

# Baseline transition matrix
transMatrix <- DefineTransition(CMP, 0, 0, 0, param$MR,
                                0, CMP, param$RR, 0, param$MR,
                                0, 0, CMP, 0.00256, param$MR,
                                0, 0, 0, 1, 0,
                                0, 0, 0, 0, 1, state.names = state.names)


# Creates an unevaluated set of parameters
parameters <- DefineParameters(MR = Get.MR(DT, year, rate.assumption = "High"),
                               RR = Get.RR(DT, year))


# Uses aust.LGA.rds file to create a sample input
pop.master <- CreatePopulationMaster()

# Model parameters
start.year <- 2016
year <- start.year # initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles 

# Creates and initialises the population output table for the model (markov.cycle = 0)
# pop.output <- pop.master[YARP <= year & AGERP <= 40 & AGEP <= 40][, cycle := markov.cycle][1:100]
pop.output <- pop.master[YARP <= year][, cycle := markov.cycle]



# TODO - If start.year != 2016 then recalculate AGEP at start.year!

pop.output <- RunModel(pop.output)

# Saves output
saveRDS(pop.output, "Data/pop.output.rds")

Sys.time()