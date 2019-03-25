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
#CreateRDSDataFiles()


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
vic.tb.mortality <- readRDS("Data/vic.tb.mortality.rds") # this is also required

# Creating a vector of state names
state.names <- c("p.sus", "p.sus.fp.t", "p.sus.fp.nt", "p.sus.fp.tc", "p.sus.tn",
                 "p.ltbi", "p.ltbi.tp.t", "p.ltbi.tp.tc", "p.ltbi.tp.tc.tb", "p.ltbi.tp.tc.tbr",
                 "p.ltbi.tp.nt", "p.ltbi.tp.nt.tb", "p.ltbi.tp.nt.tbr", "p.ltbi.fn", "p.ltbi.fn.tb",
                 "p.ltbi.fn.tbr", "p.ltbi.tb", "p.ltbi.tbr", "p.ltbi.tp.tc.tb.death", "p.ltbi.tp.nt.tb.death",
                 "p.ltbi.fn.tb.death", "p.ltbi.tb.death", "p.death")

# Number of states
state.number <- length(state.names)

arglist.4R <- CreateArgumentList(state.names, state.number)

transMatrix4R <- do.call(DefineTransition, arglist.4R)


#CreateStates(state.names) # --- not used --- instantiates a set of states objects with default vaules

# Creates an unevaluated transition matrix
# Use 'CMP' for complement and 'param$*' for parameters.
# Each parameter must be a pairlist argument in DefineParameters().
transMatrix4R <- DefineTransition(CMP, 0.05 * 0.07 * 0.6818, 0.05 * 0.07 * 0.3182, 0, 0.05 * 0.93, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, 0, 0, CMP, 0.05 * 0.5915 * 0.6818, 0, 0, 0, .05 * 0.5915 * 0.3182, 0, 0, 0.05 * 0.4085, 0, 0, param$RR, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, CMP, 0.04 * param$RR, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, param$TBMR, 0, 0, 0, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, param$RR, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, param$TBMR, 0, 0, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, param$RR, 0, 0, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, param$TBMR, 0, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, param$TBMR, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, param$MR,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, state.names = state.names)

# Baseline transition matrix
transMatrixBaseline <- DefineTransition(CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$RR, 0, 0, 0, 0, 0, param$MR,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, param$TBMR, param$MR,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, CMP, 0, 0, 0, 0, param$MR,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                        state.names = state.names)

# Depending on which model to run
transMatrix <- transMatrix4R
# transMatrix <- transMatrixBaseline


# Creates an unevaluated set of parameters
parameters <- DefineParameters(MR = Get.MR(DT, year, rate.assumption = "High"),
                               RR = Get.RR(DT, year),
                               TBMR = Get.TBMR(DT, year))


# Uses aust.LGA.rds file to create a sample input
pop.master <- CreatePopulationMaster()

# Model parameters
start.year <- 2016
year <- start.year # Initialise year with start.year
markov.cycle <- 0 # Tracks the current cycle
cycles <- 10 # Model run cycles
n_cohorts_to_evaluate <- nrow(pop.master) # Can be adjusted to save running time if you don't want to evaluate the entire population
n_cohorts_to_evaluate <- 10

# Creates and initialises the population output table for the model (markov.cycle = 0)
# pop.output <- pop.master[YARP <= year & AGERP <= 40 & AGEP <= 40][, cycle := markov.cycle][1:100]
pop.output <- pop.master[YARP <= year][, cycle := markov.cycle][1: n_cohorts_to_evaluate]

# Toggle to reduce number of cohorts to evaluate to speed running time
cohorts_to_track <- nrow(pop.output)
#cohorts_to_track <- 1e4
  
# TODO - If start.year != 2016 then recalculate AGEP at start.year!

pop.output <- RunModel(pop.output[1: cohorts_to_track])

# Saves output
saveRDS(pop.output, "Data/pop.output.Baseline.rds")

