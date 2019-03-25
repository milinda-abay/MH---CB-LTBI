# The set of Define* functions implement lazy evaluation and create respective objects.
# Ultimately a singular model object will have multiple strategies with multiple transition matrices and states.

# TODO - implement DefineStates & DefineStrategy
# The Define* series of functions are used to setup the Model

DefineStates <- function(...) {

    .dots <- lazyeval::lazy_dots(...)
    structure(.dots, class = c("state", class(.dots)))
}


DefineTransition <- function(..., state.names) {

    # Extract transition matrix from first arguments, of any number, and assign names
    .dots <- lazyeval::lazy_dots(...)
    n <- sqrt(length(.dots))
    names(.dots) <- sprintf("cell_%i_%i", rep(seq_len(n), each = n), rep(seq_len(n), n))
    
    # Perform checks
    CheckSquare(n, state.names)
    CheckComplement(.dots, n)
    
    structure(.dots, class = c("uneval_matrix", class(.dots)), state.names = as.vector(state.names))
}


DefineParameters <- function(...) {

    .dots <- lazyeval::lazy_dots(...)
    structure(.dots, class = c("uneval_parameters", class(.dots)))
}


DefineStrategy <- function(..., transition = DefineTransition()) {

    .dots <- lazyeval::lazy_dots(...)
    states <- structure(.dots, class = c("uneval_state_list", class(.dots)))
    structure(list(transition = transition, states = states), class = "uneval_model")
}


# The following functions perform various calculations at model runtime.
#------------------------------------------------------------------------#

CheckSquare <- function(root, states) {
  if (!length(states) == root) {
    stop("Transition matrix is not square of number of states")
  }
}

CheckComplement <- function(transition.matrix, l) {
    # Used by DefineTransition to verify only one CMP (complement) parameter per row in the transition matrix

    cmp.pos <- sapply(transition.matrix, function(x) x[1], simplify = TRUE)
    cmp.pos <- cmp.pos == quote(CMP)
    dim(cmp.pos) <- c(l, l)

    # Sum by columns because cmp.pos because is filled column-wise and so is tranposed
    if (!all(colSums(cmp.pos) <= 1)) {
        stop("Only one 'CMP' is allowed per matrix row.")
    } 
}

CreateArgumentList <- function(state.names, state.number) {

    # Create and initialise a list
    arglist <- rep(list(NA), state.number ^ 2)
    dim(arglist) <- c(state.number, state.number)

  
    arglist[1,] <- c(quote(CMP), quote(0.05 * 0.07 * 0.6818), 0.05 * 0.07 * 0.3182, 0, 0.05 * 0.93, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, quote(MR))
    
    print(arglist)



    arglist[[state.number ^ 2 + 1]] <- state.names
    names(arglist)[530] <- "state.names"

    arglist

}

# Look up the mortality rate from vic.mortality
Get.MR <- function(DT, year, rate.assumption = "High") {

    vic.mortality[Year == year & mrate == rate.assumption][DT, Rate, on = .(Age = AGEP, Sex = SEXP)]

}

# Look up the Reactivation rate
Get.RR <- function(DT, year) {

    RRates[DT[, .(AGERP, SEXP, ST = year - YARP)], Rate, on = .(Age = AGERP, Sex = SEXP, statetime = ST)]

}


Get.TBMR <- function(DT, year) {

    vic.tb.mortality[DT[, .(AGEP, SEXP)], rate, on = .(age = AGEP, sex = SEXP)]
    
}

# Calculates the CMP value after evaluation of the promise objects in parameter and transition matrix.
CalculateCMP <- function(tM, l, z) {

    y <- unlist(tM)

    dim(y) <- c(z, l, l)

    # permutate the aray
    y <- aperm(y, perm = c(1, 3, 2))

    # 3D logical array of -pi locations
    posC <- y == -pi

    # now set -pi's to 0
    y[posC] <- 0

    # dropping the 3rd dimension in order to select the rowSums in sequence.
    valC <- 1 - rowSums(y, dims = 2)[which(posC, arr.ind = TRUE)[, -3]]

    # inserting the values back in
    y[posC] <- valC

    # permutate again to get original shape
    y <- aperm(y, perm = c(1, 3, 2))

    dim(y) <- c(z, l * l)

    colnames(y) <- names(tM)

    data.table(y)
}


# Performs matrix multiplication on each row (cohort) with the evaluated transition matrix for that row.
PerformMatrixMultiplication <- function(dM, tM, l, z) {

    # Make the current data matrix a list
    bar <- unlist(dM)

    # Make it an array then permute it
    dim(bar) <- c(z, l, 1)
    bar <- aperm(bar, perm = c(3, 2, 1))

    # Do the same for the transition matrix
    foo <- unlist(tM)
    dim(foo) <- c(z, l, l)
    foo <- aperm(foo, perm = c(3, 2, 1))

    # Carry out the matrix multiplication with a data.table frame.
    # Enables iteration and subsetting by using .I
    dM[, as.list(matrix(bar[,, .I], ncol = l) %*% foo[,, .I]), by = seq_len(z)]
    
}


# The primary function RunModel() calls to performs row by row transition calculations.
GetStateCounts <- function(DT, year) {

    z <- nrow(DT)
    l <- sqrt(length(transMatrix))

    # assign the current environment for evaluation. 
    # TODO - create a function for multiple parameters
    parameters$MR$env <- environment()
    parameters$RR$env <- environment()
    parameters$TBMR$env <- environment()

    # evaluate parameters 
    # NOTE: at this point both Get.MR() and Get.RR() functions are called by the evaluator.
    # Use param$* when using DefineTransition() 
    param <- lazy_eval(parameters)

    # a Hack for YARP < 2016, vic mortality doesnt have data to look up 
    param$MR[is.na(param$MR)] <- 0.01
    param$RR[is.na(param$RR)] <- 0.0013
    param$TBMR[is.na(param$TBMR)] <- 0.01

    
    # assign the current environment for evaluation. 
    for (i in 1:length(transMatrix)) {

        transMatrix[[i]]$env <- environment()

    }


    # Evaluates the transition matrix and insert a '-pi' placeholder for CMP.
    tM <- lazy_eval(transMatrix, data = list(CMP = -pi))


    # Scalar values don't get evaluated into vectors
    # loop and manually expand to vectors
    for (i in 1:length(transMatrix)) {

        if (length(tM[[i]]) == 1) {

            tM[[i]] <- rep(tM[[i]], times = z)
        }
    }


    # Manipulates the tM to calculate the CMP
    tM <- CalculateCMP(tM, l, z)

    # Select the numeric state value columns in preparation for multiplication.
    dM <- DT[, ..state.names]

    

    print("PMM Start")
    print(Sys.time())
    results <- PerformMatrixMultiplication(dM, tM, l, z)
    print("PMM End")
    print(Sys.time())

    #browser() # uncomment for testing

    results[, - c("seq_len")]

}

#------------------------------------------------------------------------#

# The main model runtime loop 
RunModel <- function(pop.output) {

    # initialise the calculation object
    pop.calculated <- copy(pop.output)

    while (markov.cycle != cycles) {

        print(nrow(pop.calculated))
        print(pop.calculated[1:10,.N, by =.(AGEP,cycle)])

        # The vectorised solution where the entire table is passed to GetStateCounts
        pop.calculated[, c(state.names) := GetStateCounts(pop.calculated, year)]

        # Update counters
        markov.cycle <- markov.cycle + 1
        year <- year + 1
        
        # Inflows for next cycle. 
        # A conditional flag use this for testing.
        modelinflow <- FALSE

        if (modelinflow == TRUE) {
            pop.inflow <- pop.master[YARP == year,][, cycle := NA][1:10] # [1:10] due to testing
        } else {
            pop.inflow <- NULL
        }
        
        
        # Aging the population in the calculation object
        pop.calculated[, AGEP := AGEP + 1]
        
        pop.calculated <- rbind(pop.calculated, pop.inflow)
        pop.calculated[, cycle := markov.cycle]

        
        # Calculate state values
        # CalculateStateValues(pop.calculated)

        # Saving state in pop.output
        pop.output <- rbind(pop.output, pop.calculated)
        
    }

    pop.output
}

# Utility functions for data cleansing and reshaping
FixFertility <- function(hf, mf, lf) {

    # Prepares the fertility data into a data.table.

    # Args: 
    #   hf: a data.table of high fertility rates.
    #   mf: a data.table of medium fertility rates.
    #   lf: a data.table of low fertility rates.
    #
    # Return:
    #   a data.table with age, year, rate and fertility as columns.

    names(hf) <- c("Age", 2017:2027)
    names(mf) <- c("Age", 2017:2027)
    names(lf) <- c("Age", 2017:2027)

    hf.firstrow <- which(hf$"2017" == "Victoria") + 2
    hf.lastrow <- which(hf$"2017" == "Queensland") - 4

    mf.firstrow <- which(mf$"2017" == "Victoria") + 2
    mf.lastrow <- which(mf$"2017" == "Queensland") - 4

    lf.firstrow <- which(lf$"2017" == "Victoria") + 2
    lf.lastrow <- which(lf$"2017" == "Queensland") - 4

    hf <- hf[hf.firstrow:hf.lastrow,,]
    mf <- mf[mf.firstrow:mf.lastrow,,]
    lf <- lf[lf.firstrow:lf.lastrow,,]

    hf$"2017" <- as.numeric(hf$"2017")
    mf$"2017" <- as.numeric(mf$"2017")
    lf$"2017" <- as.numeric(lf$"2017")

    hf$Age <- as.integer(hf$Age)
    mf$Age <- as.integer(mf$Age)
    lf$Age <- as.integer(lf$Age)

    hf <- melt(hf, id.vars = "Age", variable.factor = F, variable.name = "Year", value.name = "Rate")
    mf <- melt(mf, id.vars = "Age", variable.factor = F, variable.name = "Year", value.name = "Rate")
    lf <- melt(lf, id.vars = "Age", variable.factor = F, variable.name = "Year", value.name = "Rate")

    hf$Year <- as.integer(hf$Year)
    mf$Year <- as.integer(mf$Year)
    lf$Year <- as.integer(lf$Year)

    hf[, frate := .("High"),]
    mf[, frate := .("Med"),]
    lf[, frate := .("Low"),]



    return(rbind(hf, mf, lf))

}

FixMortality <- function(hm, mm) {

    # Prepares the mortality data into a table.

    # Args: 
    #   hm: a data.table of high mortality rates
    #   mm: a data.table of medium mortality rates

    # Return:
    #   a data.table with age, year, proportion and mortality as dimensions.

    hm <- hm[7:nrow(hm), c("Australian Bureau of Statistics", "..2", "..4", "..14")]
    mm <- mm[7:nrow(mm), c("Australian Bureau of Statistics", "..2", "..4", "..14")]

    names(hm) <- c("Year", "Age", "Male", "Female")
    names(mm) <- c("Year", "Age", "Male", "Female")

    hm$Age <- as.integer(hm$Age)
    mm$Age <- as.integer(mm$Age)


    hm <- melt(hm, id.vars = c("Age", "Year"), variable.factor = F, variable.name = "Sex", value.name = "Rate")
    mm <- melt(mm, id.vars = c("Age", "Year"), variable.factor = F, variable.name = "Sex", value.name = "Rate")

    hm$Year <- as.integer(hm$Year)
    mm$Year <- as.integer(mm$Year)

    hm$Rate <- as.numeric(hm$Rate)
    mm$Rate <- as.numeric(mm$Rate)

    hm[, mrate := .("High"),]
    mm[, mrate := .("Med"),]


    return(rbind(hm, mm))

}

FixMigration <- function(hma, hmd, mma, mmd, lma, lmd) {

    # Prepares the migration data into an table.

    # Args: 
    #   hma: a data.table of high migration arrival rates
    #   hmd: a data.table of high migration departure rates
    #   mma: a data.table of medium migration arrival rates
    #   mmd: a data.table of medium migration departure rates
    #   lma: a data.table of high migration arrival rates
    #   lmd: a data.table of high migration departure rates

    # Return:
    #   a data.table with age, year, proportion and mortality as dimensions.

    #hma <- vic.high.migration.arrivals
    #hmd <- vic.high.migration.departures
    #mma <- vic.medium.migration.arrivals
    #mmd <- vic.medium.migration.departures
    #lma <- vic.low.migration.arrivals
    #lmd <- vic.low.migration.departures

    hma <- hma[8:nrow(hma), c("Australian Bureau of Statistics", "..2", "..4", "..14")]
    hmd <- hmd[8:nrow(hmd), c("Australian Bureau of Statistics", "..2", "..4", "..14")]

    mma <- hma[8:nrow(mma), c("Australian Bureau of Statistics", "..2", "..4", "..14")]
    mmd <- mmd[8:nrow(mmd), c("Australian Bureau of Statistics", "..2", "..4", "..14")]

    lma <- lma[8:nrow(lma), c("Australian Bureau of Statistics", "..2", "..4", "..14")]
    lmd <- lmd[8:nrow(lmd), c("Australian Bureau of Statistics", "..2", "..4", "..14")]

    names(hma) <- c("Year", "Age", "Male", "Female")
    names(hmd) <- c("Year", "Age", "Male", "Female")
    names(mma) <- c("Year", "Age", "Male", "Female")
    names(mmd) <- c("Year", "Age", "Male", "Female")
    names(lma) <- c("Year", "Age", "Male", "Female")
    names(lmd) <- c("Year", "Age", "Male", "Female")

    hma$Age <- as.integer(hma$Age)
    hmd$Age <- as.integer(hmd$Age)

    mma$Age <- as.integer(mma$Age)
    mmd$Age <- as.integer(mmd$Age)

    lma$Age <- as.integer(lma$Age)
    lmd$Age <- as.integer(lmd$Age)

    hma <- melt(hma, id.vars = c("Age", "Year"), variable.factor = F, variable.name = "Sex", value.name = "Rate")
    hmd <- melt(hmd, id.vars = c("Age", "Year"), variable.factor = F, variable.name = "Sex", value.name = "Rate")

    mma <- melt(mma, id.vars = c("Age", "Year"), variable.factor = F, variable.name = "Sex", value.name = "Rate")
    mmd <- melt(mmd, id.vars = c("Age", "Year"), variable.factor = F, variable.name = "Sex", value.name = "Rate")

    lma <- melt(lma, id.vars = c("Age", "Year"), variable.factor = F, variable.name = "Sex", value.name = "Rate")
    lmd <- melt(lmd, id.vars = c("Age", "Year"), variable.factor = F, variable.name = "Sex", value.name = "Rate")

    hma$Year <- as.integer(hma$Year)
    hmd$Year <- as.integer(hmd$Year)

    mma$Year <- as.integer(mma$Year)
    mmd$Year <- as.integer(mmd$Year)

    lma$Year <- as.integer(lma$Year)
    lmd$Year <- as.integer(lmd$Year)

    hma$Rate <- as.numeric(hma$Rate)
    hmd$Rate <- as.numeric(hmd$Rate)
    mma$Rate <- as.numeric(mma$Rate)
    mmd$Rate <- as.numeric(mmd$Rate)
    lma$Rate <- as.numeric(lma$Rate)
    lmd$Rate <- as.numeric(lmd$Rate)

    hma[, c("Flow", "mrate") := .("Arrival", "High"),]
    hmd[, c("Flow", "mrate") := .("Departure", "High"),]

    mma[, c("Flow", "mrate") := .("Arrival", "Medium"),]
    mmd[, c("Flow", "mrate") := .("Departure", "Medium"),]

    lma[, c("Flow", "mrate") := .("Arrival", "Low"),]
    lmd[, c("Flow", "mrate") := .("Departure", "Low"),]

    return(rbind(hma, hmd, mma, mmd, lma, lmd))

}

CreateRDSDataFiles <- function() {
    # Uses the FixFertility, Fix Mortality & FixMigration functions to create RDS data.table objects.

    # Args: 
    #   None, this function contains code for the creation of RDS objects.

    # Return:
    #   No returns, but it saves the following *.rds data object in the ./Data folder
    #   vic.pop.rds - ABS victoria population projections
    #   vic.fertility.rds - ABS victoria fertility projections
    #   vic.mortality.rds - ABS victoria mortality projections
    #   vic.migration.rds - ABS victoria migration projections


    # Loading ABS population projections for Victoria 2017-2025
    vic.pop2017 <- fread("Data/POP_PROJ_REGION_2012_2061_11012019113857503.csv", skip = 0, header = T, stringsAsFactors = F)
    vic.pop2018 <- fread("Data/POP_PROJ_REGION_2012_2061_11012019114104680.csv", skip = 0, header = T, stringsAsFactors = F)
    vic.pop2019 <- fread("Data/POP_PROJ_REGION_2012_2061_11012019114211913.csv", skip = 0, header = T, stringsAsFactors = F)
    vic.pop2020 <- fread("Data/POP_PROJ_REGION_2012_2061_11012019114347166.csv", skip = 0, header = T, stringsAsFactors = F)
    vic.pop2021 <- fread("Data/POP_PROJ_REGION_2012_2061_11012019114421604.csv", skip = 0, header = T, stringsAsFactors = F)
    vic.pop2022 <- fread("Data/POP_PROJ_REGION_2012_2061_11012019114458184.csv", skip = 0, header = T, stringsAsFactors = F)
    vic.pop2023 <- fread("Data/POP_PROJ_REGION_2012_2061_11012019114533489.csv", skip = 0, header = T, stringsAsFactors = F)
    vic.pop2024 <- fread("Data/POP_PROJ_REGION_2012_2061_11012019114605011.csv", skip = 0, header = T, stringsAsFactors = F)
    vic.pop2025 <- fread("Data/POP_PROJ_REGION_2012_2061_11012019114642182.csv", skip = 0, header = T, stringsAsFactors = F)

    # Merge and consolidate population projections from 2017 to 2025
    vic.pop <- rbind(vic.pop2017, vic.pop2018, vic.pop2019, vic.pop2020, vic.pop2021, vic.pop2022, vic.pop2023, vic.pop2024, vic.pop2025)
    saveRDS(vic.pop, "Data/vic.pop.rds")
    rm(vic.pop2017, vic.pop2018, vic.pop2019, vic.pop2020, vic.pop2021, vic.pop2022, vic.pop2023, vic.pop2024, vic.pop2025)

    # Read excel data files
    vic.high.fertility <- setDT(read_excel("Data/32220ds02_2017-2066_projection_assumptions_detailed.xls", sheet = 2))
    vic.medium.fertility <- setDT(read_excel("Data/32220ds02_2017-2066_projection_assumptions_detailed.xls", sheet = 3))
    vic.low.fertility <- setDT(read_excel("Data/32220ds02_2017-2066_projection_assumptions_detailed.xls", sheet = 4))

    vic.high.mortality <- setDT(read_excel("Data/32220ds02_2017-2066_projection_assumptions_detailed.xls", sheet = 5))
    vic.medium.mortality <- setDT(read_excel("Data/32220ds02_2017-2066_projection_assumptions_detailed.xls", sheet = 6))

    vic.high.migration.arrivals <- setDT(read_excel("Data/32220ds02_2017-2066_projection_assumptions_detailed.xls", sheet = 7))
    vic.high.migration.departures <- setDT(read_excel("Data/32220ds02_2017-2066_projection_assumptions_detailed.xls", sheet = 8))

    vic.medium.migration.arrivals <- setDT(read_excel("Data/32220ds02_2017-2066_projection_assumptions_detailed.xls", sheet = 9))
    vic.medium.migration.departures <- setDT(read_excel("Data/32220ds02_2017-2066_projection_assumptions_detailed.xls", sheet = 10))

    vic.low.migration.arrivals <- setDT(read_excel("Data/32220ds02_2017-2066_projection_assumptions_detailed.xls", sheet = 11))
    vic.low.migration.departures <- setDT(read_excel("Data/32220ds02_2017-2066_projection_assumptions_detailed.xls", sheet = 12))

    # Create and save fertility rates table
    vic.fertility <- FixFertility(vic.high.fertility, vic.medium.fertility, vic.low.fertility)
    rm(vic.high.fertility, vic.medium.fertility, vic.low.fertility)
    saveRDS(vic.fertility, "Data/vic.fertility.rds")

    # Create and save mortality rates table
    vic.mortality <- FixMortality(vic.high.mortality, vic.medium.mortality)
    rm(vic.high.mortality, vic.medium.mortality)
    saveRDS(vic.mortality, "Data/vic.mortality.rds")

    #Create and save migration rates table
    vic.migration <- FixMigration(vic.high.migration.arrivals, vic.high.migration.departures, vic.medium.migration.arrivals, vic.medium.migration.departures, vic.low.migration.arrivals, vic.low.migration.departures)
    rm(vic.high.migration.arrivals, vic.high.migration.departures, vic.medium.migration.arrivals, vic.medium.migration.departures, vic.low.migration.arrivals, vic.low.migration.departures)
    saveRDS(vic.migration, "Data/vic.migration.rds")

    # Create and save TB mortality rates
    vic.tb.mortality <- fread("Data/TBmortality_VIC_2002_2013.csv")
    vic.tb.mortality <- vic.tb.mortality[sex != "both"][, .(age, sex, rate = (died / total) / (2014 - 2002))]
    vic.tb.mortality[sex == "male", sex := "Mmale"]
    vic.tb.mortality[sex == "female", sex := "Female"]
    saveRDS(vic.tb.mortality , "Data/vic.tb.mortality.rds")




}

# Creates a master migrant population table
CreatePopulationMaster <- function() {

    # Create a pop.master table merging census (2006,2011, 2016) and ABS projection data.
    # It must be a long format table with the following structure.
    #
    # Sex of person (SEXP), Age at census (AGEP), Year of arrival (YARP),
    # Birth place of person (ISO3), local government area (LGA),
    # Number of persons	(NUMP)
    #__________________________________________________
    # SEXP    | AGEP  |  YARP | ISO3 |  LGA    | NUMP |
    #--------------------------------------------------
    # Male	  | 10	  |  2006 | AFG	 |  Casey  | 4	  |  NUMP =  { average of 3 census (2006,2011,2016) datasets } 
    # Female  |	12	  |  2007 |	IND	 |  Monash | 10	  |  NUMP =  { average of 2 census (2011,2016) datasets } 
    # ?	      | ?	  |  ?	  |  ?	 |  ?	   | ?	  |  
    # Male	  | 30	  |  2016 |	VNM	 |  Hume   | 7	  |  NUMP =  { census 2016 datasets } 
    # ?	      | ?	  |  ?	  |  ?	 |  ?	   | ?	  |
    # ------------------2017---------------------------	No data for YARP 2017
    # Male	  | 50	  |  2018 |	?	 |  ?	   | 324  |	ABS migration projection with three assumptions (high, med & low ) by arrivals and departures
    # Female  |	40	  |  2027 |	?	 |  ?	   | 721  |	Net overseas migration levels will remain constant from YARP>2027 onwards
    #						
    # TODO -> Based on census datasets (2006,2011,2016) estimate a NUMP distribution for YARP > 2018  by LGA and ISO3.						
    #
    # As a validation exercise the aust.LGA cohort is duplicated into male & female and LGA aggregated
    # This done to validate the model runtime. It must be fixed!

    pop.master.male <- aust.LGA[, .(NUMP = .5 * sum(NUMP), LTBP = .5 * sum(LTBP), AGERP = AGEP - (2016 - YARP), SEXP = "Male"), by = c("AGEP", "ISO3", "YARP")]
    pop.master.female <- pop.master.male[, .(AGEP, ISO3, YARP, NUMP, LTBP, AGERP, SEXP = "Female")]

    pop.master <- rbind(pop.master.male, pop.master.female)
    rm(pop.master.female, pop.master.male)

    # Also creating migrant cohort arrivals for YARP > 2016. i.e. 2017 to 2025.
    # again this is for validating the model at runtime.

    pop.master.2017 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2017, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2018 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2018, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2019 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2019, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2020 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2019, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2021 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2019, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2022 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2019, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2023 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2019, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2024 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2019, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2025 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2019, NUMP, LTBP, AGERP, SEXP),]

    pop.master <- rbind(pop.master, pop.master.2017, pop.master.2018, pop.master.2019,
                        pop.master.2020, pop.master.2021, pop.master.2022, pop.master.2023,
                        pop.master.2024, pop.master.2025)

    rm(pop.master.2017, pop.master.2018, pop.master.2019, pop.master.2020, pop.master.2021,
       pop.master.2022, pop.master.2023, pop.master.2024, pop.master.2025)

    # Must order the pop.master table by YARP due to subsetting and recombining. 
    setkey(pop.master, YARP, SEXP, AGEP, ISO3)

    # Remove australian born and calculates the susceptible and latent population
    # TODO - Fix this! It is hard coded for 23 states.
    pop.master <- pop.master[ISO3 != "AUS"][, (state.names) := .(NUMP - LTBP, 0, 0, 0, 0, LTBP, 0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0)]

    # pop.master <- pop.master[ISO3 != "AUS"][, (state.names) := .(NUMP - LTBP, LTBP, 0, 0, 0)]

    pop.master <- pop.master[, AGERP := AGEP-(2016-YARP)]

}


# *** Not used at this point*** Creates a default set of states and values
CreateStates <- function(state.names) {

    for (i in state.names) {
        assign(i, pos = 1, DefineStates(cost = 234, untility = 1))
    }

}


#strategy <- DefineStrategy(

  #transition = transMatrix,
  #p.sus = p.sus,
  #p.death = p.death,
  #p.ltbi = p.ltbi
#)



# Create state value measures

#state.measures <- c("QALY", "Cost of TST", "Cost of IGRA", "Cost of 4R", "Cost of hospitalisation")

#state.value.matrix <- array(NA, dim = c(length(state.names), length(state.measures), cycles),
                            #dimnames = list(states = state.names, measures = state.measures, cycles = 1:cycles))



