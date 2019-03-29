

DefineStates <- function(...) {

    .dots <- lazyeval::lazy_dots(...)
    structure(.dots, class = c("state", class(.dots)))
}

# *** Not used at this point*** Creates a default set of states and values
CreateStates <- function(state.names) {

    for (i in state.names) {
        assign(i, pos = 1, DefineStates(cost = 234, utility = 1))
    }

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
    pop.master.2020 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2020, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2021 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2021, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2022 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2022, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2023 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2023, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2024 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2024, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2025 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2025, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2026 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2026, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2027 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2027, NUMP, LTBP, AGERP, SEXP),]

    pop.master <- rbind(pop.master, pop.master.2017, pop.master.2018, pop.master.2019,
                        pop.master.2020, pop.master.2021, pop.master.2022, pop.master.2023,
                        pop.master.2024, pop.master.2025, pop.master.2026, pop.master.2027)

    rm(pop.master.2017, pop.master.2018, pop.master.2019, pop.master.2020, pop.master.2021,
       pop.master.2022, pop.master.2023, pop.master.2024, pop.master.2025, pop.master.2026,
       pop.master.2027)

    # Must order the pop.master table by YARP due to subsetting and recombining. 
    setkey(pop.master, YARP, SEXP, AGEP, ISO3)

    # Remove australian born and calculate the susceptible and latent population
    # TODO - Fix this! It is hard coded for 23 states.
    pop.master <- pop.master[, cycle := as.integer(NA)]
    pop.master <- pop.master[ISO3 != "AUS"][, (state.names) := .(NUMP - LTBP, 0, 0, 0, 0, LTBP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)]
    pop.master <- pop.master[, c(paste("V.", state.names, sep = "")) := NA]
    # pop.master <- pop.master[ISO3 != "AUS"][, (state.names) := .(NUMP - LTBP, LTBP, 0, 0, 0)]

    # Create a age at arrival column AGERP
    pop.master <- pop.master[, AGERP := AGEP - (2016 - YARP)]

}


ModifyPop <- function(pop.master, arglist) {

    arglist$drop.state.name()
    x <- aperm(arglist$show.list(), c(2, 1))


    pop.master[, ':='(p.sus.fp.t = p.sus * x[[1, 2]], p.sus.fp.nt = p.sus * x[[1, 3]],
                      p.sus.tn = p.sus * x[[1, 5]], p.ltbi.tp.t = p.ltbi * x[[6, 7]],
                      p.ltbi.tp.nt = p.ltbi * x[[6, 11]], p.ltbi.fn = p.ltbi * x[[6, 14]])]

    pop.master[, c("p.sus", "p.ltbi") := 0]

    pop.master

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
