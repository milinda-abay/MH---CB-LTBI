


# *** Not used at this point*** Creates a default set of states and values
CreateStates <- function(state.names) {

    for (i in state.names) {
        assign(i, pos = 1, DefineStates(cost = COST, utility = UTILITY))
    }

}

# Creates a master migrant population table
CreatePopulationMaster <- function(Modify = FALSE) {

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



    pop.master <- aust.vic[, .(AGEP, ISO3, YARP, NUMP, LTBP, AGERP = AGEP - (2016L - YARP), SEXP)]


    # Also creating migrant cohort arrivals for YARP > 2016. i.e. 2017 to 2025.
    # again this is for validating the model at runtime.

    # deleted 2016 due to it being a census year with 1/2 half.
    pop.master <- pop.master[YARP != 2016]



    pop.master.2016 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2016L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2017 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2017L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2018 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2018L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2019 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2019L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2020 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2020L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2021 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2021L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2022 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2022L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2023 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2023L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2024 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2024L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2025 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2025L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2026 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2026L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2027 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2027L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2028 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2028L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2029 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2029L, NUMP, LTBP, AGERP, SEXP),]
    pop.master.2030 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2030L, NUMP, LTBP, AGERP, SEXP),]


    pop.master <- rbind(pop.master, pop.master.2016, pop.master.2017, pop.master.2018, pop.master.2019,
                        pop.master.2020, pop.master.2021, pop.master.2022, pop.master.2023,
                        pop.master.2024, pop.master.2025, pop.master.2026, pop.master.2027,
                        pop.master.2028, pop.master.2029, pop.master.2030)

    rm(pop.master.2016, pop.master.2017, pop.master.2018, pop.master.2019, pop.master.2020, pop.master.2021,
       pop.master.2022, pop.master.2023, pop.master.2024, pop.master.2025, pop.master.2026,
       pop.master.2027, pop.master.2028, pop.master.2029, pop.master.2030)

    # Must order the pop.master table by YARP due to sub-setting and recombining. 
    setkey(pop.master, YARP, SEXP, AGEP, ISO3)

    # Calculate the susceptible and latent population
    # TODO - Fix this! It is hard coded for 23 states.
    pop.master <- pop.master[, cycle := as.integer(NA)]
    pop.master <- pop.master[, (new.state.names) := as.numeric(NA)]
    pop.master <- pop.master[, (state.names) := .(NUMP - LTBP, 0, 0, 0, 0, LTBP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)]
    

    # Create a age at arrival column AGERP
    pop.master[YARP < 2016, AGERP := AGEP - (2016L - YARP)]
    pop.master[YARP == 2016, AGERP := AGEP]
    pop.master[YARP > 2016, AGERP := AGEP - 1L] # because the 2015 cohort is replicated for 2017 to 2030.


    if (Modify) {

        # recheck this logic! Only for S1 100% off-shore testing.
        # Making the cohort one year younger and starting them from 2019 in p.sus and p.ltbi.
        pop.master[YARP >= 2019, AGEP := AGEP - 1L]

    }

    pop.master

}

CreateRDSDataFiles <- function() {
    # Uses the FixFertility, Fix Mortality & FixMigration functions to create RDS data.table objects.

    # Args: 
    #   None, this function contains code for the creation of RDS objects.

    # Return:
    #   No returns, but it saves the following *.rds data object in the ./Data folder
    #   vic.pop.rds - ABS Victoria population projections
    #   vic.fertility.rds - ABS Victoria fertility projections
    #   vic.mortality.rds - ABS Victoria mortality projections
    #   vic.migration.rds - ABS Victoria migration projections


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
    vic.mortality.101 <- vic.mortality[Age == 100][, Age := 101]
    vic.mortality.102 <- vic.mortality[Age == 100][, Age := 102]
    vic.mortality.103 <- vic.mortality[Age == 100][, Age := 103]
    vic.mortality.104 <- vic.mortality[Age == 100][, Age := 104]
    vic.mortality.105 <- vic.mortality[Age == 100][, Age := 105]
    vic.mortality.106 <- vic.mortality[Age == 100][, Age := 106]
    vic.mortality.107 <- vic.mortality[Age == 100][, Age := 107]
    vic.mortality.108 <- vic.mortality[Age == 100][, Age := 108]
    vic.mortality.109 <- vic.mortality[Age == 100][, Age := 109]
    vic.mortality.110 <- vic.mortality[Age == 100][, Age := 110]

    vic.mortality <- rbind(vic.mortality, vic.mortality.101, vic.mortality.102, vic.mortality.103, vic.mortality.104,
                       vic.mortality.105, vic.mortality.106, vic.mortality.107, vic.mortality.108,
                       vic.mortality.109, vic.mortality.110)
    vic.mortality[, Prob := RateToProb(Rate)]
    saveRDS(vic.mortality, "Data/vic.mortality.rds")

    #Create and save migration rates table
    vic.migration <- FixMigration(vic.high.migration.arrivals, vic.high.migration.departures, vic.medium.migration.arrivals, vic.medium.migration.departures, vic.low.migration.arrivals, vic.low.migration.departures)
    rm(vic.high.migration.arrivals, vic.high.migration.departures, vic.medium.migration.arrivals, vic.medium.migration.departures, vic.low.migration.arrivals, vic.low.migration.departures)
    saveRDS(vic.migration, "Data/vic.migration.rds")

    # Create and save TB mortality rates
    vic.tb.mortality <- fread("Data/TBmortality_VIC_2002_2013.csv")
    vic.tb.mortality <- vic.tb.mortality[sex != "both"]
    vic.tb.mortality[sex == "male", sex := "Male"]
    vic.tb.mortality[sex == "female", sex := "Female"]
    vic.tb.mortality[, Prob := RateToProb(Rate)]

    saveRDS(vic.tb.mortality, "Data/vic.tb.mortality.rds")

}

# Converts each *.rds files to five(S, SC, SQ, F & FC) *.csv files
CreateOutput <- function(strategy, test, treatment) {
    

    if (test == "No Test") {

        DT <- readRDS("Data/Output/S0.rds")

    } else {

        DT <- readRDS(paste("Data/Output/", strategy, ".", test, ".", treatment, ".rds", sep = ""))
        
    }

    DT[, c("Strategy", "Test", "Treatment") := .(strategy, test, treatment)]
    DT <- DT[, c(124:126, 1:123)]


    colsToSum <- names(DT)[c(7, 8, 12:126)]

    DT <- DT[, lapply(.SD, sum, na.rm = TRUE), by = .(Strategy, Test, Treatment, ISO3, AGEP, SEXP, cycle), .SDcols = colsToSum]
        
    cyc <- which(colnames(DT) == "LTBP")
    psus <- which(colnames(DT) == "p.sus")
    pdeath <- which(colnames(DT) == "p.death")
    Vpsus <- which(colnames(DT) == "V.p.sus")
    Vpdeath <- which(colnames(DT) == "V.p.death")
    SCpsus <- which(colnames(DT) == "SC.p.sus")
    SCpdeath <- which(colnames(DT) == "SC.p.death")
    FCpsus <- which(colnames(DT) == "FC.p.sus")
    FCpdeath <- which(colnames(DT) == "FC.p.death")
    SQpsus <- which(colnames(DT) == "SQ.p.sus")
    SQpdeath <- which(colnames(DT) == "SQ.p.death")


    # State count table
    DT.S <- DT[, c(1:..pdeath)]
    fwrite(DT.S, paste("Data/Output/", strategy, "_", test, "_", treatment, "_S.csv", sep = ""))
    #DT.S <- fread(file = paste("Data/Output/", strategy, "_", test, "_", treatment, "_S.csv", sep = ""))
    #saveRDS(DT.S, paste("Data/Output/", strategy, "_", test, "_", treatment, "_S.rds", sep = ""))
    rm(DT.S)
    
    # Flow count table
    DT.F <- DT[, c(1:..cyc, ..Vpsus:..Vpdeath)]
    colnames(DT.F) <- gsub("V.", "", colnames(DT.F))
    fwrite(DT.F, paste("Data/Output/", strategy, "_", test, "_", treatment, "_F.csv", sep = ""))
    #DT.F <- fread(file = paste("Data/Output/",strategy,"_",test,"_", treatment, "_F.csv", sep =""))
    #saveRDS(DT.F, paste("Data/Output/", strategy, "_", test, "_", treatment, "_F.rds", sep = ""))
    rm(DT.F)
    
    # State cost table
    DT.SC <- DT[, c(1:..cyc, ..SCpsus:..SCpdeath)]
    colnames(DT.SC) <- gsub("SC.", "", colnames(DT.SC))
    fwrite(DT.SC, paste("Data/Output/", strategy, "_", test, "_", treatment, "_SC.csv", sep = ""))
    #DT.SC <- fread(file = paste("Data/Output/", strategy, "_", test, "_", treatment, "_SC.csv", sep =""))
    #saveRDS(DT.SC, paste("Data/Output/", strategy, "_", test, "_", treatment, "_SC.rds", sep = ""))
    rm(DT.SC)

    # Flow cost table
    DT.FC <- DT[, c(1:..cyc, ..FCpsus:..FCpdeath)]
    colnames(DT.FC) <- gsub("FC.", "", colnames(DT.FC))
    fwrite(DT.FC, paste("Data/Output/", strategy, "_", test, "_", treatment, "_FC.csv", sep = ""))
    #DT.FC <- fread(file = paste("Data/Output/", strategy, "_", test, "_", treatment, "_FC.csv", sep = ""))
    #saveRDS(DT.FC, paste("Data/Output/", strategy, "_", test, "_", treatment, "_FC.rds", sep = ""))
    rm(DT.FC)

    DT.SQ <- DT[, c(1:..cyc, ..SQpsus:..SQpdeath)]
    colnames(DT.SQ) <- gsub("SQ.", "", colnames(DT.SQ))
    fwrite(DT.SQ, paste("Data/Output/", strategy, "_", test, "_", treatment, "_SQ.csv", sep = ""))
    #DT.SQ <- fread(file = paste("Data/Output/", strategy, "_", test, "_", treatment, "_SQ.csv", sep = ""))
    #saveRDS(DT.SQ, paste("Data/Output/", strategy, "_", test, "_", treatment, "_SQ.rds", sep = ""))
    rm(DT.SQ)

}

# Used to read each type of *.csv file
Readdata <- function(fn) {
    dt_temp <- fread(paste("Data/Output/",fn,sep = ""), sep = ",")

    dt_temp

}

ReadStrategy2 <- function(fn) {

    dt_temp <- readRDS(paste("Data/Output/", fn, sep = ""))

    dt_temp

}

combineS2files <- function(fn) {

    all.S2.files <- list.files(path = "Data/Output", pattern = fn)
    mylist <- lapply(all.S2.files, ReadStrategy2)
    mylist[[2]] <- mylist[[2]][, cycle := cycle + 1]
    mylist[[3]] <- mylist[[3]][, cycle := cycle + 2]
    mylist[[4]] <- mylist[[4]][, cycle := cycle + 3]
    mylist[[5]] <- mylist[[5]][, cycle := cycle + 4]
    mylist[[6]] <- mylist[[6]][, cycle := cycle + 5]
    mylist[[7]] <- mylist[[7]][, cycle := cycle + 6]
    mylist[[8]] <- mylist[[8]][, cycle := cycle + 7]
    mylist[[9]] <- mylist[[9]][, cycle := cycle + 8]
    mylist[[10]] <- mylist[[10]][, cycle := cycle + 9]
    tempDT <- rbindlist(mylist, fill = TRUE)
    saveRDS(tempDT, paste("Data/Output/", fn, ".rds", sep = ""))

}






# Converts a rate into a probability
RateToProb <- function(r, to = 1, per = 1) {
    stopifnot(
    r >= 0,
    to > 0,
    per > 0
  )
    r <- r / per
    1 - exp(-r * to)
}


#transMatrix4R <- DefineTransition(
#CMP, param$POP * (1 - param$TESTSP) * param$TREATR, param$POP * (1 - param$TESTSP) * (1 - param$TREATR), 0, param$POP * param$TESTSP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, param$MR,
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
