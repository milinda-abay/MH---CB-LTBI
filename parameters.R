# Winter parameters

# Look up the mortality rate from vic.mortality
get_mr <- function(xDT, year, rate_assumption = "Med") {

    DT <- copy(xDT[, .(AGEP, SEXP)])

    # To lookup all ages beyond 110
    DT[AGEP > 110, AGEP := 110]

    vic.mortality[Year == year & mrate == rate_assumption][DT, Prob, on = .(Age = AGEP, Sex = SEXP)]

}

# Look up the Reactivation rate
get_rr <- function(xDT, year) {

    DT <- copy(xDT[, .(AGERP, SEXP, YARP)])

    DT[AGERP > 110, AGERP := 110]

    RRates[DT[, .(AGERP, SEXP, ST = year - YARP)], Rate, on = .(Age = AGERP, Sex = SEXP, statetime = ST)]

}

# Look up TB mortality rate
get_tbmr <- function(xDT, year) {

    DT <- copy(xDT[, .(AGEP, SEXP)])

    # To lookup all ages beyond 95 & 97
    DT[AGEP > 95 & SEXP == "Male", AGEP := 95]
    DT[AGEP > 97 & SEXP == "Female", AGEP := 97]

    vic.tb.mortality[DT[, .(AGEP, SEXP)], Prob, on = .(age = AGEP, sex = SEXP)]

}

# Look up test sensitivity / specificity 
get_test <- function(S, testing) {

    as.numeric(tests_dt[tests == testing, ..S])

}

# Look up treatment completion rate
get_treat <- function(S, treat) {

    as.numeric(treatment_dt[treatment == treat, ..S])

}

# Look up target population percentage
get_pop <- function(DT, strategy, markov_cycle) {

    my_name <- lazy_eval(strategy$properties$my_name)

    if ((my_name == "S1" || my_name == "S0_1") && markov_cycle <= 5) {

        1

    } else if (my_name == "S2" && markov_cycle <= 5) {

        ifelse(DT[, YARP] == 2020 + markov_cycle, .684, 0)

    } else if (my_name == "S0_12" || my_name == "S0_345") {

        # not needed baseline transition matrix takes care of it but...
        0

    } else if ((my_name == "S3" || my_name == "S4" || my_name == "S5") &&
               markov_cycle <= 5) {

        
        # leaves S3, S4 & S5
        ifelse(DT$YARP < 2020,
            switch(my_name,
                S3 = 0.05,
                S4 = 0.10,
                S5 = 0.15,
                stop("Error in Pop")
            ),
            .684 # stop("Error in Pop DT YARP")
        )

    } else {

        0
    }

}

get_utility <- function(t) {

    as.numeric(utility_dt[treatment == t][, 2:21])

}

get_discount <- function() {

    .03

}

# Create a sample data table of test sensitivity & specificity
tests_dt <- data.table(tests = c("QTFGIT", "TST05", "TST10", "TST15"), SN = c(0.76, 0.74, 0.72, 0.4),
                    SP = c(0.97, 0.56, 0.58, 0.87), cost.primary = c(79.75, 67.10, 67.10, 67.10),
                    cost.tertiary = c(122.71, 164.5, 164.5, 164.5))


# Create a sample treatment data table
treatment_dt <- data.table(treatment = c("4R", "9H", "3HP", "6H"),
                        rate = c(.83, .78, .82, .63),
                        cost.primary = c(437.13, 578.87, 440.34, 436.42),
                        cost.tertiary = c(632.38, 969.37, 596.54, 709.77),
                        sae = c(0.000000009, 0.00000025, 0.00000016, 0.0000002))


#9H cost changed from 549.22 to 578.87 and 939.72 to 969.37 respectively. 

# Create a sample utility data table
# TODO: fix hard coded data table. It should take state_names and create the columns.
utility_dt <- data.table(treatment = c("", "4R", "9H", "3HP", "6H"))
utility_dt[, c(state_names) := as.numeric(NA)]


utility_dt[treatment == "6H", c(state_names) := .(1, 0.9995, 1, 1, 1, 1, 0.9995,
                            1, 1, 0.75, 0.94, 1, 0.75, 0.94, 0.75,
                            0.94, 0, 0, 0, 0)]

utility_dt[treatment == "9H", c(state_names) := .(1, 0.999375, 1, 1, 1, 1, 0.999375,
                            1, 1, 0.75, 0.94, 1, 0.75, 0.94, 0.75,
                            0.94, 0, 0, 0, 0)]

utility_dt[treatment == "4R", c(state_names) := .(1, 0.999775, 1, 1, 1, 1, 0.999775,
                            1, 1, 0.75, 0.94, 1, 0.75, 0.94, 0.75,
                            0.94, 0, 0, 0, 0)]

utility_dt[treatment == "3HP", c(state_names) := .(1, 0.9996, 1, 1, 1, 1, 0.9996,
                            1, 1, 0.75, 0.94, 1, 0.75, 0.94, 0.75,
                            0.94, 0, 0, 0, 0)]

utility_dt[treatment == "", c(state_names) := .(1, NA, NA, NA, NA, 1, NA, NA, NA,
                            NA, NA, NA, NA, NA, 0.75, 0.94, NA, NA, 0, 0)]

unevaluated_flow_cost <- lazy(c(1:20))
unevaluated_state_cost <- lazy(c(1:20))

# Creates a master migrant population table
create_population_master <- function(modify = FALSE) {

    # Create a winter_input_dt table merging census (2006,2011, 2016) and ABS projection data.
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

    winter_input_dt <- aust.vic[, .(AGEP, ISO3, YARP, NUMP, LTBP, AGERP = AGEP - (2016L - YARP), SEXP)]

    # Also creating migrant cohort arrivals for YARP > 2016. i.e. 2017 to 2025.
    # again this is for validating the model at runtime.

    # deleted 2016 due to it being a census year with 1/2 half.
    winter_input_dt <- winter_input_dt[YARP != 2016]


    # Create new arrival cohorts 
    winter_input_dt.2016 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2016L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2017 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2017L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2018 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2018L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2019 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2019L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2020 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2020L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2021 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2021L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2022 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2022L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2023 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2023L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2024 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2024L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2025 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2025L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2026 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2026L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2027 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2027L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2028 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2028L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2029 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2029L, NUMP, LTBP, AGERP, SEXP),]
    winter_input_dt.2030 <- winter_input_dt[YARP == 2015, .(AGEP = AGEP - 1, ISO3, YARP = 2030L, NUMP, LTBP, AGERP, SEXP),]


    winter_input_dt <- rbind(winter_input_dt, winter_input_dt.2016, winter_input_dt.2017, winter_input_dt.2018, winter_input_dt.2019,
                        winter_input_dt.2020, winter_input_dt.2021, winter_input_dt.2022, winter_input_dt.2023,
                        winter_input_dt.2024, winter_input_dt.2025, winter_input_dt.2026, winter_input_dt.2027,
                        winter_input_dt.2028, winter_input_dt.2029, winter_input_dt.2030)

    rm(winter_input_dt.2016, winter_input_dt.2017, winter_input_dt.2018, winter_input_dt.2019, winter_input_dt.2020, winter_input_dt.2021,
       winter_input_dt.2022, winter_input_dt.2023, winter_input_dt.2024, winter_input_dt.2025, winter_input_dt.2026,
       winter_input_dt.2027, winter_input_dt.2028, winter_input_dt.2029, winter_input_dt.2030)


    # Must order the winter_input_dt table by YARP due to sub-setting and recombining. 
    setkey(winter_input_dt, YARP, SEXP, AGEP, ISO3)

    # Calculate the susceptible and latent population

    winter_input_dt <- winter_input_dt[, cycle := as.integer(NA)]

    # TODO - Fix this! It is hard coded for 20 states.
    winter_input_dt <- winter_input_dt[, (state_names) := .(NUMP - LTBP, 0, 0, 0, 0, LTBP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)]

    # Because we are running the model from 2020 the retrospective cohort must be aged from 2016 to 2020
    winter_input_dt[YARP <= 2016, AGEP := AGEP + 4] # Census was taken in 2016
    winter_input_dt[YARP == 2017, AGEP := AGEP + 3]
    winter_input_dt[YARP == 2018, AGEP := AGEP + 2]
    winter_input_dt[YARP == 2019, AGEP := AGEP + 1]


    winter_input_dt

}

load_data_files <- function() {
    # Loads any data files into the environment

    # Args: 
    #   None

    # Return:
    #   No returns, loads data files to .GlobalEnv

    assign("aust.vic", readRDS("Data/aust.vic.rds"), 1)
    assign("vic.mortality", readRDS("Data/vic.mortality.rds"), 1)
    assign("RRates", readRDS("Data/RRates.rds"), 1)
    assign("vic.tb.mortality", readRDS("Data/vic.tb.mortality.rds"), 1)


    # aust <- readRDS("Data/aust.rds")
    # aust.vic.LGA <- readRDS("Data/aust.vic.LGA.rds") # this is for S0
    # prob.Inf <- readRDS("Data/prob.Inf.rds") 
    # tbhaz.200rep <- readRDS("Data/tbhaz.200rep.rds")
    # tbhaz.5000rep <- readRDS("Data/tbhaz.5000rep.rds")
    # vic.fertility <- readRDS("Data/vic.fertility.rds")
    # vic.migration <- readRDS("Data/vic.migration.rds")
    # vic.pop <- readRDS("Data/vic.pop.rds")


}
