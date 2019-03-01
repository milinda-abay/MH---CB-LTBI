

# Creates a master migrant population table
Create.Population.Master <- function() {

    # Create a pop.master table merging census (2006,2011, 2016) and ABS projection data.
    # It must be a long format table with the following structure.
    #
    # Sex of person (SEXP), Age at arrival (AGEP), Year of arrival (YARP),
    # Birth place of person (ISO3), local government area (LGA),
    # Number of persons	(NUMP)
    #__________________________________________________
    # SEXP    | AGEP  |  YARP | ISO3 |  LGA    | NUMP |
    #--------------------------------------------------
    # Male	  | 10	  |  2006 | AFG	 |  Casey  | 4	  |  NUMP =  { average of 3 census (2006,2011,2016) datasets } 
    # Female  |	12	  |  2007 |	IND	 |  Monash | 10	  |  NUMP =  { average of 2 census (2011,2016) datasets } 
    # …	      | …	  |  …	  |  …	 |  …	   | …	  |  
    # Male	  | 30	  |  2016 |	VNM	 |  Hume   | 7	  |  NUMP =  { census 2016 datasets } 
    # …	      | …	  |  …	  |  …	 |  …	   | …	  |
    # ------------------2017---------------------------	No data for YARP 2017
    # Male	  | 50	  |  2018 |	?	 |  ?	   | 324  |	ABS migration projection with three assumptions (high, med & low ) by arrivals and departures
    # Female  |	40	  |  2027 |	?	 |  ?	   | 721  |	Net overseas migration levels will remain constant from YARP>2027 onwards
    #						
    # TODO -> Based on census datasets (2006,2011,2016) estimate a NUMP distribution for YARP > 2018  by LGA and ISO3.						
    #
    # As a validation exercise the aust.LGA cohort is duplicated into male & female and LGA aggregated
    # This done to validate the model runtime. It must be fixed!

    pop.master.male <- aust.LGA[, .(NUMP = sum(NUMP), LTBP = sum(LTBP), SEXP = "Male"), by = c("AGEP", "ISO3", "YARP")]
    pop.master.female <- pop.master.male[, .(AGEP, ISO3, YARP, NUMP, LTBP, SEXP = "Female")]

    pop.master <- rbind(pop.master.male, pop.master.female)
    rm(pop.master.female, pop.master.male)

    # Also creating migrant cohort arrivals for YARP > 2016. i.e. 2017, 2018, 2019.
    # again this is for validating the model at runtime.

    pop.master.2017 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2017, NUMP, LTBP, SEXP),]
    pop.master.2018 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2018, NUMP, LTBP, SEXP),]
    pop.master.2019 <- pop.master[YARP == 2015, .(AGEP, ISO3, YARP = 2019, NUMP, LTBP, SEXP),]

    pop.master <- rbind(pop.master, pop.master.2017, pop.master.2018, pop.master.2019)
    rm(pop.master.2017, pop.master.2018, pop.master.2019)

    # Must order the pop.master table by YARP due to subsetting and recombining. 
    setkey(pop.master, YARP, SEXP, AGEP, ISO3)

    # Remove australian born and calculates the susceptible and latent population
    pop.master <- pop.master[ISO3 != "AUS"][, (state.names) := .(NUMP - LTBP, 0, 0, 0, 0,LTBP, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)]

    
}

# Define state names
state.names <-  c("p.sus", "p.sus.fp", "p.sus.fp.t", "p.sus.fp.tc",	"p.sus.tn",	"p.ltbi",
                 "p.ltbi.tp", "p.ltbi.tp.t", "p.ltbi.tp.tc", "p.ltbi.tp.tc.tb",	"p.ltbi.tp.tc.tbr",
                 "p.ltbi.fn", "p.ltbi.fn.tb", "p.ltbi.fn.tbr", "p.death", "p.tb.death")

# Create a vector of transition probabilities
transition.probabilities <- c(0.89, 0.01, 0, 0, 0.09, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0,
                              0, 0, 0.99, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0,
                              0, 0, 0, 0.99, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0,
                              0, 0, 0, 0.99, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0,
                              0, 0, 0, 0, 0.99, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0,
                              0, 0, 0, 0, 0, 0.09, 0.8, 0, 0, 0, 0, 0.1, 0, 0, 0.01, 0,
                              0, 0, 0, 0, 0, 0, 0, 0.99, 0, 0, 0, 0, 0, 0, 0.01, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0.99, 0, 0, 0, 0, 0, 0.01, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0.19, 0.8, 0, 0, 0, 0, 0.01, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.79, 0, 0, 0, 0.01, 0.2,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.99, 0, 0, 0, 0.01, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.29, 0.7, 0, 0.01, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.79, 0.01, 0.2,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.99, 0.01, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

# Create a transition matrix

transistion.matrix <- matrix(data = transition.probabilities, nrow = 16, ncol = 16, byrow = T, dimnames = list(state.names))
colnames(transistion.matrix) <- state.names



Run.Model <- function() {

    # initialise the calculation frame
    
    pop.calculated <- pop.output

    
    while (markov.cycle != cycles) {

        print(nrow(pop.calculated))
        print(pop.calculated[1:10,.N, by =.(AGEP,cycle)])
        # Creates the initial population matrix
        pop.matrix <- as.matrix(pop.calculated[, ..state.names])
        
        # Creating a data.table to hold the output 
        pop.calculated <- pop.calculated[, .(AGEP, ISO3, YARP, NUMP, LTBP, SEXP, cycle)]

        # performing the matrix multiplication and binding the values
        pop.calculated <- cbind(pop.calculated, pop.matrix %*% transistion.matrix)

        # Update counters
        markov.cycle <- markov.cycle + 1
        year <- year + 1


        # Inflows for next cycle
        pop.inflow <- pop.master[YARP == year,][, cycle := NA]

        # Updating population calculation frame
        pop.calculated[, AGEP := AGEP + 1]
        pop.calculated <- rbind(pop.calculated, pop.inflow)
        pop.calculated[, cycle := markov.cycle]

        # Saving state in pop.output
        pop.output <<- rbind(pop.output, pop.calculated)

     
    }
    
}


