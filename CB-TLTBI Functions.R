

# Time varying transition probability for ltbi -> tb
# dependent on state time (currently not used). Can be called from within the transition matrix.
ltbi2tb <- function(state_time) {

    if (state_time <= 2) {
        return(rate_to_prob(r = 2.5, per = 100, to = 1))
    }
        
    else {
        return(rate_to_prob(r = 2.5 * (.99 * state_time), per = 100, to = 1))
    }
}


# Gets the inflow values from a data.table (currently not used)
# Just a proof of concept

get.inflow <- function(mc) {

    print(mc)
    mc <- 2016
    
    print(aust.LGA[YARP == mc, sum(NUMP)])
    return(aust.LGA[YARP == mc,sum(NUMP)])


}


get.p.sus <- function(year = 0){

    # this is the total number of uninfected migrants that arrived in 2016
    return(migrants.2016[,sum(num.sus)])

}


get.p.ltbi <- function(year = 0){

    # this is the total number of infected migrants that arrived in 2016
    return(migrants.2016[, sum(num.ltbi)])

}



get.p.ltbi

# For Heterogeneity & Demographic Analysis used in update()
test.data <- vic.mortality[Age < 80 & mrate == "Med" & Rate != 1, .(Age.init = Age, Sex, mrate), by = .(Age, Sex, mrate)]
test.data <- test.data[, 4:6]


vic.mortality[Age < 80,.(Age)]


# Gets the mortality rate from vic.mortality

get.mortality.rate <- function(S, A, Y, R) {


    temp.DT <-  data.table (S, A, Y, R)

    # print(vic.mortality[temp.DT, Rate, on = c( Sex="S", Age="A", Year="Y", mrate="R")])
    print(vic.mortality[temp.DT, .(Age, Sex, Year, Rate, mrate), on = c(Sex = "S", Age = "A", Year = "Y", mrate = "R")])
    return(vic.mortality[temp.DT, Rate, on = c(Sex = "S", Age = "A", Year = "Y", mrate = "R")])
    
}

# Setup costs for drugs

cost.isoniazid <- 10
cost.rifampicin <- 20
cost.rifapentine <- 30


# Markov model setup

rr <- .5 # relative risk of treatment with all drugs

# Define parameters
param <- define_parameters(

Sex = "Male",
Age.init = 80,
Age = Age.init + markov_cycle,

Year.init = 2017,
Year = Year.init + markov_cycle,

mrate = "High",

mr = get.mortality.rate(Sex, Age, Year, mrate)

)


# Define states

p.sus <- define_state(
  cost_healthcare = 0,
  cost_drugs = 0,
  cost_total = cost_healthcare + cost_drugs,
  utility = 1
  )


p.ltbi <- define_state(
  cost_healthcare = discount(1234, 0.6),
  cost_drugs = discount(dispatch_strategy(
  strategy.9H = cost.isoniazid,
  strategy.everything = cost.isoniazid + cost.rifampicin + cost.rifapentine,
  strategy.nothing = 0), 0.06),
  cost_total = cost_healthcare + cost_drugs,
  utility = 0.85
  
  )

p.tb <- define_state(
  cost_healthcare = discount(2345, 0.6),
  cost_drugs = discount(dispatch_strategy(
  strategy.9H = cost.isoniazid,
  strategy.everything = cost.isoniazid + cost.rifampicin + cost.rifapentine,
  strategy.nothing = 0), 0.06),
  cost_total = cost_healthcare + cost_drugs,
  utility = 0.4
  )

p.death <- define_state(
  cost_healthcare = 0,
  cost_drugs = 0,
  cost_total = 0,
  utility = 0
  )

# Define transition matrix

tmatrix.nothing <- define_transition(
  state_names = c("p.sus", "p.ltbi", "p.tb", "p.death"),
  C, 0, 0, mr,
  0, C, 0.05 / 20, mr,
  0, 0, C, 0.8,
  0, 0, 0, 1
)


tmatrix.9H <- define_transition(
  state_names = c("p.sus","p.ltbi","p.tb", "p.death"),
  C, 0, 0, mr,
  0, C, 0.02 / 20, mr,
  0, 0, C, 0.8,
  0, 0, 0, 1
)

tmatrix.everything <- define_transition(
  state_names = c("p.sus", "p.ltbi", "p.tb", "p.death"),
  C, 0, 0, mr * rr,
  0, C, (0.05 / 20) * rr, mr * rr,
  0, 0, C, 0.8*rr,
  0, 0, 0, 1
)


strat.9H <- define_strategy(
  transition = tmatrix.9H,
  p.sus = p.sus,
  p.ltbi = p.ltbi,
  p.tb = p.tb,
  p.death = p.death
  
  )

strat.everything <- define_strategy(
  transition = tmatrix.everything,
  p.sus = p.sus,
  p.ltbi = p.ltbi,
  p.tb = p.tb,
  p.death = p.death
  
  )

strat.nothing <- define_strategy(
  transition = tmatrix.nothing,
  p.sus = p.sus,
  p.ltbi = p.ltbi,
  p.tb = p.tb,
  p.death = p.death

  )

