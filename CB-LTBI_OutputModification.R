

# find and replace S# as required. e.g S5 to S1

x <- getwd()

# output RDS files on external hard disk
setwd("D:/")


CreateOutput("S0_12")
CreateOutput("S0_345")
CreateOutput("S1")
CreateOutput("S3")
CreateOutput("S4")
CreateOutput("S5")

# Create output files for PowerBI i.e splitting each output into four files

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

