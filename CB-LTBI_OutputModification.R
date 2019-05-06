# Managing output files

CreateOutput("S0", "No Test", "No Treatment")


# find and replace S# as required. e.g S5 to S1

CreateOutput("S5", "QTFGIT", "4R")
CreateOutput("S5", "TST05", "4R")
CreateOutput("S5", "TST10", "4R")
CreateOutput("S5", "TST15", "4R")
CreateOutput("S5", "QTFGIT", "3HP")
CreateOutput("S5", "TST05", "3HP")
CreateOutput("S5", "TST10", "3HP")
CreateOutput("S5", "TST15", "3HP")
CreateOutput("S5", "QTFGIT", "9H")
CreateOutput("S5", "TST05", "9H")
CreateOutput("S5", "TST10", "9H")
CreateOutput("S5", "TST15", "9H")
CreateOutput("S5", "QTFGIT", "6H")
CreateOutput("S5", "TST05", "6H")
CreateOutput("S5", "TST10", "6H")
CreateOutput("S5", "TST15", "6H")

#  To combine S2 files 
filenames <- list.files(path = "Data/Output", pattern = "S2")
filenames <- unique(substr(filenames, 6, 30))
filenames <- gsub(".rds", "", filenames)

lapply(filenames, combineS2files)




# Create output files for PowerBI i.e splitting each output into four files

all.files <- list.files(path = "Data/Output", pattern = "M.csv")
mylist <- lapply(all.files, Readdata)
Master <- rbindlist(mylist, fill = TRUE)
rm(mylist)
fwrite(Master, "Data/Master.csv")
rm(Master)

all.files <- list.files(path = "Data/Output", pattern = "S.csv")
mylist <- lapply(all.files, Readdata)
StateCount <- rbindlist(mylist, fill = TRUE)
rm(mylist)
fwrite(StateCount, "Data/StateCount.csv")
rm(StateCount)

all.files <- list.files(path = "Data/Output", pattern = "SC.csv")
mylist <- lapply(all.files, Readdata)
StateCost <- rbindlist(mylist, fill = TRUE)
rm(mylist)
fwrite(StateCost, "Data/StateCost.csv")
rm(StateCost)

all.files <- list.files(path = "Data/Output", pattern = "F.csv")
mylist <- lapply(all.files, Readdata)
FlowCount <- rbindlist(mylist, fill = TRUE)
rm(mylist)
fwrite(FlowCount, "Data/FlowCount.csv")
rm(FlowCount)

all.files <- list.files(path = "Data/Output", pattern = "FC.csv")
mylist <- lapply(all.files, Readdata)
FlowCost <- rbindlist(mylist, fill = TRUE)
rm(mylist)
fwrite(FlowCost, "Data/FlowCost.csv")
rm(FlowCost)

all.files <- list.files(path = "Data/Output", pattern = "SQ.csv")
mylist <- lapply(all.files, Readdata)
StateQALY <- rbindlist(mylist, fill = TRUE)
rm(mylist)
fwrite(StateQALY, "Data/StateQALY.csv")
rm(StateQALY)

