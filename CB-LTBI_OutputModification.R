

# Managing output files
S0 <- readRDS("Data/Output/S0.rds")
CreateOutput(S0, "S0", "No Test", "No Treatment")

S5.QTFGIT.4R <- readRDS("Data/Output/S5.QTFGIT.4R.rds")
CreateOutput(S5.QTFGIT.4R, "S5", "QTFGIT", "4R")
rm(S5.QTFGIT.4R)

S5.TST5.4R <- readRDS("Data/Output/S5.TST5.4R.rds")
CreateOutput(S5.TST5.4R, "S5", "TST05", "4R")
rm(S5.TST5.4R)

S5.TST10.4R <- readRDS("Data/Output/S5.TST10.4R.rds")
CreateOutput(S5.TST10.4R, "S5", "TST10", "4R")
rm(S5.TST10.4R)

S5.TST15.4R <- readRDS("Data/Output/S5.TST15.4R.rds")
CreateOutput(S5.TST15.4R, "S5", "TST15", "4R")
rm(S5.TST15.4R)

S5.QTFGIT.3HP <- readRDS("Data/Output/S5.QTFGIT.3HP.rds")
CreateOutput(S5.QTFGIT.3HP, "S5", "QTFGIT", "3HP")
rm(S5.QTFGIT.3HP)


S5.TST5.3HP <- readRDS("Data/Output/S5.TST5.3HP.rds")
CreateOutput(S5.TST5.3HP, "S5", "TST05", "3HP")
rm(S5.TST5.3HP)

S5.TST10.3HP <- readRDS("Data/Output/S5.TST10.3HP.rds")
CreateOutput(S5.TST10.3HP, "S5", "TST10", "3HP")
rm(S5.TST10.3HP)

S5.TST15.3HP <- readRDS("Data/Output/S5.TST15.3HP.rds")
CreateOutput(S5.TST15.3HP, "S5", "TST15", "3HP")
rm(S5.TST15.3HP)


S5.QTFGIT.9H <- readRDS("Data/Output/S5.QTFGIT.9H.rds")
CreateOutput(S5.QTFGIT.9H, "S5", "QTFGIT", "9H")
rm(S5.QTFGIT.9H)

S5.TST5.9H <- readRDS("Data/Output/S5.TST5.9H.rds")
CreateOutput(S5.TST5.9H, "S5", "TST05", "9H")
rm(S5.TST5.9H)

S5.TST10.9H <- readRDS("Data/Output/S5.TST10.9H.rds")
CreateOutput(S5.TST10.9H, "S5", "TST10", "9H")
rm(S5.TST10.9H)

S5.TST15.9H <- readRDS("Data/Output/S5.TST15.9H.rds")
CreateOutput(S5.TST15.9H, "S5", "TST15", "9H")
rm(S5.TST15.9H)

S5.QTFGIT.6H <- readRDS("Data/Output/S5.QTFGIT.6H.rds")
CreateOutput(S5.QTFGIT.6H, "S5", "QTFGIT", "6H")
rm(S5.QTFGIT.6H)

S5.TST5.6H <- readRDS("Data/Output/S5.TST5.6H.rds")
CreateOutput(S5.TST5.6H, "S5", "TST05", "6H")
rm(S5.TST5.6H)

S5.TST10.6H <- readRDS("Data/Output/S5.TST10.6H.rds")
CreateOutput(S5.TST10.6H, "S5", "TST10", "6H")
rm(S5.TST10.6H)

S5.TST15.6H <- readRDS("Data/Output/S5.TST15.6H.rds")
CreateOutput(S5.TST15.6H, "S5", "TST15", "6H")
rm(S5.TST15.6H)



# Create output files for PowerBI i.e splitting each output into four files

all.files <- list.files(path = "Data/Output", pattern = "S.csv")
mylist <- lapply(all.files, Readdata)
StateCount <- rbindlist(mylist, fill = TRUE)
fwrite(StateCount,"Data/StateCount.csv")
rm(StateCount)

all.files <- list.files(path = "Data/Output", pattern = "SC.csv")
mylist <- lapply(all.files, Readdata)
StateCost <- rbindlist(mylist, fill = TRUE)
fwrite(StateCost, "Data/StateCost.csv")
rm(StateCost)

all.files <- list.files(path = "Data/Output", pattern = "F.csv")
mylist <- lapply(all.files, Readdata)
FlowCount <- rbindlist(mylist, fill = TRUE)
fwrite(FlowCount, "Data/FlowCount.csv")
rm(FlowCount)

all.files <- list.files(path = "Data/Output", pattern = "FC.csv")
mylist <- lapply(all.files, Readdata)
FlowCost <- rbindlist(mylist, fill = TRUE)
fwrite(FlowCost, "Data/FlowCost.csv")
rm(FlowCost)

all.files <- list.files(path = "Data/Output", pattern = "SQ.csv")
mylist <- lapply(all.files, Readdata)
StateQALY <- rbindlist(mylist, fill = TRUE)
fwrite(StateQALY, "Data/StateQALY.csv")
rm(StateQALY)













