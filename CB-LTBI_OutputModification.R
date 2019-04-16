

# Managing output files
S0 <- readRDS("Data/Output/S0.rds")
CreateOutput(S0, "S0", "No Test", "No Treatment")

S5.QTFGIT.4R <- readRDS("Data/Output/S5.QTFGIT.4R.rds")
S5.TST5.4R <- readRDS("Data/Output/S5.TST5.4R.rds")
S5.TST10.4R <- readRDS("Data/Output/S5.TST10.4R.rds")
S5.TST15.4R <- readRDS("Data/Output/S5.TST15.4R.rds")
S5.QTFGIT.3HP <- readRDS("Data/Output/S5.QTFGIT.3HP.rds")
S5.TST5.3HP <- readRDS("Data/Output/S5.TST5.3HP.rds")
S5.TST10.3HP <- readRDS("Data/Output/S5.TST10.3HP.rds")
S5.TST15.3HP <- readRDS("Data/Output/S5.TST15.3HP.rds")
S5.QTFGIT.9H <- readRDS("Data/Output/S5.QTFGIT.9H.rds")
S5.TST5.9H <- readRDS("Data/Output/S5.TST5.9H.rds")
S5.TST10.9H <- readRDS("Data/Output/S5.TST10.9H.rds")
S5.TST15.9H <- readRDS("Data/Output/S5.TST15.9H.rds")
S5.QTFGIT.6H <- readRDS("Data/Output/S5.QTFGIT.6H.rds")
S5.TST5.6H <- readRDS("Data/Output/S5.TST5.6H.rds")
S5.TST10.6H <- readRDS("Data/Output/S5.TST10.6H.rds")
S5.TST15.6H <- readRDS("Data/Output/S5.TST15.6H.rds")


# Create output files for PowerBI i.e splitting each output into four files

CreateOutput(S5.QTFGIT.4R, "S5", "QTFGIT", "4R")
CreateOutput(S5.TST5.4R, "S5", "TST05", "4R")
CreateOutput(S5.TST10.4R, "S5", "TST10", "4R")
CreateOutput(S5.TST15.4R, "S5", "TST15", "4R")
CreateOutput(S5.QTFGIT.3HP, "S5", "QTFGIT", "3HP")
CreateOutput(S5.TST5.3HP, "S5", "TST05", "3HP")
CreateOutput(S5.TST10.3HP, "S5", "TST10", "3HP")
CreateOutput(S5.TST15.3HP, "S5", "TST15", "3HP")
CreateOutput(S5.QTFGIT.6H, "S5", "QTFGIT", "6H")
CreateOutput(S5.TST5.6H, "S5", "TST05", "6H")
CreateOutput(S5.TST10.6H, "S5", "TST10", "6H")
CreateOutput(S5.TST15.6H, "S5", "TST15", "6H")
CreateOutput(S5.QTFGIT.9H, "S5", "QTFGIT", "9H")
CreateOutput(S5.TST5.9H, "S5", "TST05", "9H")
CreateOutput(S5.TST10.9H, "S5", "TST10", "9H")
CreateOutput(S5.TST15.9H, "S5", "TST15", "9H")

filenames <- list.files("Data/Output", "S.csv",)

filenames





StateCount <- rbind(readRDS("Data/BO_No Test_4R_S.rds"), readRDS("Data/S1_TST10_4R_S.rds"),
                    readRDS("Data/S1_TST15_4R_S.rds"), readRDS("Data/S1_QTFGIT_4R_S.rds"),
                    readRDS("Data/S2_TST10_4R_S.rds"), readRDS("Data/S2_TST15_4R_S.rds"),
                    readRDS("Data/S2_QTFGIT_4R_S.rds"))

saveRDS(StateCount, "Data/StateCount.rds")


FlowCount <- rbind(readRDS("Data/BO_No Test_4R_F.rds"), readRDS("Data/S1_TST10_4R_F.rds"),
                    readRDS("Data/S1_TST15_4R_F.rds"), readRDS("Data/S1_QTFGIT_4R_F.rds"),
                    readRDS("Data/S2_TST10_4R_F.rds"), readRDS("Data/S2_TST15_4R_F.rds"),
                    readRDS("Data/S2_QTFGIT_4R_F.rds"))

saveRDS(FlowCount, "Data/FlowCount.rds")

StateCost <- rbind(readRDS("Data/BO_No Test_4R_SC.rds"), readRDS("Data/S1_TST10_4R_SC.rds"),
                    readRDS("Data/S1_TST15_4R_SC.rds"), readRDS("Data/S1_QTFGIT_4R_SC.rds"),
                    readRDS("Data/S2_TST10_4R_SC.rds"), readRDS("Data/S2_TST15_4R_SC.rds"),
                    readRDS("Data/S2_QTFGIT_4R_SC.rds"))

saveRDS(StateCost, "Data/StateCost.rds")


FlowCost <- rbind(readRDS("Data/BO_No Test_4R_FC.rds"), readRDS("Data/S1_TST10_4R_FC.rds"),
                    readRDS("Data/S1_TST15_4R_FC.rds"), readRDS("Data/S1_QTFGIT_4R_FC.rds"),
                    readRDS("Data/S2_TST10_4R_FC.rds"), readRDS("Data/S2_TST15_4R_FC.rds"),
                    readRDS("Data/S2_QTFGIT_4R_FC.rds"))

saveRDS(FlowCost, "Data/FlowCost.rds")
