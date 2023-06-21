# This script will analyse RT data from the data folder
# T.Eerola, 22/4/2023

#### Data ----------------
AUG <- read.csv('data/MAJ_AUG_F.csv')
print(paste0("AUG: N=",length(unique(AUG$Id)))) # 102
DIM <- read.csv('data/MAJ_DIM_F.csv')
print(paste0("DIM: N=",length(unique(DIM$Id)))) # 99
MIN <- read.csv('data/MAJ_MIN_F.csv')
print(paste0("MIN: N=",length(unique(MIN$Id)))) # 110
SUS <- read.csv('data/MAJ_SUS_F.csv')
print(paste0("SUS: N=",length(unique(SUS$Id)))) # 101
