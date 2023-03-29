# Allocation of CRFS Rockfish Genus to Rockfish Species
# Julia Coates, February 2023

library(dplyr)
library(tidyr)
library(ggplot2)
setwd("C:/Users/jcoates/OneDrive - California Department of Fish and Wildlife/Groundfish")

allrock <- read.csv("RecFIN_rockfish_catch_18_22.csv") # Queried rockfish species, all modes and California areas.  
allrockpc <- filter(allrock, RECFIN_MODE_NAME=="PARTY/CHARTER BOATS")
allrockpr <- filter(allrock, RECFIN_MODE_NAME!="PARTY/CHARTER BOATS")

pcbyyr <- allrockpc %>% group_by(RECFIN_YEAR, SPECIES_NAME) %>% summarize(sumcatchnum=sum(TOTAL_MORTALITY_NUM))
ggplot(pcbyyr, aes(fill=SPECIES_NAME, y=sumcatchnum, x=RECFIN_YEAR)) + 
  geom_bar(position="stack", stat="identity") + labs(title="PC 2020")

prbyyr <- allrockpr %>% group_by(RECFIN_YEAR, SPECIES_NAME) %>% summarize(sumcatchnum=sum(TOTAL_MORTALITY_NUM))
ggplot(prbyyr, aes(fill=SPECIES_NAME, y=sumcatchnum, x=RECFIN_YEAR)) + 
  geom_bar(position="stack", stat="identity") + labs(title="PR 2020")

#================================================================================================
# Create a matrix of allocated values for PC statewide

pc <- spread(pcbyyr, RECFIN_YEAR, sumcatchnum)
colnames(pc) <- c("species", "x2018", "x2019", "x2020", "x2021", "x2022")
pc[,2:6] <- sapply(pc[,2:6], as.numeric)
pc[is.na(pc)] <- 0
pc$ave1819 <- (pc$x2018 + pc$x2019)/2
sumave1819 <- sum(pc$ave1819)  # The total average catch in 2018-19 is 2376219
pc$ave1819b <- pc$ave1819  # Copy this average to a new column
pc$ave1819b[29] <- 0  # Remove rockfish genus catch from this new column
sumave1819b <- sum(pc$ave1819b)  # The total average catch in 2018-19 without rockfish genus is 2171833
pc$prop1819 <- pc$ave1819 / sumave1819  # Calculate the proportion of catch for each species to the total
pc$prop1819b <- pc$ave1819b / sumave1819b  # Calculate the proportion of catch for each species to the total without rockfish genus
sum2020 <- sum(pc$x2020) # Get the total catch in 2020 including rockfish genus
exp_rg_2020 <- pc$prop1819[29] * sum2020  # This is the expected value for rockfish genus catch in 2020
to_allocate <- pc$x2020[29] - exp_rg_2020  # This is the amount of rockfish genus catch in 2020 that needs to be allocated
pc$x2020allocated <- pc$x2020 + (pc$prop1819b * to_allocate) # Allocate the extra rockfish genus catch
pc$x2020allocated[29] <- exp_rg_2020  # Replace the rockfish genus catch with the expected value
sum(pc$x2020allocated)  # Check that the new allocated 2020 sum is equal to the original 2020 sum.  It is.  

pc_allocated <- pc[,c(1:3, 11, 5:6)]
pc_allocated <- gather(pc_allocated, year, sumcatchnum, 2:6)
pc_allocated$year[pc_allocated$year=="x2018"] <- 2018
pc_allocated$year[pc_allocated$year=="x2019"] <- 2019
pc_allocated$year[pc_allocated$year=="x2020allocated"] <- 2020
pc_allocated$year[pc_allocated$year=="x2021"] <- 2021
pc_allocated$year[pc_allocated$year=="x2022"] <- 2022
ggplot(pc_allocated, aes(fill=species, y=sumcatchnum, x=year)) + 
  geom_bar(position="stack", stat="identity") + labs(title="PC 2020 Allocated")

#================================================================================================
# Create a matrix of allocated values for PR statewide

pr <- spread(prbyyr, RECFIN_YEAR, sumcatchnum)
colnames(pr) <- c("species", "x2018", "x2019", "x2020", "x2021", "x2022")
pr[,2:6] <- sapply(pr[,2:6], as.numeric)
pr[is.na(pr)] <- 0
pr$ave1819 <- (pr$x2018 + pr$x2019)/2
sumave1819 <- sum(pr$ave1819)  # The total average catch in 2018-19 is 2376219
pr$ave1819b <- pr$ave1819  # Copy this average to a new column
pr$ave1819b[30] <- 0  # Remove rockfish genus catch from this new column
sumave1819b <- sum(pr$ave1819b)  # The total average catch in 2018-19 without rockfish genus is 2171833
pr$prop1819 <- pr$ave1819 / sumave1819  # Calculate the proportion of catch for each species to the total
pr$prop1819b <- pr$ave1819b / sumave1819b  # Calculate the proportion of catch for each species to the total without rockfish genus
sum2020 <- sum(pr$x2020) # Get the total catch in 2020 including rockfish genus
exp_rg_2020 <- pr$prop1819[30] * sum2020  # This is the expected value for rockfish genus catch in 2020
to_allocate <- pr$x2020[30] - exp_rg_2020  # This is the amount of rockfish genus catch in 2020 that needs to be allocated
pr$x2020allocated <- pr$x2020 + (pr$prop1819b * to_allocate) # Allocate the extra rockfish genus catch
pr$x2020allocated[30] <- exp_rg_2020  # Replace the rockfish genus catch with the expected value
sum(pr$x2020allocated)  # Check that the new allocated 2020 sum is equal to the original 2020 sum.  It is.  

pr_allocated <- pr[,c(1:3, 11, 5:6)]
pr_allocated <- gather(pr_allocated, year, sumcatchnum, 2:6)
pr_allocated$year[pr_allocated$year=="x2018"] <- 2018
pr_allocated$year[pr_allocated$year=="x2019"] <- 2019
pr_allocated$year[pr_allocated$year=="x2020allocated"] <- 2020
pr_allocated$year[pr_allocated$year=="x2021"] <- 2021
pr_allocated$year[pr_allocated$year=="x2022"] <- 2022
ggplot(pr_allocated, aes(fill=species, y=sumcatchnum, x=year)) + 
  geom_bar(position="stack", stat="identity") + labs(title="PR 2020 Allocated")

#================================================================================================
# Create a matrix of allocated values by district for PC in 2020

districts <- unique(allrock$DISTRICT_NAME)
districtnum <- c(5,2,3,4,1,6)
districts <- districts[match(districtnum, sort(districtnum))]

pcmat2020 <- matrix(NA, 12, 4)
colnames(pcmat2020) <- c("genus_num", "copper_num", "black_num", "canary_num")
rownames(pcmat2020) <- c("original 1", "original 2", "original 3", "original 4", "original 5", "original 6", "allocated 1", "allocated 2", "allocated 3", "allocated 4", "allocated 5", "allocated 6")

for (i in 1:length(districts)){
  temp <- filter(allrockpc, DISTRICT_NAME==districts[i])
  temp <- temp %>% group_by(RECFIN_YEAR, SPECIES_NAME) %>% summarize(sumcatchnum=sum(TOTAL_MORTALITY_NUM))
  pc <- spread(temp, RECFIN_YEAR, sumcatchnum)
  colnames(pc) <- c("species", "x2018", "x2019", "x2020", "x2021", "x2022")
  pc[,2:6] <- sapply(pc[,2:6], as.numeric)
  pc[is.na(pc)] <- 0
  pc$ave1819 <- (pc$x2018 + pc$x2019)/2
  sumave1819 <- sum(pc$ave1819)  # The total average catch in 2018-19 is 2376219
  pc$ave1819b <- pc$ave1819  # Copy this average to a new column
  pc[pc$species=="ROCKFISH GENUS", "ave1819b"] <- 0 # Remove rockfish genus catch from this new column
  sumave1819b <- sum(pc$ave1819b)  # The total average catch in 2018-19 without rockfish genus is 2171833
  pc$prop1819 <- pc$ave1819 / sumave1819  # Calculate the proportion of catch for each species to the total
  pc$prop1819b <- pc$ave1819b / sumave1819b  # Calculate the proportion of catch for each species to the total without rockfish genus
  sum2020 <- sum(pc$x2020) # Get the total catch in 2020 including rockfish genus
  exp_rg_2020 <- pc[pc$species=="ROCKFISH GENUS", "prop1819"] * sum2020  # This is the expected value for rockfish genus catch in 2020
  to_allocate <- (pc[pc$species=="ROCKFISH GENUS", "x2020"]) - exp_rg_2020  # This is the amount of rockfish genus catch in 2020 that needs to be allocated
  pc$x2020allocated <- pc$x2020 + (pc$prop1819b * to_allocate[1,1]) # Allocate the extra rockfish genus catch
  pc[pc$species=="ROCKFISH GENUS", "x2020allocated"] <- exp_rg_2020  # Replace the rockfish genus catch with the expected value
  sum(pc$x2020allocated)  # Check that the new allocated 2020 sum is equal to the original 2020 sum.  It is.
  pc <- as.data.frame(pc)
  pcmat2020[i,1] <- pc[pc$species=="ROCKFISH GENUS", "x2020"]
  pcmat2020[i,2] <- ifelse(length(pc[pc$species=="COPPER ROCKFISH", "x2020"])==0, 0, pc[pc$species=="COPPER ROCKFISH", "x2020"])
  pcmat2020[i,3] <- ifelse(length(pc[pc$species=="BLACK ROCKFISH", "x2020"])==0, 0, pc[pc$species=="BLACK ROCKFISH", "x2020"])
  pcmat2020[i,4] <- ifelse(length(pc[pc$species=="CANARY ROCKFISH", "x2020"])==0, 0, pc[pc$species=="CANARY ROCKFISH", "x2020"])
  pcmat2020[i+6,1] <- pc[pc$species=="ROCKFISH GENUS", "x2020allocated"]
  pcmat2020[i+6,2] <- ifelse(length(pc[pc$species=="COPPER ROCKFISH", "x2020allocated"])==0, 0, pc[pc$species=="COPPER ROCKFISH", "x2020allocated"])
  pcmat2020[i+6,3] <- ifelse(length(pc[pc$species=="BLACK ROCKFISH", "x2020allocated"])==0, 0, pc[pc$species=="BLACK ROCKFISH", "x2020allocated"])
  pcmat2020[i+6,4] <- ifelse(length(pc[pc$species=="CANARY ROCKFISH", "x2020allocated"])==0, 0, pc[pc$species=="CANARY ROCKFISH", "x2020allocated"])
}

pcmat2020 <- as.data.frame(round(pcmat2020, 2))

write.csv(pcmat2020, "genus_pc_2020.csv")

#================================================================================================
# Create a matrix of allocated values by district for PC in 2021

pcmat2021 <- matrix(NA, 12, 4)
colnames(pcmat2021) <- c("genus_num", "copper_num", "black_num", "canary_num")
rownames(pcmat2021) <- c("original 1", "original 2", "original 3", "original 4", "original 5", "original 6", "allocated 1", "allocated 2", "allocated 3", "allocated 4", "allocated 5", "allocated 6")

for (i in 1:length(districts)){
  temp <- filter(allrockpc, DISTRICT_NAME==districts[i])
  temp <- temp %>% group_by(RECFIN_YEAR, SPECIES_NAME) %>% summarize(sumcatchnum=sum(TOTAL_MORTALITY_NUM))
  pc <- spread(temp, RECFIN_YEAR, sumcatchnum)
  colnames(pc) <- c("species", "x2018", "x2019", "x2020", "x2021", "x2022")
  pc[,2:6] <- sapply(pc[,2:6], as.numeric)
  pc[is.na(pc)] <- 0
  pc$ave1819 <- (pc$x2018 + pc$x2019)/2
  sumave1819 <- sum(pc$ave1819)  # The total average catch in 2018-19 is 2376219
  pc$ave1819b <- pc$ave1819  # Copy this average to a new column
  pc[pc$species=="ROCKFISH GENUS", "ave1819b"] <- 0 # Remove rockfish genus catch from this new column
  sumave1819b <- sum(pc$ave1819b)  # The total average catch in 2018-19 without rockfish genus is 2171833
  pc$prop1819 <- pc$ave1819 / sumave1819  # Calculate the proportion of catch for each species to the total
  pc$prop1819b <- pc$ave1819b / sumave1819b  # Calculate the proportion of catch for each species to the total without rockfish genus
  sum2021 <- sum(pc$x2021) # Get the total catch in 2020 including rockfish genus
  exp_rg_2021 <- pc[pc$species=="ROCKFISH GENUS", "prop1819"] * sum2021  # This is the expected value for rockfish genus catch in 2021
  to_allocate <- (pc[pc$species=="ROCKFISH GENUS", "x2020"]) - exp_rg_2021  # This is the amount of rockfish genus catch in 2021 that needs to be allocated
  pc$x2021allocated <- pc$x2021 + (pc$prop1819b * to_allocate[1,1]) # Allocate the extra rockfish genus catch
  pc[pc$species=="ROCKFISH GENUS", "x2021allocated"] <- exp_rg_2021  # Replace the rockfish genus catch with the expected value
  sum(pc$x2021allocated)  # Check that the new allocated 2020 sum is equal to the original 2021 sum.  It is.
  pc <- as.data.frame(pc)
  pcmat2021[i,1] <- pc[pc$species=="ROCKFISH GENUS", "x2021"]
  pcmat2021[i,2] <- ifelse(length(pc[pc$species=="COPPER ROCKFISH", "x2021"])==0, 0, pc[pc$species=="COPPER ROCKFISH", "x2021"])
  pcmat2021[i,3] <- ifelse(length(pc[pc$species=="BLACK ROCKFISH", "x2021"])==0, 0, pc[pc$species=="BLACK ROCKFISH", "x2021"])
  pcmat2021[i,4] <- ifelse(length(pc[pc$species=="CANARY ROCKFISH", "x2021"])==0, 0, pc[pc$species=="CANARY ROCKFISH", "x2021"])
  pcmat2021[i+6,1] <- pc[pc$species=="ROCKFISH GENUS", "x2021allocated"]
  pcmat2021[i+6,2] <- ifelse(length(pc[pc$species=="COPPER ROCKFISH", "x2021allocated"])==0, 0, pc[pc$species=="COPPER ROCKFISH", "x2021allocated"])
  pcmat2021[i+6,3] <- ifelse(length(pc[pc$species=="BLACK ROCKFISH", "x2021allocated"])==0, 0, pc[pc$species=="BLACK ROCKFISH", "x2021allocated"])
  pcmat2021[i+6,4] <- ifelse(length(pc[pc$species=="CANARY ROCKFISH", "x2021allocated"])==0, 0, pc[pc$species=="CANARY ROCKFISH", "x2021allocated"])
}

pcmat2021 <- as.data.frame(round(pcmat2021, 2))

write.csv(pcmat2021, "genus_pc_2021.csv")

#================================================================================================
# Create a matrix of allocated values by district for PR in 2020

prmat <- matrix(NA, 12, 4)
colnames(prmat) <- c("genus_num", "copper_num", "black_num", "canary_num")
rownames(prmat) <- c("original 1", "original 2", "original 3", "original 4", "original 5", "original 6", "allocated 1", "allocated 2", "allocated 3", "allocated 4", "allocated 5", "allocated 6")

for (i in 1:length(districts)){
  temp <- filter(allrockpr, DISTRICT_NAME==districts[i])
  temp <- temp %>% group_by(RECFIN_YEAR, SPECIES_NAME) %>% summarize(sumcatchnum=sum(TOTAL_MORTALITY_NUM))
  pr <- spread(temp, RECFIN_YEAR, sumcatchnum)
  colnames(pr) <- c("species", "x2018", "x2019", "x2020", "x2021", "x2022")
  pr[,2:6] <- sapply(pr[,2:6], as.numeric)
  pr[is.na(pr)] <- 0
  pr$ave1819 <- (pr$x2018 + pr$x2019)/2
  sumave1819 <- sum(pr$ave1819)  # The total average catch in 2018-19 is 2376219
  pr$ave1819b <- pr$ave1819  # Copy this average to a new column
  pr[pr$species=="ROCKFISH GENUS", "ave1819b"] <- 0 # Remove rockfish genus catch from this new column
  sumave1819b <- sum(pr$ave1819b)  # The total average catch in 2018-19 without rockfish genus is 2171833
  pr$prop1819 <- pr$ave1819 / sumave1819  # Calculate the proportion of catch for each species to the total
  pr$prop1819b <- pr$ave1819b / sumave1819b  # Calculate the proportion of catch for each species to the total without rockfish genus
  sum2020 <- sum(pr$x2020) # Get the total catch in 2020 including rockfish genus
  exp_rg_2020 <- pr[pr$species=="ROCKFISH GENUS", "prop1819"] * sum2020  # This is the expected value for rockfish genus catch in 2020
  to_allocate <- (pr[pr$species=="ROCKFISH GENUS", "x2020"]) - exp_rg_2020  # This is the amount of rockfish genus catch in 2020 that needs to be allocated
  pr$x2020allocated <- pr$x2020 + (pr$prop1819b * to_allocate[1,1]) # Allocate the extra rockfish genus catch
  pr[pr$species=="ROCKFISH GENUS", "x2020allocated"] <- exp_rg_2020  # Replace the rockfish genus catch with the expected value
  sum(pr$x2020allocated)  # Check that the new allocated 2020 sum is equal to the original 2020 sum.  It is.
  pr <- as.data.frame(pr)
  prmat[i,1] <- pr[pr$species=="ROCKFISH GENUS", "x2020"]
  prmat[i,2] <- ifelse(length(pr[pr$species=="COPPER ROCKFISH", "x2020"])==0, 0, pr[pr$species=="COPPER ROCKFISH", "x2020"])
  prmat[i,3] <- ifelse(length(pr[pr$species=="BLACK ROCKFISH", "x2020"])==0, 0, pr[pr$species=="BLACK ROCKFISH", "x2020"])
  prmat[i,4] <- ifelse(length(pr[pr$species=="CANARY ROCKFISH", "x2020"])==0, 0, pr[pr$species=="CANARY ROCKFISH", "x2020"])
  prmat[i+6,1] <- pr[pr$species=="ROCKFISH GENUS", "x2020allocated"]
  prmat[i+6,2] <- ifelse(length(pr[pr$species=="COPPER ROCKFISH", "x2020allocated"])==0, 0, pr[pr$species=="COPPER ROCKFISH", "x2020allocated"])
  prmat[i+6,3] <- ifelse(length(pr[pr$species=="BLACK ROCKFISH", "x2020allocated"])==0, 0, pr[pr$species=="BLACK ROCKFISH", "x2020allocated"])
  prmat[i+6,4] <- ifelse(length(pr[pr$species=="CANARY ROCKFISH", "x2020allocated"])==0, 0, pr[pr$species=="CANARY ROCKFISH", "x2020allocated"])
}

prmat <- as.data.frame(round(prmat, 2))

write.csv(prmat, "genus_pr_2020.csv")

#=========================================================================================
# Load in CRFS data on average weight to convert matrices of allocated fish to weight
# These average weight tables are created by using the files CRFS PC Catch and Effort Estimates DDM (RecFIN format)_All_2020 (07-28-2021).xlsx for 2020 and 2021.  I used pivot tables to calculate the average weight within a district.  I wanted to do this in R but the formatting of the original file imported in strange ways and was difficult to clean up.

PcAveWt2019 <- read.csv("pc ave wt 2019.csv")
# No average weight exists for black rockfish in districts 1&2.  Borrow estimate from district 3.
PcAveWt2019[PcAveWt2019$district==1, "black.rockfish"] <- PcAveWt2019[PcAveWt2019$district==3, "black.rockfish"]
PcAveWt2019[PcAveWt2019$district==2, "black.rockfish"] <- PcAveWt2019[PcAveWt2019$district==3, "black.rockfish"]
# No average weight exists for canary rockfish in district 2.  Borrow estimate from the average of districts 1 & 3.
PcAveWt2019[PcAveWt2019$district==2, "canary.rockfish"] <- (PcAveWt2019[PcAveWt2019$district==1, "canary.rockfish"] + PcAveWt2019[PcAveWt2019$district==3, "canary.rockfish"])/2

PrAveWt2019 <- read.csv("pr ave wt 2019.csv")
# No average weight exists for black rockfish in districts 1&2.  Borrow estimate from district 3.
PrAveWt2019[PrAveWt2019$district==1, "black.rockfish"] <- PrAveWt2019[PrAveWt2019$district==3, "black.rockfish"]
PrAveWt2019[PrAveWt2019$district==2, "black.rockfish"] <- PrAveWt2019[PrAveWt2019$district==3, "black.rockfish"]

# PC 2020
pcmat2020$copper_kg <- NA
pcmat2020$copper_kg[1:6] <- pcmat2020$copper_num[1:6] * PcAveWt2019$copper.rockfish[1:6]
pcmat2020$copper_kg[7:12] <- pcmat2020$copper_num[7:12] * PcAveWt2019$copper.rockfish[1:6]

pcmat2020$black_kg <- NA
pcmat2020$black_kg[1:6] <- pcmat2020$black_num[1:6] * PcAveWt2019$black.rockfish[1:6]
pcmat2020$black_kg[7:12] <- pcmat2020$black_num[7:12] * PcAveWt2019$black.rockfish[1:6]

pcmat2020$canary_kg <- NA
pcmat2020$canary_kg[1:6] <- pcmat2020$canary_num[1:6] * PcAveWt2019$canary.rockfish[1:6]
pcmat2020$canary_kg[7:12] <- pcmat2020$canary_num[7:12] * PcAveWt2019$canary.rockfish[1:6]

# PC 2021
pcmat2021$copper_kg <- NA
pcmat2021$copper_kg[1:6] <- pcmat2021$copper_num[1:6] * PcAveWt2019$copper.rockfish[1:6]
pcmat2021$copper_kg[7:12] <- pcmat2021$copper_num[7:12] * PcAveWt2019$copper.rockfish[1:6]

pcmat2021$black_kg <- NA
pcmat2021$black_kg[1:6] <- pcmat2021$black_num[1:6] * PcAveWt2019$black.rockfish[1:6]
pcmat2021$black_kg[7:12] <- pcmat2021$black_num[7:12] * PcAveWt2019$black.rockfish[1:6]

pcmat2021$canary_kg <- NA
pcmat2021$canary_kg[1:6] <- pcmat2021$canary_num[1:6] * PcAveWt2019$canary.rockfish[1:6]
pcmat2021$canary_kg[7:12] <- pcmat2021$canary_num[7:12] * PcAveWt2019$canary.rockfish[1:6]

# PR 2020
prmat$copper_kg <- NA
prmat$copper_kg[1:6] <- prmat$copper_num[1:6] * PrAveWt2019$copper.rockfish[1:6]
prmat$copper_kg[7:12] <- prmat$copper_num[7:12] * PrAveWt2019$copper.rockfish[1:6]

prmat$black_kg <- NA
prmat$black_kg[1:6] <- prmat$black_num[1:6] * PrAveWt2019$black.rockfish[1:6]
prmat$black_kg[7:12] <- prmat$black_num[7:12] * PrAveWt2019$black.rockfish[1:6]

prmat$canary_kg <- NA
prmat$canary_kg[1:6] <- prmat$canary_num[1:6] * PrAveWt2019$canary.rockfish[1:6]
prmat$canary_kg[7:12] <- prmat$canary_num[7:12] * PrAveWt2019$canary.rockfish[1:6]

# Combine all 3 tables
pcmat2020$year <- 2020
pcmat2021$year <- 2021
prmat$year <- 2020
pcmat2020$mode <- "pc"
pcmat2021$mode <- "pc"
prmat$mode <- "pr"
genus_allocate <- rbind(pcmat2020, pcmat2021, prmat)
genus_allocate$orig_allocated <- rownames(genus_allocate) 
genus_allocate <- genus_allocate %>% separate(orig_allocated, c("orig_allocated", "district"), " ")
rownames(genus_allocate) <- c()

write.csv(genus_allocate, "genus_allocate.csv")
