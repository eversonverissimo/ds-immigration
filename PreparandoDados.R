
library(readxl)
library(dplyr)

###### Refugees #########
#########################

Refugees <- read_excel("database/Refugees.xlsx", sheet = 2)

# Add column names
Refugees[2,1] <- "Country"
Refugees[2,2] <- "Region"
Refugees[2,3] <- "Income"
colnames(Refugees) <- Refugees[2,]

# Remove unnecessary columns and rows
Refugees <- Refugees[3:136,1:23]
#View(Refugees)


###### Regions ##########
#########################

# Read regions to merge wit Refugee dataset
Regions <- read_excel("database/Countries-Regions.xlsx")
Regions <- Regions[,c("Country", "Region 1", "Continent")]

# Merge the two datasets
Refugees_New <- merge(Refugees, Regions, # Data frames or objects to be coerced
      by = "Country", all.x = TRUE)
#Null_Regions = Refugees_New[is.na(Refugees_New["Region 1"]), "Country"]

# Add column OECD
Refugees_New["OECD"] <- Refugees_New$Region == "High income: OECD"
Refugees_New[Refugees_New$OECD == TRUE, "OECD"] = 1
Refugees_New[Refugees_New$OECD == FALSE, "OECD"] = 0

# Add columns Counts of NAs, Yes, No, Conditions
Refugees_New$CountNAs <- rowSums(Refugees_New == "N/A")
Refugees_New$CountYes <- rowSums(Refugees_New == "Yes")
Refugees_New$CountNo <- rowSums(Refugees_New == "No")
Refugees_New$CountConditions <- rowSums(Refugees_New == "Conditions")

# Dummy Income
Refugees_New$LowIncome <- Refugees_New$Income == "Low income"
Refugees_New$LowerMiddleIncome <- Refugees_New$Income == "Lower middle income"
Refugees_New$UpperMiddleIncome <- Refugees_New$Income == "Upper middle income"
Refugees_New$HighIncome <- Refugees_New$Income == "High income"

Refugees_New[Refugees_New$LowIncome == TRUE, "LowIncome"] = 1
Refugees_New[Refugees_New$LowIncome == FALSE, "LowIncome"] = 0
Refugees_New[Refugees_New$LowerMiddleIncome == TRUE, "LowerMiddleIncome"] = 1
Refugees_New[Refugees_New$LowerMiddleIncome == FALSE, "LowerMiddleIncome"] = 0
Refugees_New[Refugees_New$UpperMiddleIncome == TRUE, "UpperMiddleIncome"] = 1
Refugees_New[Refugees_New$UpperMiddleIncome == FALSE, "UpperMiddleIncome"] = 0
Refugees_New[Refugees_New$HighIncome == TRUE, "HighIncome"] = 1
Refugees_New[Refugees_New$HighIncome == FALSE, "HighIncome"] = 0


# Bilateral
bilateralImigration <- read_excel("database/bilateralmigration.xlsx")

bilateralImigrationDF <- data.frame(bilateralImigration[1:218,1:218])
rownames(bilateralImigrationDF) <- bilateralImigrationDF[,1]
colnames(bilateralImigrationDF) <- bilateralImigrationDF[,1]
bilateralImigrationDF <- bilateralImigrationDF[-1,-1]
bilateralImigrationDF[] <- lapply(bilateralImigrationDF, function(x) as.numeric(as.character(x)))

source <- rowSums(bilateralImigrationDF)
destination <- colSums(bilateralImigrationDF)
immigrationRate <- destination/source

source2 <- data.frame(bilateralImigrationDF[, 'World'])
colnames(source2) <- c('World')
rownames(source2) <- rownames(bilateralImigrationDF)
destination2 <- data.frame(t(bilateralImigrationDF['World',]))
immigrationRate2 <- destination2/source2
immigrationRate2$Country <- rownames(immigrationRate2)
immigrationRate2 <- rename(immigrationRate2, ImmigrationRate = World)

# Merge the two datasets
Refugees_New <- merge(Refugees_New, immigrationRate2, # Data frames or objects to be coerced
                      by = "Country", all.x = TRUE)
Refugees_New <- subset(Refugees_New, Country != 'Taiwan, Republic of China') 

quantis <- quantile(Refugees_New$ImmigrationRate, prob=c(.25,.5,.75), na.rm=TRUE)

Refugees_New[, "LowImmigration"] = 0
Refugees_New[, "LowerMiddleImmigration"] = 0
Refugees_New[, "UpperMiddleImmigration"] = 0
Refugees_New[, "HighImmigration"] = 0

Refugees_New[Refugees_New$ImmigrationRate <= quantis[1], "LowImmigration"] = 1
Refugees_New[between(Refugees_New$ImmigrationRate, quantis[1], quantis[2]), "LowerMiddleImmigration"] = 1
Refugees_New[between(Refugees_New$ImmigrationRate, quantis[2], quantis[3]), "UpperMiddleImmigration"] = 1
Refugees_New[Refugees_New$ImmigrationRate > quantis[3], "HighImmigration"] = 1


Refugees_New$ImmigrationStatus = ""
Refugees_New[Refugees_New$LowImmigration == 1,         "ImmigrationStatus"] = "LowImmigration"
Refugees_New[Refugees_New$LowerMiddleImmigration == 1, "ImmigrationStatus"] = "LowerMiddleImmigration"
Refugees_New[Refugees_New$UpperMiddleImmigration == 1, "ImmigrationStatus"] = "UpperMiddleImmigration"
Refugees_New[Refugees_New$HighImmigration == 1,        "ImmigrationStatus"] = "HighImmigration"

             

write.csv(Refugees_New, "database/Dataset.csv")
