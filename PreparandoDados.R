
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

write.csv(Refugees_New, "database/Dataset.csv")



