# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(readxl)

Refugees <- read.csv("database/Dataset.csv")

cor(Refugees[,c("LowIncome")], 
    Refugees[,c("CountYes", "CountNo", "CountConditions", "CountNAs")])
cor(Refugees[,c("LowerMiddleIncome")], 
    Refugees[,c("CountYes", "CountNo", "CountConditions", "CountNAs")])
cor(Refugees[,c("UpperMiddleIncome")], 
    Refugees[,c("CountYes", "CountNo", "CountConditions", "CountNAs")])
  cor(Refugees[,c("HighIncome")], 
    Refugees[,c("CountYes", "CountNo", "CountConditions", "CountNAs")])
cor(Refugees[,c("OECD")], 
    Refugees[,c("CountYes", "CountNo", "CountConditions", "CountNAs")])



# OECD vs Counts
chart.Correlation(Refugees[,c("OECD", "CountNAs")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("OECD", "CountYes")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("OECD", "CountNo")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("OECD", "CountConditions")], histogram = TRUE, method = "pearson")

# OECD vs Income
chart.Correlation(Refugees[,c("OECD", "LowIncome")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("OECD", "LowerMiddleIncome")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("OECD", "UpperMiddleIncome")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("OECD", "HighIncome")], histogram = TRUE, method = "pearson")

# Income vs Count Nas
chart.Correlation(Refugees[,c("LowIncome", "CountNAs")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("LowerMiddleIncome", "CountNAs")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("UpperMiddleIncome", "CountNAs")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("HighIncome", "CountNAs")], histogram = TRUE, method = "pearson")

# Income vs Count Yes
chart.Correlation(Refugees[,c("LowIncome", "CountYes")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("LowerMiddleIncome", "CountYes")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("UpperMiddleIncome", "CountYes")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("HighIncome", "CountYes")], histogram = TRUE, method = "pearson")

# Income vs Count No
chart.Correlation(Refugees[,c("LowIncome", "CountNo")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("LowerMiddleIncome", "CountNo")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("UpperMiddleIncome", "CountNo")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("HighIncome", "CountNo")], histogram = TRUE, method = "pearson")

# Income vs Count Conditions
chart.Correlation(Refugees[,c("LowIncome", "CountConditions")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("LowerMiddleIncome", "CountConditions")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("UpperMiddleIncome", "CountConditions")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("HighIncome", "CountConditions")], histogram = TRUE, method = "pearson")


################ IMMIGRATION RATE ###################

# OECD vs ImmigrationRate
chart.Correlation(Refugees[,c("OECD", "ImmigrationRate")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("OECD", "LowImmigration")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("OECD", "LowerMiddleImmigration")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("OECD", "UpperMiddleImmigration")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("OECD", "HighImmigration")], histogram = TRUE, method = "pearson")

# CountNo vs ImmigrationRate
chart.Correlation(Refugees[,c("CountNo", "ImmigrationRate")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("CountNo", "LowImmigration")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("CountNo", "LowerMiddleImmigration")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("CountNo", "UpperMiddleImmigration")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("CountNo", "HighImmigration")], histogram = TRUE, method = "pearson")

# CountYes vs ImmigrationRate
chart.Correlation(Refugees[,c("CountYes", "ImmigrationRate")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("CountYes", "LowImmigration")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("CountYes", "LowerMiddleImmigration")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("CountYes", "UpperMiddleImmigration")], histogram = TRUE, method = "pearson")
chart.Correlation(Refugees[,c("CountYes", "HighImmigration")], histogram = TRUE, method = "pearson")
