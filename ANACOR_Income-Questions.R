
library(readxl)
library(ggplot2)

Refugees <- read.csv("database/Dataset.csv")
Refugees_categorical <- Refugees[,2:25]
Refugees_categorical$OECD <- Refugees$OECD

pvalues = array()
its = array()
for (i in 4:23){
  table_cont <- table(Refugees_categorical$Income, Refugees_categorical[,i])
  qui2 <- chisq.test(table_cont)
  pvalues[i-3] <- qui2$p.value
  its[i-3] <- qui2$statistic/nrow(Refugees_categorical)
}

pvalues_plot <- cbind(array(1:20), pvalues)
pval_threshold <- 0.05
pval_threshold_line <- array(pval_threshold, dim=20)

color_bars <- pvalues > pval_threshold
color_bars[color_bars == TRUE] = 'darkblue'
color_bars[color_bars == FALSE] = 'darkred'
correlation <- pvalues > pval_threshold
correlation[correlation == TRUE] = 'Não-correlacionado'
correlation[correlation == FALSE] = 'Correlacionado'

df <- data.frame(
  Perguntas=pvalues_plot[,1],
  PValues=pvalues_plot[,2],
  Correlação=correlation,
  inertia=its,
  pval_threshold_line=pval_threshold_line)

ggplot(df, aes(Perguntas)) + 
  geom_bar(aes(y=PValues, fill=Correlação), stat="identity") + 
  geom_line(aes(y=inertia), colour="green") +
  geom_line(aes(y=pval_threshold_line), colour="red") +
  theme_light() +
  theme(legend.box="horizontal",legend.key=element_blank(), 
        legend.title=element_blank(),legend.position="top") +
  scale_fill_manual(values=c("Não-correlacionado" = "darkred", "Correlacionado" = "darkblue"))

