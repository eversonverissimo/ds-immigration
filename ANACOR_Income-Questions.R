
library(readxl)
library(ggplot2)

Refugees <- read.csv("database/Dataset.csv")
Refugees_categorical <- Refugees[,2:25]
Refugees_categorical$ImmigrationStatus <- Refugees$ImmigrationStatus

pvalues = array()
its = array()
for (i in 4:23){
  table_cont <- table(Refugees_categorical[,5], Refugees_categorical[,i])
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


# Perguntas vs Perguntas
#pvalues = matrix(nrow=20, ncol=20)
pvalues = data.frame(col1 = 1, col2 = 2, PVal = 0)
for (i in 4:23){
  for (j in 4:23){
    table_cont <- table(Refugees_categorical[,i], Refugees_categorical[,j])
    qui2 <- chisq.test(table_cont)
    #pvalues[pvalues$col1 == i-3, pvalues$col2 == j-3] <- qui2$p.value
    if (i != j){
      pvalues[nrow(pvalues) + 1,] = c((i-3), (j-3), qui2$p.value)
    }
  }
}
pvalues <- pvalues[-1,]
#pvalues_matrix <- as.matrix(pvalues)
ggp <- ggplot(pvalues_matrix, aes(X1, X2)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = 0.05))
ggp   
heatmap(pvalues, Rowv = NA, Colv = NA)   

pvalues %>%
  ggplot(aes(x = col1, 
             y = col2, 
             fill = log10(PVal), 
             label = round(log10(PVal), 3))) +
  geom_tile() +
  geom_label(size = 3, fill = "white") +
  scale_fill_gradient2(low = "darkred", 
                       mid = "white", 
                       high = "darkblue",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text())
  
