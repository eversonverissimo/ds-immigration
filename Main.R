
library(readxl)
Refugees <- read_excel("database/Refugees.xlsx", sheet = 2)

# Add column names
Refugees[2,1] <- "Country"
Refugees[2,2] <- "Region"
Refugees[2,3] <- "Income"
colnames(Refugees) <- Refugees[2,]

# Remove unnecessary columns and rows
Refugees <- Refugees[3:136,1:23]
View(Refugees)


merge(x, y, # Data frames or objects to be coerced
      by = intersect(names(x), names(y)),


#Criando uma tabela de contingências
tab <- table(Refugees$Region, 
             Refugees$Income)
#Exemplo de uma tabela de contingências mais elegante
sjt.xtab(var.row = Refugees$Region,
         var.col = Refugees$Income,show.exp = TRUE)

#Teste Qui-Quadrado
qui2 <- chisq.test(tab)
qui2

#Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2$stdres) %>%
  rename(Region = 1,
         Income = 2) %>% 
  ggplot(aes(x = fct_rev(Region), 
             y = Income, 
             fill = Freq, 
             label = round(Freq,3))) +
  geom_tile() +
  geom_label(size = 3, fill = "white") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text())

