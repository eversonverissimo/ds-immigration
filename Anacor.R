
library(anacor)
library(readxl)
library(sjPlot)
library(dplyr)
library(stringr)
library(ggplot2)


Refugees <- read.csv("database/Dataset.csv")

Refugees_categorical <- Refugees[,2:25]
Refugees_categorical$Income <- str_replace(Refugees_categorical$Income, "Low income",          "1. Low income")
Refugees_categorical$Income <- str_replace(Refugees_categorical$Income, "Lower middle income", "2. Lower middle income")
Refugees_categorical$Income <- str_replace(Refugees_categorical$Income, "Upper middle income", "3. Upper middle income")
Refugees_categorical$Income <- str_replace(Refugees_categorical$Income, "High income",         "4. High income")

Refugees_categorical$OECD <- Refugees$OECD
Refugees_categorical$OECD[Refugees_categorical$OECD == 0] <- 'Não'
Refugees_categorical$OECD[Refugees_categorical$OECD == 1] <- 'Sim'
#Refugees_categorical <- Refugees_categorical %>% dplyr::arrange(factor(col1, levels = c("Low income", "Lower middle income", "Upper middle income", "High income")))

col1 <- Refugees_categorical$OECD
col2 <- Refugees_categorical[,18]
cols <- data.frame(col1, col2)

# Tabelas de frequências
##############################################
tab_freq <- summary(Refugees_categorical)

# Tabelas de Contingências
##############################################

tableContFunc <- function(col1, col2) {
  
  cols <- data.frame(col1, col2)
  
  # Criando uma tabela de contingências
  tab_cont <- table(cols)
  tab_cont
}
tab_cont <- tableContFunc(cols$col1, cols$col2)
tab_cont

# Tabela de contingências mais elegante com valores esperados
sjt.xtab(var.row = cols$col1,
         var.col = cols$col2,
         show.exp = TRUE,
         show.cell.prc = TRUE,
         title="Tabela de contingência")

pvalueCorrelatedFunc <- function(tab_cont){
  qui2 <- chisq.test(tab_cont)
  qui2$p.value
}
p_value_correlated <- pvalueCorrelatedFunc(tab_cont)
p_value_correlated

# Teste Qui-Quadrado
##############################################
qui2 <- chisq.test(tab_cont)
qui2

qui2$statistic # Valor calculado Qui2
qui2$parameter # Graus de liberdade
qui2$p.value # p-value
qui2$method
qui2$data.name
qui2$observed # Valores observados
qui2$expected # Valores esperados
qui2$observed - qui2$expected # Valores dos resíduos
qui2$residuals #Resíduos PADRONIZADOS
qui2$stdres #Resíduos PADRONIZADOS AJUSTADOS


# Mapa de calor dos resíduos padronizados ajustados
####################################################
data.frame(qui2$stdres) %>%
  ggplot(aes(x = col1, 
             y = col2, 
             fill = Freq, 
             label = round(Freq,3))) +
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


#Decomposição da inércia principal total
##############################################
It <- qui2$statistic/nrow(cols)
It

#Construindo a matriz P
##############################################
P <- 1/nrow(cols) * tab_cont
P

# Profiles
##############################################

#Column profile
data.frame(tab_cont) %>% 
  group_by(col2) %>% 
  summarise(col1 = col1,
            Massas = Freq / sum(Freq)) %>% 
  dcast(col1 ~ col2) %>% 
  column_to_rownames("Col1") %>% 
  round(., digits = 3)

column_profile <- apply(tab_cont, MARGIN = 1, FUN = sum) / nrow(cols)
column_profile

#Row profiles
data.frame(tab_cont) %>% 
  group_by(col1) %>% 
  summarise(Var2 = col2,
            Massas = Freq / sum(Freq)) %>% 
  dcast(Var1 ~ Var2) %>% 
  column_to_rownames("Var1") %>% 
  round(., digits = 3)

row_profile <- apply(tab_cont, MARGIN = 2, FUN = sum) / nrow(cols)
row_profile

#Matriz Dl
Dl <- diag(column_profile)
Dl

#Matriz Dc
Dc <- diag(row_profile)
Dc

#Matriz lc'
lc <- column_profile %o% row_profile
lc

#Matriz A
A <- diag(diag(Dl) ^ (-1/2)) %*% (P - lc) %*% diag(diag(Dc) ^ (-1/2))
A

#Curiosidade:
A_matriz <- qui2$residuals / sqrt(nrow(cols))
A_matriz

#Matriz W
W_matriz <- t(A_matriz) %*% A_matriz
W_matriz

#Extraindo os eigenvalues da matriz W
eigenvalues <- eigen(W_matriz)
eigenvalues

sum(eigenvalues$values) #It
It

#Dimensionalidade dos dados
dimensoes <- min(nrow(A_matriz) - 1, ncol(A_matriz) - 1)
dimensoes

#Percentual da Inércia Total explicada
It_explicada <- eigenvalues$values[1:2] / It
It_explicada


#Cálculo das coordenadas do mapa perceptual
##############################################

#Decomposição do valor singular da matriz A
decomp <- svd(x = A_matriz,
              nu = dimensoes,
              nv = dimensoes)
decomp

#Variável em linha - coordenada no eixo das abcissas
Xl_col1 <- diag((decomp$d[1]) * diag(diag(Dl)^(-1/2)) * decomp$u[,1])
Xl_col1

#Variável em linha - coordenada no eixo das ordenadas
Yl_col1 <- diag((decomp$d[2]) * diag(diag(Dl)^(-1/2)) * decomp$u[,2])
Yl_col1

#Variável em coluna - coordenada no eixo das abcissas
Xc_col2 <- diag((decomp$d[1]) * diag(diag(Dc)^(-1/2)) * decomp$v[,1])
Xc_col2

#Variável em coluna - coordenada no eixo das ordenadas
Yc_col2 <- diag((decomp$d[2]) * diag(diag(Dc)^(-1/2)) * decomp$v[,2])
Yc_col2

# Mapa perceptual bidimensional
##############################################

# Passo 1: Guardando as coordenadas, de cada categoria e de cada variável,  num 
# único objeto
coordenadas <- NA

if (ncol(decomp$u) > 1){
  
  coordenadas <- data.frame(Categorias = cbind(c(levels(cols$col1),
                                                 levels(cols$col2))),
                            Dim1 = cbind(c(Xl_col1, Xc_col2)),
                            Dim2 = cbind(c(Yl_col1, Yc_col2)))
} else {
  coordenadas <- data.frame(Categorias = cbind(c(levels(cols$col1),
                                                 levels(cols$col2))),
                            Dim1 = cbind(c(Xl_col1, Xc_col2)),
                            Dim2 = 0)
}

coordenadas


# Passo 2: Como iremos estratificar as categorias em função de cores distintas
# em função de qual variável elas pertencem, vamos criar uma coluna que faça
# essa identificação:
variaveis <- apply(cols[,1:2],
                   MARGIN =  2,
                   FUN = function(x) nlevels(as.factor(x)))
variaveis


# Passo 3: Vamos juntar, o objeto variaveis ao objeto coordenadas:
coordenadas_final <- data.frame(coordenadas,
                                Variaveis = rep(names(variaveis), variaveis))
coordenadas_final

# Passo 4: Plotando o mapa perceptual bidimensional:
coordenadas_final %>% 
  rownames_to_column() %>% 
  ggplot(aes(x = Dim1, y = Dim2, label = Categorias, color = Variaveis)) +
  geom_point() +
  geom_label_repel() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = paste("Dimension 1:", paste0(round(It_explicada[1] * 100, digits = 2), "%")),
       y = paste("Dimension 2:", paste0(round(It_explicada[2] * 100, digits = 2), "%"))) +
  scale_color_manual("Variable:",
                     values = c("darkblue", "darkred")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")


# Calculando chi-square
anacor(tab_cont)

