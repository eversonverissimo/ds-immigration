# Pacotes a serem instalados e carregados ---------------------------------

#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","sjPlot","knitr","kableExtra",
             "FactoMineR","cabootcrs", "factoextra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Análise de Correspondênciasa Múltiplas (ACM) - Abordagem Teórica --------
# Carregando a base de dados

Refugees <- read.csv("database/Dataset.csv")

Refugees_categorical <- Refugees[,4:24]
Refugees_categorical$Income <- str_replace(Refugees_categorical$Income, "Low income",          "1. Low income")
Refugees_categorical$Income <- str_replace(Refugees_categorical$Income, "Lower middle income", "2. Lower middle income")
Refugees_categorical$Income <- str_replace(Refugees_categorical$Income, "Upper middle income", "3. Upper middle income")
Refugees_categorical$Income <- str_replace(Refugees_categorical$Income, "High income",         "4. High income")

Refugees_categorical$OECD <- Refugees$OECD
Refugees_categorical$OECD[Refugees_categorical$OECD == 0] <- 'Não'
Refugees_categorical$OECD[Refugees_categorical$OECD == 1] <- 'Sim'

#Refugees_categorical$ImmigrationStatus <- Refugees$ImmigrationStatus

Refugees_categorical <- relocate(Refugees_categorical, ImmigrationStatus, .after = Income)
Refugees_categorical <- relocate(Refugees_categorical, OECD, .after = Income)


# Refugees_categorical os dados
Refugees_categorical %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Não há como se estabelecer umas única tabela de contingência que abranja todas
# as variáveis presentes na base de dados. Assim, poderíamos utilizar a função
# table() ou a função sjt.xtab() para analisarmos as variáveis duas a duas:

# Perfil x Aplicação
sjt.xtab(var.row = Refugees_categorical$Can.Refugees.work.in.the.formal.private.sector., 
         var.col = Refugees_categorical$Are.Refugees.entitled.to.receive.unemployment.benefits.,
         show.exp = TRUE, 
         show.row.prc = TRUE, 
         show.col.prc = TRUE)


# Estudo do Qui-Quadrado de cada tabela de contingências ------------------

tab_Refugees_categorical <- table(Refugees_categorical$Can.Refugees.work.in.the.formal.private.sector.,
                              Refugees_categorical$Are.Refugees.entitled.to.receive.unemployment.benefits.)

tab_Refugees_categorical

qui2_tab_Refugees_categorical <- chisq.test(tab_Refugees_categorical)
qui2_tab_Refugees_categorical

# Para a ANACOR, juntamos os cruzamentos das categorias de DUAS variáveis 
# categóricas numa matriz de contingências. Nesse momento, porém, temos TRÊS
# variáveis categóricas. Como proceder?

# Matriz Binária e Matriz de Burt -----------------------------------------

# Para se estabelecer uma matriz binária:
matriz_binaria <- getindicator(Xinput = tab_Refugees_categorical)
matriz_binaria

CA(matriz_binaria)

# Para a matriz de Burt:
matriz_burt <- getBurt(Xinput = tab_Refugees_categorical[, 2:4])
matriz_burt

CA(matriz_burt)


# Rodando a ACM -----------------------------------------------------------
Refugees_categorical_code <- Refugees_categorical
Refugees_categorical_code$Income <- gsub("1. ","", as.character(Refugees_categorical_code$Income))
Refugees_categorical_code$Income <- gsub("2. ","", as.character(Refugees_categorical_code$Income))
Refugees_categorical_code$Income <- gsub("3. ","", as.character(Refugees_categorical_code$Income))
Refugees_categorical_code$Income <- gsub("4. ","", as.character(Refugees_categorical_code$Income))
Refugees_categorical_code$OECD <- gsub("Não","Não pertence à OECD", as.character(Refugees_categorical_code$OECD))
Refugees_categorical_code$OECD <- gsub("Sim","Pertence à OECD", as.character(Refugees_categorical_code$OECD))

names(Refugees_categorical_code) <- c(-1:20)

Refugees_categorical_code <- rename(Refugees_categorical_code, Income = "-1")
Refugees_categorical_code <- rename(Refugees_categorical_code, OCDE = "0")
ACM <- MCA(Refugees_categorical_code, method = "Indicador")

# Plot dos individuos OECD
ggplot(data.frame(ACM$ind$coord[Refugees_categorical$OECD == "Não",1:2]), 
       aes(x = ACM$ind$coord[Refugees_categorical$OECD == "Não",1], 
           y = ACM$ind$coord[Refugees_categorical$OECD == "Não",2],
           label = Refugees$Country[Refugees_categorical$OECD == "Não"]))+
  geom_label(vjust = 2) + xlab('Dim 1 (21.6%)') + ylab('Dim 2 (7.5%)') +
  labs(title = paste("Distribuição de países de fora da OECD pelo ACM")) +
  xlim(-1.5, 2.5) + ylim(-1, 2) +
  theme_bw()

fviz_mca_var(ACM)
fviz_mca_ind(ACM)
fviz_mca_biplot(ACM)

var <- get_mca_var(ACM)
var
fviz_mca_var(ACM, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow),
             title = "Variáveis - ACM",
             ggtheme = theme_minimal())
fviz_mca_var(ACM, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping,
             title = "Categorias das variáveis - ACM",
             ggtheme = theme_minimal())
fviz_mca_var(ACM, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             title = "Categorias das variáveis - ACM",
             ggtheme = theme_minimal()
)

fviz_mca_ind(ACM, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())

color_ind <- Refugees_categorical_code$Income
color_ind <- color_ind['1. Low income'] <- 'darkred'
color_ind['2. Lower middle income'] <- 'red'
color_ind['3. Upper middle income'] <- 'lightblue'
color_ind['4. High income'] <- 'blue'

fviz_mca_ind(
  ACM, 
  habillage = Refugees_categorical_code$Income, 
  addEllipses = TRUE)

fviz_ellipses(ACM, c("OCDE", "Income"),
              geom = "point")
fviz_ellipses(ACM, 1:4, geom = "point")



fviz_cos2(ACM, choice = "var", axes = 1:2)
fviz_contrib(ACM, choice = "var", axes = 1, top = 15)


# O componte 'var$coord', presente no objeto ACM, contém as coordenadas 
# de cada categoria. 
ACM$var$coord

# Assim, temos as coordenadas:
round(ACM$var$coord, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# As inércias principais estão computadas no componente 'eig':
ACM$eig

# As coordenadas de cada observação estão no componente 'ind$coord' do objeto
# ACM:
ACM$ind$coord

# Para estudarmos o percentual da inérica principal explicada por 
# dimensão, podemos:
categorias <- apply(Refugees_categorical, 
                    MARGIN =  2, 
                    FUN = function(x) nlevels(as.factor(x)))

categorias

It <- (sum(categorias) - length(categorias)) / length(categorias)
It

sum(ACM$eig[,1])

It_explicada <- ACM$eig[,1] / sum(ACM$eig[,1])

data.frame(Dimensão = paste("Dimensão", 1:length(It_explicada)),
           Inércia_Total = It_explicada) %>%
  ggplot(aes(x = Dimensão, 
             y = Inércia_Total, 
             label = paste0(round(Inércia_Total,3)*100,"%"))) +
  geom_bar(stat = "identity",
           color = "#440154FF", 
           fill = "#287C8EFF") +
  geom_label(vjust = 2) +
  labs(title = paste("Inércia Total Explicada de",
                     paste0(sum(It_explicada) * 100),"%")) +
   theme_bw()

# Já o número de dimensões da ACM é dado por:
dimensoes <- sum(categorias) - length(categorias)
dimensoes

# O mapa perceptual -------------------------------------------------------

#Para o estabelecimento de um Mapa Perceptual, precisamos:

#1º Definir o número de categorias por variável
categorias <- apply(Refugees_categorical, 
                    MARGIN =  2, 
                    FUN = function(x) nlevels(as.factor(x)))

#2º transformar o objeto ACM em um data frame, levando-se em consideração quais 
#tipos de coordenadas se quer plotar. Neste exemplo, utilizaremos as coordenadas
#dadas pela matriz de binária
ACM_mp <- data.frame(ACM$var$coord, Variável = rep(names(categorias), categorias))

ACM_mp %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  mutate(Categoria = gsub("perfil.","", Categoria),
         Categoria = gsub("aplicacao.","", Categoria),
         Categoria = gsub("estado_civil.","", Categoria)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Plotando o Mapa Perceptual:
ACM_mp %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Dim.1, 
             y = Dim.3, 
             label = Categoria, 
             color = Variável, 
             shape = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey") +
  labs(x = paste("Dimensão 1:", paste0(round(ACM$eig[1,2], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(ACM$eig[3,2], 2), "%"))) +
  scale_color_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

#Plotando o Mapa Perceptual, evidenciando-se as coordenadas das observações:

#Criando um mapa perceptual mais elegante, com as posições relativas de cada
#observação

#1º Salvar as posições relativas de cada observação
ACM_observacoes_df <- data.frame(ACM$ind$coord)

#2º Utilizando a estrutura do mapa perceptual já estabelecido, vamos acrescentar 
#as novas informações presentes no objeto mca_observacoes_df

ACM_observacoes_df %>% 
  ggplot(aes(x = Dim.1, y = Dim.2, label = perfil_investidor_aplicacao$estudante)) +
  geom_point(shape = 17, color = "#E76F5AFF", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_text_repel(max.overlaps = 100, size = 3) +
  geom_density2d(color = "gray80") +
  geom_label_repel(data = ACM_mp, 
                   aes(x = Dim.1, y = Dim.2, 
                       label = rownames(ACM_mp), 
                       fill = Variável), 
                   color = "white") +
  labs(x = paste("Dimensão 1:", paste0(round(ACM$eig[,2][1], digits = 2), "%")),
       y = paste("Dimensão 2:", paste0(round(ACM$eig[,2][2], digits = 2), "%"))) +
  scale_fill_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

#E como criar um mapa perceptual 3D de uma ACM?
ACM_3D <- plot_ly()

# Adicionando as coordenadas
ACM_3D <- add_trace(p = ACM_3D,
                    x = ACM_mp[,1],
                    y = ACM_mp[,2],
                    z = ACM_mp[,3],
                    mode = "text",
                    text = rownames(ACM_mp),
                    textfont = list(color = "#440154FF"),
                    showlegend = FALSE)

# Adicionando as labels das dimensões
ACM_3D <- layout(p = ACM_3D,
                 scene = list(xaxis = list(title = colnames(ACM_mp)[1]),
                              yaxis = list(title = colnames(ACM_mp)[2]),
                              zaxis = list(title = colnames(ACM_mp)[3]),
                              aspectmode = "data"))

ACM_3D
# Fim ---------------------------------------------------------------------
