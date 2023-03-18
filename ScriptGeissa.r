#Script Geissa-Tese####

#Abertura do banco
#Banco inicial sem alterações
library(readxl)
banco_tese <- read_excel("banco.tese.geissa.FEV.2023.xlsx", 
                         sheet = "Deputados e Deputadas")
View(banco_tese)

#Banco final com as alterações, dar o mesmo nome na hora
#de abrir
banco_tese <- read_excel("banco_tese2023.xlsx")
View(banco_tese)


#Exigências
#Regressão logística 

#Vd:Aprovação - recodificado para AproNum

# as variáveis independentes devem ser intervalares ou 
# dicotômicas todos os preditores relevantes são incluídos, 
#os irrelevantes são  excluídos e o relacionamento é linear
# não existe correlação entre o erro e as variáveis 
# independentes
# não existe perfeita multicolinearidade entre os preditores
# Portanto, não são exigidas a normalidade dos erros e a 
# homogeneidade da variança.

table(banco_tese$Aprovado)

# Não   Sim 
# 63117  1074
#rodar####
library(memisc)
banco_tese$AproNum <-  recode(banco_tese$Aprovado,
  0 <- "Não", 1 <- "Sim")
table(banco_tese$AproNum)
# 1     2 
# 63117  1074 


#Variáveis independentes

table(banco_tese$Sexo)

banco_tese$Sexo
banco_tese$Seniority.anos #tempo da legislatura em anos
banco_tese$Seniority.legislatura #Qual é a legislatura (numérica)
banco_tese$Mesa.diretora  #Se faz parte ou não
banco_tese$Cargo.mesa.diretora #esta é categorica 
banco_tese$Presidencia.comissao.espe #está num cargo de presidencia
banco_tese$Cargo.chave.geral #Se ocupa um cargo chave
banco_tese$Qual.cargo.chave.geral #esta é categorica 
banco_tese$Mais.um.cargo

#pacote para criação das tabelas dos modelos
#install.packages("sjPlot")
library(sjPlot)

#MOD1####
#Testando apenas com a variável sexo

modelo <- glm(AproNum ~ Sexo, data = banco_tese, 
              family = binomial(link = logit))

tab_model(modelo, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#MOD 2####
#Testando com todas as v independentes

banco_tese$Sexo#1
banco_tese$Seniority.anos#2
banco_tese$Seniority.legislatura#3
banco_tese$Mesa.diretora#4
banco_tese$Cargo.mesa.diretora#5 
banco_tese$Presidencia.comissao.espe#6
banco_tese$Cargo.chave.geral#7
banco_tese$Qual.cargo.chave.geral#8 
banco_tese$Mais.um.cargo#9

modelo <- glm(AproNum ~ Sexo + Seniority.anos +
                Seniority.legislatura +
                Mesa.diretora+ 
                Cargo.mesa.diretora+
                Presidencia.comissao.espe+
                Cargo.chave.geral+
                Qual.cargo.chave.geral+
                Mais.um.cargo, data = banco_tese, 
              family = binomial(link = logit))

tab_model(modelo, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#MOD 3####
#Modelo com a exclusão de algumas variáveis
#categoricas 

banco_tese$Sexo
banco_tese$Seniority.anos
banco_tese$Seniority.legislatura
banco_tese$Mesa.diretora
banco_tese$Cargo.mesa.diretora 
banco_tese$Presidencia.comissao.espe
banco_tese$Cargo.chave.geral
banco_tese$Qual.cargo.chave.geral #essa não foi utilizada
banco_tese$Mais.um.cargo

modelo <- glm(AproNum ~ Sexo + Seniority.anos +
                Seniority.legislatura +
                Mesa.diretora+
                Presidencia.comissao.espe+
                Cargo.chave.geral+
                Mais.um.cargo, data = banco_tese, 
              family = binomial(link = logit))

tab_model(modelo, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#MOD 4####
#Modelo com a exclusão da v sexo.

banco_tese$Seniority.anos
banco_tese$Seniority.legislatura
banco_tese$Mesa.diretora
banco_tese$Cargo.mesa.diretora 
banco_tese$Presidencia.comissao.espe
banco_tese$Cargo.chave.geral
banco_tese$Qual.cargo.chave.geral 
banco_tese$Mais.um.cargo

modelo <- glm(AproNum ~ Seniority.anos +
                Seniority.legislatura +
                Mesa.diretora+
                Presidencia.comissao.espe+
                Cargo.chave.geral+
                Mais.um.cargo, data = banco_tese, 
              family = binomial(link = logit))

tab_model(modelo, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#MOD 5####

banco_tese$Sexo #Não foi utilizada
banco_tese$Seniority.anos 
banco_tese$Seniority.legislatura
banco_tese$Mesa.diretora
banco_tese$Cargo.mesa.diretora 
banco_tese$Presidencia.comissao.espe
banco_tese$Cargo.chave.geral
banco_tese$Qual.cargo.chave.geral 
banco_tese$Mais.um.cargo

modelo <- glm(AproNum ~ Seniority.anos +
                Mesa.diretora+
                Presidencia.comissao.espe+
                Cargo.chave.geral+
                Mais.um.cargo, data = banco_tese, 
              family = binomial(link = logit))

tab_model(modelo, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#QUI-QUADRADO####
#Realização do qui-quadrado para testar a relação entre
#o sexo e a aprovação

tab <- table(banco_tese$Aprovado, banco_tese$Sexo)

#      Feminino Masculino
# Não     5866     57251
# Sim       94       980


 chisq.test(tab)
# 
# 
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tab
# X-squared = 0.30619, df = 1, p-value = 0.58

 #####
 #interpretação para Geissa
 #O qui-quadrado não foi significativo (p<0,05)
 #Realmente não há relação entre sexo e aprovação 
############
 
 
 #ModelosTEMA####
#modelos para verificar se os temas relacionados a mulheres
#está relacionado com o sexo

 #Vd: Tema.mulheres recodificado para TemaM.Num
 #Vi: Sexo

 options(scipen = 999)
 
tab2 <- table(banco_tese$Tema.mulheres,
              banco_tese$Sexo)
chisq.test(tab2) 

#rodar####
table(banco_tese$Tema.mulheres)
# Nao   Sim 
# 63222   969 

banco_tese$TemaM.Num <-  recode(banco_tese$Tema.mulheres,
                  0 <- "Nao", 1 <- "Sim")

table(banco_tese$TemaM.Num)
# 0        1 
# 63222   969 

modelo1 <- glm(TemaM.Num ~ Sexo,
              data = banco_tese, 
              family = binomial(link = logit))

tab_model(modelo1, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")


#Apresentação do resultado de maneira gráfica
#install.packages("coefplot")
library(coefplot)
#Coefplot####

table(banco_tese$Sexo)
# Feminino Masculino 
# 5960     58231

#banco_tese$Sexo <- recode(banco_tese$Sexo, 
#              0 <- "Feminino",
#              1 <- "Masculino")


obj1 <- coefplot(modelo1, title = "Tema mulher",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = F,
                 innerCI = 3,
                 intercept = F)

obj1 + theme_classic() + 
  geom_point(size=3, pch=21, fill="red",
            alpha=105) +



theme(text = element_text(family = "serif", size = 13),
      rect = element_blank(),
      panel.grid = element_blank(),
      title = element_text(color = "black"),
      axis.line = element_line(color = "black")) +
  theme(legend.position="none")

#Salvando o banco com as modificações 

#install.packages("writexl")
library(writexl)
write_xlsx(banco_tese, "banco_tese2023.xlsx")

#2023 testes adjacentes####
library(memisc)

banco_tese$Sexo
banco_tese$Seniority.anos #tempo da legislatura em anos
banco_tese$Seniority.legislatura #Qual é a legislatura (numérica)
banco_tese$Mesa.diretora  #Se faz parte ou não
banco_tese$Cargo.mesa.diretora #esta é categorica 
banco_tese$Presidencia.comissao.espe #está num cargo de presidencia
banco_tese$Cargo.chave.geral #Se ocupa um cargo chave
banco_tese$Qual.cargo.chave.geral #esta é categorica 
banco_tese$Mais.um.cargo


#resultados estranhos####
#retirando os NA dos casos que deram resultados 
#estranhos
library(tidyverse)
banco_tese <-  drop_na(banco_tese, Mais.um.cargo)
banco_tese <-  drop_na(banco_tese, Cargo.mesa.diretora)

#omitindo os NA de toda a base 
banco_tese <- na.omit(banco_tese)

#MOD1.1####

modelo <- glm(AproNum ~ Sexo + Seniority.anos +
                Seniority.legislatura +
                Mesa.diretora+ 
                Cargo.mesa.diretora+
                Presidencia.comissao.espe+
                Cargo.chave.geral+
                Qual.cargo.chave.geral+
                Mais.um.cargo, data = banco_tese, 
              family = binomial(link = logit))

library(sjPlot)

tab_model(modelo, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#Quando fiz inicialmente a análise, não tive a preocupação 
#de organizar os levels, pq afinal nada havia dado significativo 
#Mas em razão desse número alto q não estamos entendendo
#irei arrumar os levels

table(banco_tese$Cargo.mesa.diretora)
# 1º Secretário 1º Vice-Presidente      2º Secretário 2º Vice-Presidente 
# 43                 54                281                 70 
# 3º Secretário      4º Secretário                 NA                Nao 
# 121                124              13661              47794 

banco_tese$Cargo.mesa.diretora2 <- 
  factor(banco_tese$Cargo.mesa.diretora, 
        levels = c("1º Vice-Presidente", 
                   "2º Vice-Presidente",
                   "1º Secretário",
                   "2º Secretário",
                   "3º Secretário",
                   "4º Secretário",
                   "NA",
                   "Nao"))
levels(banco_tese$Cargo.mesa.diretora2)

banco_tese$Cargo.mesa.diretora3 <- recode(
  banco_tese$Cargo.mesa.diretora2, 4 <- c("1º Vice-Presidente", 
                                          "2º Vice-Presidente"),
  3 <- c("1º Secretário",
         "2º Secretário"),
  2 <- c("3º Secretário",
         "4º Secretário"),
  1 <- c("NA",
         "Nao"))

table(banco_tese$Cargo.mesa.diretora3)

banco_tese$Cargo.mesa.diretora3 <-  as.numeric(
  banco_tese$Cargo.mesa.diretora3)

table(banco_tese$Mais.um.cargo)
# NA   Nao   Sim 
# 13661 48323   164
banco_tese$Mais.um.cargo <- 
  factor(banco_tese$Mais.um.cargo, 
         levels = c("Sim",
                    "Nao"))
levels(banco_tese$Mais.um.cargo)
#[1] "Sim" "Nao"

#MOD 2.1####
modeloOrg <- glm(AproNum ~ Sexo + Seniority.anos +
                Seniority.legislatura +
                Mesa.diretora+ 
                Cargo.mesa.diretora3 +
                Presidencia.comissao.espe+
                Cargo.chave.geral+
                Qual.cargo.chave.geral+
                Mais.um.cargo, data = banco_tese, 
              family = binomial(link = logit))

tab_model(modeloOrg, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")


#arrumar qual cargo chave e  mesa diretora
table(banco_tese$Mesa.diretora)
# NA   Nao   Sim 
# 13661 47794   693 

levels(banco_tese$Mesa.diretora)

banco_tese$Mesa.diretora2 <- 
  factor(banco_tese$Mesa.diretora, 
         levels = c("NA", 
                    "Sim",
                    "Nao"))
levels(banco_tese$Mesa.diretora2)

banco_tese$Mesa.diretora2 <-  
  as.factor(banco_tese$Mesa.diretora2)

banco_tese$Mesa.diretora2 <-  recode(
  banco_tese$Mesa.diretora2,
  0 <-"NA",
  1 <- "Sim",
  2 <- "Nao")

#Vou recodificar primeiro essa variável
table(banco_tese$Qual.cargo.chave.geral)

banco_tese$Cargo.chave.geral2 <- 
recode(banco_tese$Qual.cargo.chave.geral,
       "Não/NA" <- c("NA", "Nao"),
       "Secretários" <-  c("1º Secretário",
                           "2º Secretário",
                           "3º Secretário",
                           "4º Secretário"),
       "Vice-presidentes" <-  c("1º Vice-Presidente",
                                "2º Vice-Presidente"),
       "Presidentes" <- c("Pres.comissao.esp",
                          "Pres.comissao.perm",
                          "Pres.comissao.perm/pres.comissao.esp"))
table(banco_tese$Cargo.chave.geral2)
levels(banco_tese$Cargo.chave.geral2)

#MOD 3.1####

modeloOrg2 <- glm(AproNum ~ Sexo + Seniority.anos +
                   Seniority.legislatura +
                   Mesa.diretora2+ 
                   Cargo.mesa.diretora3 +
                   Presidencia.comissao.espe+
                   Cargo.chave.geral+
                   Cargo.chave.geral2+
                   Mais.um.cargo, data = banco_tese, 
                 family = binomial(link = logit))

tab_model(modeloOrg2, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#INSERÇÃO DAS NOVAS VARIÁVEIS####
banco_tese$Coalizao
banco_tese$Qual.coalizao

#Mod com as 2 novas variáveis#####
model <- glm(AproNum ~ Sexo + Seniority.anos +
                    Seniority.legislatura +
                    Mesa.diretora2+ 
                    Cargo.mesa.diretora3 +
                    Presidencia.comissao.espe+
                    Cargo.chave.geral+
                    Cargo.chave.geral2+
                    Mais.um.cargo+
                    Qual.coalizao +
                    Coalizao, 
                  data = banco_tese, 
                  family = binomial(link = logit))

tab_model(model, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#Mod só com a coalização#####
model <- glm(AproNum ~ Sexo + Seniority.anos +
               Seniority.legislatura +
               Mesa.diretora2+ 
               Cargo.mesa.diretora3 +
               Presidencia.comissao.espe+
               Cargo.chave.geral+
               Cargo.chave.geral2+
               Mais.um.cargo+
               Coalizao, 
             data = banco_tese, 
             family = binomial(link = logit))

tab_model(model, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#Recodificar coalizão
table(banco_tese$Coalizao)

# NA   Nao   Sim 
# 7405 24192 28295 
banco_tese$Coalizao2 <-  recode(
  banco_tese$Coalizao, 0 <- "NA",
  1 <- "Sim",
  2 <- "Nao")
banco_tese$Coalizao2 <-  as.numeric(banco_tese$Coalizao2)

model <- glm(AproNum ~ Sexo + Seniority.anos +
               Seniority.legislatura +
               Mesa.diretora2+ 
               Cargo.mesa.diretora3 +
               Presidencia.comissao.espe+
               Cargo.chave.geral+
               Cargo.chave.geral2+
               Mais.um.cargo+
               Coalizao2, 
             data = banco_tese, 
             family = binomial(link = logit))

tab_model(model, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#Mod só com o tipo de coalização#####
model <- glm(AproNum ~ Sexo + Seniority.anos +
               Seniority.legislatura +
               Mesa.diretora2+ 
               Cargo.mesa.diretora3 +
               Presidencia.comissao.espe+
               Cargo.chave.geral+
               Cargo.chave.geral2+
               Mais.um.cargo+
               Qual.coalizao, 
             data = banco_tese, 
             family = binomial(link = logit))

tab_model(model, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#Como deu significativo, vamos agregar pra ver se 
#melhora os resultados 
table(banco_tese$Qual.coalizao)

banco_tese$CoalizaçãoAgregada <- recode(
  banco_tese$Qual.coalizao, 
  "Sarney" <- c("Sarney 1", "Sarney 2"),
  "Color" <-  c("Collor 1", "Collor 2", "Collor 3",
                "Collor 4"),
  "FHC" <-  c("FHC I 1", "FHC I 2", "FHC II 1",
              "FHC II 2"),
  "Lula" <- c("Lula I 1", "Lula I 2", "Lula I 3",
              "Lula I 4", "Lula I 5", "Lula II 1", 
              "Lula II 2", "Lula II 3"),
  "Dilma" <- c("Dilma I 1",   "Dilma I 2",  
               "Dilma I 3", "Dilma II 1", "Dilma II 2"),
  "Temer" <- c(" Temer 1",  "Temer 2", "Temer 3"),
  "Bolsonaro" <- c("Bolsonaro 1", "Bolsonaro 2")
)

table(banco_tese$CoalizaçãoAgregada)
# Sarney     Color       FHC      Lula     Dilma     Temer Bolsonaro 
# 4127      4068     10827     10868     11121      3187     11914


#Mod coalização agregada#####
model <- glm(AproNum ~ Sexo + Seniority.anos +
               Seniority.legislatura +
               Mesa.diretora2+ 
               Cargo.mesa.diretora3 +
               Presidencia.comissao.espe+
               Cargo.chave.geral+
               Cargo.chave.geral2+
               Mais.um.cargo+
               CoalizaçãoAgregada, 
             data = banco_tese, 
             family = binomial(link = logit))

tab_model(model, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#Mod com a coalização original + partido#####
banco_tese$Partido

model <- glm(AproNum ~ Sexo + Seniority.anos +
               Seniority.legislatura +
               Mesa.diretora2+ 
               Cargo.mesa.diretora3 +
               Presidencia.comissao.espe+
               Cargo.chave.geral+
               Cargo.chave.geral2+
               Mais.um.cargo+
               Qual.coalizao+
               Partido, 
             data = banco_tese, 
             family = binomial(link = logit))

tab_model(model, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

summary(model)

#Mod retirando a seniority anos#####

model <- glm(AproNum ~ Sexo + 
               Seniority.legislatura +
               Mesa.diretora2+ 
               Cargo.mesa.diretora3 +
               Presidencia.comissao.espe+
               Cargo.chave.geral+
               Cargo.chave.geral2+
               Mais.um.cargo+
               Qual.coalizao,
             data = banco_tese, 
             family = binomial(link = logit))

tab_model(model, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#Mod retirando a seniority legislatura#####

model <- glm(AproNum ~ Sexo + 
               Seniority.anos +
               Mesa.diretora2+ 
               Cargo.mesa.diretora3 +
               Presidencia.comissao.espe+
               Cargo.chave.geral+
               Cargo.chave.geral2+
               Mais.um.cargo+
               Qual.coalizao,
             data = banco_tese, 
             family = binomial(link = logit))

tab_model(model, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#Mod apenas com sexo e coalização#####

model <- glm(AproNum ~ Sexo + 
               Qual.coalizao,
             data = banco_tese, 
             family = binomial(link = logit))

tab_model(model, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#Mod apenas com sexo e coalização#####

model <- glm(AproNum ~ Sexo + 
               Seniority.anos +
               Seniority.legislatura +
               Qual.coalizao,
             data = banco_tese, 
             family = binomial(link = logit))

tab_model(model, show.ci = F, auto.label = T, 
          show.se = F, collapse.ci = F, 
          wrap.labels = 60, p.style = "stars")

#Salvando o banco com as modificações 

#install.packages("writexl")
library(writexl)
write_xlsx(banco_tese, "banco_tese2023.xlsx")
