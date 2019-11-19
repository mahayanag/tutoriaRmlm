######################################################
### TUTORIAL PARA SECAO 3: MODELOS LINEARES MISTOS ###
######################################################

 
 # baixar pacotes (esses pacotes serao carregados mais adiante)
 
 install.packages("lme4")
 install.packages("lmerTest")
 
 #carregar pacotes
 
 library(lme4)
 library(ggplot2)
 library(dplyr)
 
 # o pacote lmerTest sera carregado posteriormente

##################################################
### 3.1 Medidas repetidas e efeitos aleatorios ###
##################################################
 
#importar conjunto de dados

priming <- read.csv("dados/priming.csv")

#inspecionar dados

head(priming)

#conhecer variaveis

str(priming)


## Conferindo distribuicao de dados

ggplot(priming, aes(sample = tempo))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~prime)



##########################################
### 3.1.1 Inspecao e limpeza dos dados ###
##########################################

## Selecionando apenas resposta cujo tempo esteja entre 200 ms e 1500 ms e que estejam marcadas como correct para a variavel resposta

priming = priming%>%
  filter(tempo > 200 & tempo < 1500 & resposta == "correct")

## Conferindo novamente a distribuicao dos dados

ggplot(priming, aes(sample = tempo))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~prime)

## Vendo a media (em ms) dos tempos de reacao para os dois niveis da variavel prime

priming%>%
  group_by(prime)%>%
  summarise(mean(tempo))


##################################################################
### 3.1.2 Modelo linear misto: interceptos e slopes aleatorios ###
##################################################################

## Conferindo distribuicao de dados por participante

ggplot(priming, aes(sample = tempo))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~participante)

# Calculo da media por participante

priming%>%
  group_by(participante)%>%
  summarise(media = mean(tempo))%>%
  arrange(desc(media))

####################################################
### 3.1.3 Modelos lineares mistos no pacote lme4 ###
####################################################


# carregando o pacote lme4

library(lme4)

# ajustando modelo linear com interceptos aleatorios por item e participante e slopes aleatorios por prime

modelo.prime <- lmer(tempo ~ prime + (1+prime|participante) + (1+prime|item), data = priming)

# ajustanto o modelo linear: retira slope aleatorio de prime por participante (continua problema de convergencia) 

modelo.prime <- lmer(tempo ~ prime + (1|participante) + (1+prime|item), data = priming)

# ajustanto o modelo linear: sem slopes aleatorios 

modelo.prime <- lmer(tempo ~ prime + (1|participante) + (1|item), data = priming)

# resultado de modelo.prime

summary(modelo.prime)


# carregando o pacote lmerTest

library(lmerTest)

# ajustanto o modelo linear 

modelo.prime <- lmer(tempo ~ prime + (1|participante) + (1|item), data = priming)

# resultado de modelo.prime

summary(modelo.prime)

############################################
### 3.2 Comparacao por modelos aninhados ###
############################################


#criando modelo completo e modelo aninhado

modelo.nulo  <- lmer(tempo ~ 1 + (1|participante) + (1|item), data = priming, REML = FALSE)

modelo.prime <- lmer(tempo ~ prime + (1|participante) + (1|item), data = priming, REML = FALSE)

anova(modelo.nulo, modelo.prime)

#################################################################
### 3.3 Modelos lineares mistos com duas variaveis e interacao ##
#################################################################

# relembrando conjunto de dados

head(priming)


# construcao do modelo completo (erro de convergencia)

model.full <- lmer(tempo ~ prime*SOA + (1+prime*SOA|participante) + (1+prime*SOA|item), data = priming, REML=FALSE)

# remocao de slopes para participante (erro de convergencia)

model.full <- lmer(tempo ~ prime*SOA + (1|participante) + (1+prime*SOA|item), data = priming, REML=FALSE)

# remocao de slopes para item

model.full <- lmer(tempo ~ prime*SOA + (1|participante) + (1|item), data = priming, REML=FALSE)

# modelo sem interacao

model.prime.SOA <- lmer(tempo ~ prime+SOA + (1|participante) + (1|item), data = priming, REML=FALSE)

# comparacao de modelos aninhados

anova(model.full, model.prime.SOA)

## Resultado: interacao nao eh significativo


# modelo com prime, sem SOA

model.prime <- lmer(tempo ~ prime + (1|participante) + (1|item), data = priming, REML=FALSE)

# comparacao de modelos aninhados

anova(model.prime.SOA, model.prime)

## Resultado: SOA nao eh significativo


# modelo sem interacao

model.SOA <- lmer(tempo ~ SOA + (1|participante) + (1|item), data = priming, REML=FALSE)

# comparacao de modelos aninhados

anova(model.prime.SOA, model.SOA)

## Resultado: prime eh significativo


# medias dos tempos de reacao

priming%>%
  group_by(prime)%>%
  summarise(mean(tempo))

# coeficientes do melhor modelo ajustado

summary(model.prime)


#############################################################
### 3.5 Distribuicao dos dados: transformacao logaritmica ###
#############################################################


## importando conjunto de dados tempo

tempo = read.csv("dados/tempo.csv")

## inspecionando (apenas 1 variavel numerica chamada tempo)

str(tempo)

## inspecionando distribuicao

ggplot(tempo, aes(sample = tempo))+
  stat_qq(alpha=0.3)+
  stat_qq_line()

ggplot(tempo, aes(x = tempo))+
  geom_histogram()

## criar coluna com valor log(tempo)

tempo$log.tempo = log(tempo$tempo)

## inspecionando a nova coluna

str(tempo)


## inspecionando nova distribuicao

ggplot(tempo, aes(sample = log.tempo))+
  stat_qq(alpha=0.3)+
  stat_qq_line()

ggplot(tempo, aes(x = log.tempo))+
  geom_histogram()


