######################################################
### TUTORIAL PARA SECAO 5: MODELOS LINEARES MISTOS ###
######################################################

## Carregar pacotes para a secao

library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)

##########################################
### 5.3 Regressao logistica e log-odds ###
##########################################



#importar conjunto de dados

pronome = read.csv("dados/pronome.csv")

# inspecionar conjunto de dados

head(pronome)

# inspecionar conjunto de dados 2

str(pronome)


# filtragem para considerar apenas casos suja resposta tenha sido alvo ou fonte

pronome = pronome%>%
  filter(interpretacao == "alvo" | interpretacao == "fonte")%>%
  droplevels()


# tabela com contagem de pronome por aspecto

pronome%>%
  group_by(aspecto, interpretacao)%>%
  summarise(n = n())

# tabela com frequencia relativa de pronome por aspecto

pronome%>%
  group_by(aspecto, interpretacao)%>%
  summarise(n = n())%>%
  mutate(freq = n / sum(n))


## grafico da relacao entre aspecto x interpretacao

pronome%>%
  group_by(aspecto, interpretacao)%>%
  summarise(n = n())%>%
  mutate(frequencia = n / sum(n))%>%
  ggplot(., aes(x = aspecto, y= frequencia, fill = interpretacao))+
  scale_y_continuous(labels=scales::percent)+
  geom_col(position = "dodge")+
  theme_bw()


# criando um modelo linear generalizado

modelo.aspecto = glm(interpretacao ~ aspecto, pronome, family = binomial)

# vendo o resultado do modelo

summary(modelo.aspecto)


# consultando niveis e o valor de referencia

levels(pronome$interpretacao)


# calculando o logaritimo da chance fonte:alvo para aspecto imperfectivo

log(174/85)


# calculando a chance (odds ratio) de fonte:alvo para imperfectivo a partir da funcao exponencial

exp(0.716404)


# calculando a chance de fonte:alvo no perfectivo em comparacao ao intercept

exp(-1.124980)

# calculando a chance de alvo:fonte no perfectivo em comparacao ao intercept

exp(1.124980)

#########################################################
### 5.4 Modelos Lineares Generalizados Mistos (MLGMs) ###
#########################################################


# construindo um MLGM com interceptos aleatorios por item e participantes e slopes aleatorios por aspecto para item e participante (problema de convergencia)

modelomisto.aspecto = glmer(interpretacao ~ aspecto + (1+aspecto|item) + (1+aspecto|participante), data=pronome, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

# retirando slope de aspecto por item (problema de convergencia)

modelomisto.aspecto = glmer(interpretacao ~ aspecto + (1+aspecto|item) + (1|participante), data=pronome, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

# retirando slope de aspecto por participante (problema de convergencia)

modelomisto.aspecto = glmer(interpretacao ~ aspecto + (1+aspecto|item) + (1|participante), data=pronome, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

# retirando slope de aspecto por item

modelomisto.aspecto = glmer(interpretacao ~ aspecto + (1|item) + (1|participante), data=pronome, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

# resultado do MLGM
summary(modelomisto.aspecto)


# modelo sem aspecto como efeito fixo 

modelomisto.null = glmer(interpretacao ~ 1 + (1|item) + (1|participante), data=pronome, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

# comparacao de modelos

anova(modelomisto.aspecto, modelomisto.null)

