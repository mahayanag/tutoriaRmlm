######################################################
### TUTORIAL PARA SECAO 4: MODELOS LINEARES MISTOS ###
######################################################

## instalar pacote

install.packages("emmeans")

## Carregar pacotes para a secao

library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)

## pacote emmeans sera carregado posteriormente

#######################################################
### 4.1 Tabela de coeficientes em MLMs com interacao###
#######################################################


#importar conjunto de dados

priming2 <- read.csv("dados/priming2.csv")

#inspecionar dados

head(priming2)

#conhecer variaveis

str(priming2)

## Conferindo distribuicao de dados

ggplot(priming2, aes(sample = tempo))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~prime)


## Selecionando apenas resposta cujo tempo esteja entre 200 ms e 1500 ms e que estejam marcadas como correct para a variavel resposta

priming2 = priming2%>%
  filter(tempo > 200 & tempo < 1500 & resposta == "correct")

## Conferindo novamente a distribuicao dos dados

ggplot(priming2, aes(sample = tempo))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~prime)

## Vendo a media (em ms) dos tempos de reacao para as quatro condicoes

priming2%>%
  group_by(prime, SOA)%>%
  summarise(mean(tempo))


## Conferindo distribuicao de dados por participante

ggplot(priming2, aes(sample = tempo))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~participante)


# construcao do modelo completo (erro de convergencia)

model.full <- lmer(tempo ~ prime*SOA + (1+prime*SOA|participante) + (1+prime*SOA|item), data = priming2, REML=FALSE)

# remocao de slopes para participante (erro de convergencia)

model.full <- lmer(tempo ~ prime*SOA + (1|participante) + (1+prime*SOA|item), data = priming2, REML=FALSE)

# remocao de slopes para item

model.full <- lmer(tempo ~ prime*SOA + (1|participante) + (1|item), data = priming2, REML=FALSE)


# modelo sem interacao

model.prime.SOA <- lmer(tempo ~ prime+SOA + (1|participante) + (1|item), data = priming2, REML=FALSE)

# comparacao de modelos aninhados

anova(model.full, model.prime.SOA)

## Resultado: interacao eh significativo


# media por prime

priming2%>%
  group_by(prime)%>%
  summarise(media = mean(tempo))
            
# media por SOA

priming2%>%
  group_by(SOA)%>%
  summarise(media = mean(tempo))

# media por prime x SOA

priming2%>%
  group_by(SOA, prime)%>%
  summarise(media = mean(tempo))%>%
  mutate_if(is.numeric, format, 1) ## retorna valores com 4 digitos depois do ponto

# coeficientes do modelo

summary(model.full)


######################
### 4.3 Contrastes ###
######################

# conferindo sum coding para variavel priming

contrasts(priming2$prime)

# conferindo sum coding para variavel SOA

contrasts(priming2$SOA)


# ajustando e conferindo sum coding para variavel priming

contrasts(priming2$prime) = c(-0.5, 0.5)
contrasts(priming2$prime)

# ajustando e conferindo sum coding para variavel SOA

contrasts(priming2$SOA) = c(-0.5,0.5)
contrasts(priming2$SOA)

# ajustando novamente o modelo linear com os novos contrastes

model.full <- lmer(tempo ~ prime*SOA + (1|participante) + (1|item), data = priming2, REML=FALSE)

## resultado

summary(model.full)


## media de tempo de resposta

mean(priming2$tempo)


# media por prime

priming2%>%
  group_by(prime)%>%
  summarise(media = mean(tempo))

# beta de prime1

708 - 668


# media por SOA

priming2%>%
  group_by(SOA)%>%
  summarise(media = mean(tempo))

# beta de SOA1

700 - 677

# media por prime x SoA

priming2%>%
  group_by(SOA, prime)%>%
  summarise(media = mean(tempo))%>%
  mutate_if(is.numeric, format, 1)

## calculando diferencas

# SOAcurto nrelacionado - SOAcurto relacionado

efeito.curto = 706.2971 - 693.9389

# SOAlongo nrelacionado - SOAlongo relacionado

efeito.longo = 710.2517 - 643.2373

# Diferenca entre os valores

efeito.curto - efeito.longo



#############################
### 4.4 Analises post-hoc ###
#############################

# carregar emmeans

library(emmeans)


# analise post-hoc

post.hoc = emmeans(model.full, ~ prime*SOA)

# visualizacao dos pares da analise post-hoc

pairs(post.hoc, adjust="tukey")


