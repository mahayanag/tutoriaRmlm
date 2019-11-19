###############################################
### Pratica: modelos lineares generalizados ###
###############################################


## pacotes

library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)

# importar conjunto de dados

pronome = read.csv("dados/pronome.csv")

# filtrar dados para excluir nivel ambiguo

pronome = pronome%>%
  filter(interpretacao == "alvo" | interpretacao == "fonte")%>%
  droplevels()

# Inspecao de dados

pronome%>%
  group_by(classe, aspecto, interpretacao)%>%
  summarise(n = n())

pronome%>%
  group_by(classe, aspecto, interpretacao)%>%
  summarise(n = n())%>%
  mutate(freq = n / sum(n))

# Inspecao grafica

pronome%>%
  group_by(classe, aspecto, interpretacao)%>%
  summarise(n = n())%>%
  mutate(frequencia = n / sum(n))%>%
  ggplot(., aes(x = aspecto, y= frequencia, fill = interpretacao))+
  scale_y_continuous(labels=scales::percent)+
  geom_col(position = "dodge")+
  theme_bw()+
  facet_wrap(~ classe)


# ajustando e conferindo sum coding para variavel aspecto

contrasts(pronome$aspecto) = c(-0.5, 0.5)
contrasts(pronome$aspecto)

# ajustando e conferindo sum coding para variavel classe

contrasts(pronome$classe) = c(-0.5,0.5)
contrasts(pronome$classe)


## ajustando modelo com aspecto*classe; intercepto aleatorio por participante e item, slope para aspecto para item; slope de aspecto e classe, bem como interacao, para participante (problema de convergência)

modelo.aspecto.int.classe = glmer(interpretacao ~ aspecto*classe + (1+aspecto|item) + (1+aspecto*classe|participante), data=pronome, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

# eliminando slopes de participante (problemas de convergência)

modelo.aspecto.int.classe = glmer(interpretacao ~ aspecto*classe + (1+aspecto|item) + (1|participante), data=pronome, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

# eliminando slopes de item (converge)

modelo.aspecto.int.classe = glmer(interpretacao ~ aspecto*classe + (1|item) + (1|participante), data=pronome, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))


# modelo aninhado sem interacao

modelo.aspecto.classe = glmer(interpretacao ~ aspecto+classe + (1|item) + (1|participante), data=pronome, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

# comparacao modelos aninhados para testar interacao

anova(modelo.aspecto.classe, modelo.aspecto.int.classe)

    ##resultado: interacao nao significativo

# modelo aninhado sem aspecto

modelo.classe = glmer(interpretacao ~ classe + (1|item) + (1|participante), data=pronome, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

# comparacao modelos aninhados para testar aspecto

anova(modelo.classe, modelo.aspecto.classe)

    ##resultado: aspecto significativo

# modelo aninhado sem classe

modelo.aspecto = glmer(interpretacao ~ aspecto + (1|item) + (1|participante), data=pronome, family=binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

# comparacao modelos aninhados para testar classe

anova(modelo.aspecto, modelo.aspecto.classe)

    ##resultado: classe significativo

## conferindo resultado do melhor modelo ajustado

summary(modelo.aspecto.classe)
    ## resultado aspecto1: mais fonte para aspecto imperfectivo; 
    ## resultado classe1: mais fonte para classe 3

## Nao seria necessario fazer analise post-hoc visto que nao temos interacoes e a tabela de coeficientes ja mostra efeitos principais, mas podemos se quisermos comparar os niveis par-a-par

## analise post-hoc

ph = emmeans(modelo.aspecto.classe, ~ aspecto+classe)

pairs(ph, adjust="tukey")

## RESPOSTA: Ajustamos uma regressao logistica com interpretacao do pronome como variavel resposta, aspecto, classe e a interacao desses dois fatores como efeitos fixos e interceptos aleatorios por participantes e itens. Os efeitos fixos foram contrastados usando *sum coding* (0.5, -0.5) para reportar efeitos principais. Uma comparacao com modelos aninhados indicou que a interacao entre aspecto e classe nao contribui significativamente para o modelo (Chisq = 2.94, p = 0.086), mas que aspecto (Chisq = 56.025, p < 0.0001) e classe (Chisq = 10.626, p = 0.001) têm efeito significativo na distribuicao do conjunto de dados. Os coeficientes do melhor modelo ajustado podem ser vistos na Tabela X:

# [e aqui inseririamos a tabela de coeficientes]
    