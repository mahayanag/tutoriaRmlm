######################################################
### TUTORIAL PARA SECAO 2: MODELOS LINEARES MISTOS ###
######################################################


#instalar pacote novo para a secao

install.packages("yarrr")

#carregar pacotes

library(ggplot2)
library(dplyr)

##############################################################
### 2.1 Modelos lineares: sintaxe em R e conceitos basicos ###
##############################################################

# Criar conjunto de dados de Winter (2013)

## criando vetor idade
idade = c(14,23,35,48,52,67)

##criando vetor pitch
pitch = c(252,244,240,233,212,204)

## combinando os dois vetores em um dataframe
my.df = data.frame(idade,pitch)

## vendo o dataframe

head(my.df)

## Criando um modelo linear de pitch em funcao de idade

modelo.idade = lm(pitch ~ idade, my.df)




## ver os nomes dos objetos contidos no objeto modelo.idade

names(modelo.idade)

## ver o que ha em fitted.values

modelo.idade$fitted.values

## ver o que ha em residuals

modelo.idade$residuals


# buscando o sumario dos valores do modelo linear

summary(modelo.idade)

############################################
### 2.2 Os coeficientes do modelo linear ###
############################################

# buscando o sumario dos valores do modelo linear

summary(modelo.idade)


# pessoa com um ano

267.0765 - 0.9099

# pessoa com dois anos

267.0765 - (0.9099*2)

# pessoa com tres anos

267.0765 - (0.9099*3)

# pessoa com quatorze anos

267.0765 - (0.9099*14)

# buscando os valores ajustados no modelo (primeiro dado corresponde a individuo de 14 anos)

modelo.idade$fitted

##########################################################
### 2.3 Modelos lineares com efeitos fixos categoricos ###
##########################################################

#Criando dataframe para testar se pitch ~ sexo

## vetor com dados de pitch
pitch = c(233,204,242,130,112,142)


## vetor com dados de sexo
sexo = c(rep("feminino",3),rep("masculino",3))

## combinando os dois vetores em um dataframe
my.df2 = data.frame(sexo,pitch)

## vendo o dataframe

head(my.df2)

# ajustar o modelo

modelo.sexo = lm(pitch ~ sexo, my.df2)

# resultados

summary(modelo.sexo)

## medias de pitch por sexo

my.df2%>%
  group_by(sexo)%>%
  summarize(mean(pitch))

# sobrescrever a coluna sexo com um novo nivel de referencia

my.df2$sexo = relevel(my.df2$sexo, ref = "masculino")

# ajustar o modelo

modelo.sexo2 = lm(pitch ~ sexo, my.df2)

# resultados

summary(modelo.sexo2)


#####################################################
### 2.5 Modelos lineares com mais de uma variavel ###
#####################################################

## conhecendo o conjunto de dados

head(diamonds)

## conhecendo o conjunto de dados

str(diamonds)


## construindo o modelo

modelo.diamonds = lm(value ~ weight + clarity, data = diamonds)

## resultados

summary(modelo.diamonds)


# calculando valor estimado para o primeiro diamante da tabela

y = 145.446 + (2.219*9.35) + (22.036*0.88)

y


## valores ajustados do modelo

head(modelo.diamonds$fitted.values)


###############################
### 2.6 Hipoteses do modelo ###
###############################

# dataframe com os residuos e fitted values do modelo

ajustados = modelo.diamonds$fitted.values
residuos = modelo.diamonds$residuals

aj.residuos = data.frame(ajustados, residuos)

# analisando os residuos

ggplot(aj.residuos, aes(x = ajustados, y = residuos))+
  geom_point(size=2)


# histograma para normalidade de residuos

ggplot(aj.residuos, aes(x = residuos))+
  geom_histogram()

# boxplot para normalidade de residuos

ggplot(aj.residuos, aes(y=residuos))+
  geom_boxplot()


# normalidade de residuos: qqplot

ggplot(aj.residuos, aes(sample=residuos))+
  stat_qq()+
  stat_qq_line()


