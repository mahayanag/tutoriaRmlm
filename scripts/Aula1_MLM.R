######################################################
### TUTORIAL PARA SECAO 1: MODELOS LINEARES MISTOS ###
######################################################

###########################
## 1.3 Como o R funciona ##
###########################

# baixar pacotes
 
 install.packages("dplyr")
 install.packages("ggplot2")

# carregar pacotes
 
 library(dplyr)
 library(ggplot2)
 

## Tudo que for precedido pelo simbolo # em uma linha eh classificado como  comentario

## Comentarios nao sao entendidos como codigo, e portanto o programa nao tentara executa-los

## Essa linha eh um comentario. A linha abaixo eh um comando. Deixe o cursor (aquela barrinha vertical que fica piscando) na linha do comando que deseja executar e aperte CTRL+ENTER. Tente fazer isso com o comando da linha abaixo.

2+2


# calculando a raiz quadrada de 4

sqrt(4)

# criando um vetor com 4 valores

idade <- c(34, 35, 39, 42)

# criar um vetor com 4 nomes

nomes <- c("Jose", "Maria", "Ana", "Pedro")

# extraindo a media de idade

mean(idade)

# extraindo a mediana de idade

median(idade)

# extraindo o desvio-padrao de idade

sd(idade)

# extraindo a media de nomes

mean(nomes)

# extraindo a mediana de nomes

median(nomes)

# extraindo o desvio-padrao de nomes

sd(nomes)

# criando um objeto com os resultados de uma funcao

media = mean(idade)
mediana = median(idade)
desvio.padrao = sd(idade)

# inspecionando os objetos criados

media
mediana
desvio.padrao

########################################
## 1.4 Manipulando conjuntos de dados ##
########################################

# criando um dataframe

df = data.frame(nomes, idade)

# media de idade

median(df$idade)


# criando o conjunto de dados chamado linguistas.total com todos os dados da planilha linguistas.csv

linguistas.total = read.csv("dados/linguistas.csv")

# Use a funcao head() para ver as linhas iniciais do conjunto de dados

head(linguistas.total)

## Use a funcao str() para conhecer o conjunto de dados (informacao sobre numero de niveis, se o vetor eh numerico ou nao)

str(linguistas.total)

# Clique no conjunto na area Environment para abrir o conjunto de dados em forma de planilha

# usando a funcao unique para ver todos os valores unicos da coluna *instituicao* no conjunto de dados

unique(linguistas.total$instituicao)

# usando a funcao unique para ver todos os valores unicos da coluna *status* no conjunto de dados

unique(linguistas.total$status)

# selecionando apenas as colunas de interesse para o projeto

linguistas.total%>%
  select(genero, idade, altura, peso, tamanho.pe, instituicao, dialeto, deslocamento, status, esporte)

# selecionando apenas participantes que indicaram que trabalham/estudam na Unicamp *OU* na UFMG *OU* na UFRN

# Marcador | indica "ou"

linguistas.total%>%
  filter(instituicao == "UFRN" | instituicao == "UFMG" | instituicao == "Unicamp")

# Selecionando participantes de todas as universidades diferentes de Outra

linguistas.total%>%
  filter(instituicao != "Outra")

# Selecionando as colunas de interesse
# Selecionando participantes de instituicoes diferentes de Outra
# Selecionando participantes com tamanho de calcado menor que 50

linguistas.total%>% 
  select(genero, idade, altura, peso, tamanho.pe, instituicao, dialeto, deslocamento, status, esporte)%>%
  filter(instituicao != "Outra")%>%
  filter(tamanho.pe < 50)


# Criando conjunto de dados

linguistas.edit = linguistas.total%>%
  select(genero, idade, altura, peso, tamanho.pe, instituicao, dialeto, deslocamento, status, esporte)%>%
  filter(instituicao != "Outra")%>%
  filter(tamanho.pe < 70)%>%
  droplevels()

##### Tarefa 1: manipulação do conjunto de dados
 
 # Codigo 1
 linguistas.total%>%
   select(genero, idade, altura, esporte, tamanho.pe, status)%>%
   filter(instituicao != "UFMG")
 
 # Codigo 2
 linguistas.total%>%
   select(genero, idade, altura, instituicao, esporte, status)%>%
   filter(instituicao != "UFMG")
 
################################################################# 
## Dataframes: extraindo informações descritivas de seus dados ##
################################################################# 
 
# Criando tabela com as medias de altura e tamanho de pe a partir do conjunto linguistas.edit

linguistas.edit%>%
summarise(mean(altura),
          mean(tamanho.pe))

# Mesmo resultado, mas com etiquetas
linguistas.edit%>%
summarise(media_altura = mean(altura),
          media_pe     = mean(tamanho.pe))

# Criando tabela agrupando os participantes por instituicao e para extracao das medias de altura e tamanho de pe e do numero de participantes

linguistas.edit%>%
  group_by(instituicao)%>%
  summarise(media_altura = mean(altura),
            media_pe     = mean(tamanho.pe),
            quantidade   = n())


# Criando tabelas agrupando participantes por instituicao e genero para saber o n de cada grupo

linguistas.edit%>%
  group_by(instituicao, genero)%>%
  summarise(qtdd = n())


###### Tarefa: descrição dos dados

# Codigo 1
linguistas.total%>%
  filter(instituicao == "Unicamp")%>%
  group_by(genero)%>%
  summarize(media = mean(altura))

# Codigo 2
linguistas.total%>%
  group_by(status, deslocamento)%>%
  summarise(qtt = n())


#######################
####### GGPLOT ########
#######################


###################################
### 1.5.2 Graficos de dispersão ###
###################################

# criando a estrutura basica do nosso grafico

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))


# adicionando a camada de geometria, indicando geometria de pontos

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point()

# modificando os elementos introduzidos pela camada geometria

# o valor de alpha vai de 0 (completamente transparente) a 1 (completamente opaco)

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 3, alpha = 0.3) 


# agrupando dados por nova variavel na camada de geometria por meio de cor

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 3, alpha = 0.3, aes(color = genero))


# agrupando dados por nova variavel na camada de geometria por meio de formato

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 3, alpha = 0.3, aes(shape = genero))



###### Tarefa: graficos de dispersão
 
 ## 1
 ggplot(linguistas.edit, aes(x = altura, y = peso))+
   geom_point(size = 3, alpha = 0.6, colour = "red")
 
 ## 2



######################
### 1.5.3 Boxplots ###
######################


# Criando um boxplot

ggplot(linguistas.edit, aes(x = genero, y = altura))+
  geom_boxplot()


# personalizando o grafico: note que e possivel informar a cor por sua notacao RGB

ggplot(linguistas.edit, aes(x = genero, y = altura))+
  geom_boxplot(alpha = 0.4, size = 4, color = "#8bd5c4")


# personalizando o grafico: note que eh possivel informar a cor por sua notacao RGB

ggplot(linguistas.edit, aes(x = genero, y = altura))+
  geom_boxplot(fill = "#8bd5c4")


# indicando preenchimento da geometria pela variavel genero

ggplot(linguistas.edit, aes(x = genero, y = altura))+
  geom_boxplot(aes(fill = instituicao))


# Boxplot mostrando altura para cada genero; um painel por instituicao

ggplot(linguistas.edit, aes(x = genero, y = altura))+
  geom_boxplot()+
  facet_wrap(~ instituicao)

# criar graficos nos 6 agrupamentos do cruzamento de instituicao e esporte

# determinar numero de colunas = 2 ('ncol = 2') nos paineis do facet para facilitar comparacao (o default seria 3)

ggplot(linguistas.edit, aes(x = genero, y = altura))+
  geom_boxplot()+
  facet_wrap(~ instituicao + esporte, ncol = 2)


# Criando graficos de dispersao com paineis diferentes para cada genero

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 3, alpha = 0.3)+
  facet_wrap(~ genero)



###### Tarefa: boxplot

 #1

 ggplot(linguistas.edit, aes(x = status, y = idade))+
   geom_boxplot(aes(fill = instituicao))
 
 #2


###################################
##### 1.5.4 Grafico de barras #####
###################################

# Tabela indicando frequencia absoluta por instituicao

linguistas.edit%>%
  group_by(instituicao)%>%
  summarise(quantidade = n())

# Grafico indicando frequencia absoluta por instituicao

linguistas.edit%>%
  group_by(instituicao)%>%
  summarise(quantidade = n())%>%
  ggplot(., aes(x = instituicao, y = quantidade))


# Grafico de barra (coluna) indicando frequencia absoluta por instituicao

linguistas.edit%>%
  group_by(instituicao)%>%
  summarise(quantidade = n())%>%
  ggplot(., aes(x = instituicao, y = quantidade))+
  geom_col()


# Grafico de barra (coluna) indicando frequencia absoluta por instituicao x dialeto

linguistas.edit%>%
  group_by(instituicao, dialeto)%>%
  summarise(quantidade = n())%>%
  ggplot(., aes(x = instituicao, y = quantidade))+
  geom_col()


# Grafico de barra (coluna) indicando frequencia absoluta por instituicao x dialeto com fill

linguistas.edit%>%
  group_by(instituicao, dialeto)%>%
  summarise(quantidade = n())%>%
  ggplot(., aes(x = instituicao, y = quantidade, fill = dialeto))+
  geom_col()


# Grafico de barra (coluna) indicando frequencia absoluta por instituicao x dialeto com fill

linguistas.edit%>%
  group_by(instituicao, dialeto)%>%
  summarise(quantidade = n())%>%
  ggplot(., aes(x = instituicao, y = quantidade, fill = dialeto))+
  geom_col(position = "dodge")


# Grafico de barra (coluna) indicando frequencia absoluta por instituicao x dialeto com fill

linguistas.edit%>%
  group_by(instituicao, dialeto)%>%
  summarise(quantidade = n())%>%
  mutate(frequencia = quantidade/sum(quantidade))


# Grafico de barra (coluna) indicando frequencia relativa por instituicao x dialeto com fill

linguistas.edit%>%
  group_by(instituicao, dialeto)%>%
  summarise(quantidade = n())%>%
  mutate(frequencia = quantidade/sum(quantidade))%>%
  ggplot(., aes(x = instituicao, y = frequencia, fill = dialeto))+
  geom_col(position = "dodge")



# Grafico de barra (coluna) indicando porcentagem por instituicao x dialeto com fill

linguistas.edit%>%
  group_by(instituicao, dialeto)%>%
  summarise(quantidade = n())%>%
  mutate(frequencia = quantidade/sum(quantidade))%>%
  ggplot(., aes(x = instituicao, y = frequencia, fill = dialeto))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels=scales::percent)




##Tarefa: grafico de barras

## 1

linguistas.edit%>%
  group_by(instituicao, status, deslocamento)%>%
  summarise(quantidade = n())%>%
  mutate(frequencia = quantidade/sum(quantidade))%>%
  ggplot(., aes(x = status, y = frequencia, fill = deslocamento))+
  geom_col()+
  scale_y_continuous(labels=scales::percent)+
  facet_wrap(~instituicao)

## 2

# Grafico de barra mostrando a proporcao (em %) de pessoas que faz atividade fisica em cada uma das instituicoes

linguistas.edit%>%
  group_by(instituicao, esporte)%>%
  summarise(quantidade = n())%>%
  mutate(frequencia = quantidade/sum(quantidade))%>%
  ggplot(., aes(x = instituicao, y = frequencia))+
  geom_col()


