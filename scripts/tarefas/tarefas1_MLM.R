################################################
### Tarefa: manipulacao do conjunto de dados ###
################################################

# Olhe os codigos abaixo sem roda-los. 

# Tente descobrir que codigo gerara uma mensagem de erro e o motivo dessa mensagem.

# Rode os codigos para ver se você acertou.

# Codigo 1
linguistas.total%>%
  select(genero, idade, altura, esporte, tamanho.pe, status)%>%
  filter(instituicao != "UFMG")

# Codigo 2
linguistas.total%>%
  select(genero, idade, altura, instituicao, esporte, status)%>%
  filter(instituicao != "UFMG")

##### RESPOSTA

## O codigo errado eh o primeiro, pois a funcao filter esta pedindo para excluir dados de acordo com os valores da coluna instituicao, mas, na linha anterior, essa coluna nao foi selecionada a partir da funcao select().


#####################################
### Tarefa 2: descricao dos dados ###
#####################################

# Olhe os codigos abaixo, tente adivinhar qual seria seu output e crie uma descricao para eles .


# Codigo 1
linguistas.total%>%
  filter(instituicao == "Unicamp")%>%
  group_by(genero)%>%
  summarize(media = mean(altura))

# Codigo 2
linguistas.total%>%
  group_by(status, deslocamento)%>%
  summarise(qtt = n())

##### RESPOSTA

# Codigo 1: media de altura para linguistas de gênero masculino e femino da Unicamp

# Codigo 2: quantidade de professores e alunos que usam cada um dos tipos de deslocamento 

#####################################
### Tarefa: graficos de dispersao ###
#####################################

# Leia o codigo abaixo e tente adivinhar que tipo de grafico ele construiria. Depois rode o codigo e veja se você acertou.


ggplot(linguistas.edit, aes(x = altura, y = peso))+
  geom_point(size = 3, alpha = 0.6, colour = "red")


#Tente escrever o codigo que produz o grafico abaixo.

ggplot(linguistas.edit, aes(x = idade, y = peso))+
  geom_point(size = 3, alpha = 0.6, aes(color = status))

########################
### Pratica: boxplot ###
########################

# Leia o codigo abaixo e tente adivinhar que tipo de grafico ele construiria. Depois rode o codigo e veja se você acertou.

ggplot(linguistas.edit, aes(x = status, y = idade))+
  geom_boxplot(aes(fill = instituicao))


# Tente escrever o codigo que reproduz o grafico abaixo (para a cor, use "lightblue").

ggplot(linguistas.edit, aes(x = esporte, y = peso))+
  geom_boxplot(fill="lightblue")+
  facet_wrap(~instituicao, ncol=3)

##################################
### Pratica: grafico de barras ###
##################################

#Leia o codigo abaixo e tente adivinhar que tipo de grafico ele construiria. Depois rode o codigo e veja se você acertou.


linguistas.edit%>%
  group_by(instituicao, status, deslocamento)%>%
  summarise(quantidade = n())%>%
  mutate(frequencia = quantidade/sum(quantidade))%>%
  ggplot(., aes(x = status, y = frequencia, fill = deslocamento))+
  geom_col()+
  scale_y_continuous(labels=scales::percent)+
  facet_wrap(~instituicao)

# Leia o codigo abaixo e tente descobrir porque ele nao retorna o que descreve sua descricao. Arrume o codigo.

# Grafico de barra mostrando a proporcao (em %) de pessoas que faz atividade fisica em cada uma das instituicoes

linguistas.edit%>%
  group_by(instituicao, esporte)%>%
  summarise(quantidade = n())%>%
  mutate(frequencia = quantidade/sum(quantidade))%>%
  ggplot(., aes(x = instituicao, y = frequencia, fill = esporte))+
  geom_col()+
  scale_y_continuous(labels=scales::percent)

###############
### PRATICA ###
###############

#importar conjunto de dados

pronome = read.csv("dados/pronome.csv")

# inspecionar conjunto de dados

head(pronome)

# filtragem para considerar apenas casos suja resposta tenha sido alvo ou fonte

pronome = pronome%>%
  filter(interpretacao == "alvo" | interpretacao == "fonte")%>%
  droplevels()

# tabela com contagem de pronome por aspecto

pronome%>%
  group_by(aspecto, interpretacao)%>%
  summarise(n = n())

# tabela com frequência relativa de pronome por aspecto

pronome%>%
  group_by(aspecto, interpretacao)%>%
  summarise(n = n())%>%
  mutate(freq = n / sum(n))

# grafico

pronome%>%
  group_by(aspecto, interpretacao)%>%
  summarise(n = n())%>%
  mutate(frequencia = n / sum(n))%>%
  ggplot(., aes(x=aspecto, y = frequencia, fill = interpretacao))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels=scales::percent)