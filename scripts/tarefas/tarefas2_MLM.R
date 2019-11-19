#################################
### Pratica: modelos lineares ###
#################################

# Considerando o conjunto de dados `linguistas.csv` que vimos na secao anterior, construa um modelo linear que responda à seguinte pergunta:
  
# O peso de uma pessoa pode ser previsto por sua altura e idade?

# Você devera:
  
# Importar o conjunto de dados;

linguistas = read.csv("dados/linguistas.csv")

# Limpar os dados para considerar apenas linguistas da UFMG, Unicamp e UFRN, ignorando a observacao que tem um tamanho de pe igual a 78

linguistas = linguistas%>%
  select(genero, idade, altura, peso, tamanho.pe, instituicao, dialeto, deslocamento, status, esporte)%>%
  filter(instituicao != "Outra")%>%
  filter(tamanho.pe < 70)%>%
  droplevels()


# Construir seu modelo linear e ver seus resultados;

modelo = lm(peso ~ idade + altura, data = linguistas)

summary(modelo)

# Analisar os residuos para ver se nao ha violacao de hipoteses


      # dataframe com os residuos e fitted values do modelo
      
      ajustados = modelo$fitted.values
      residuos =  modelo$residuals
      
      aj.residuos = data.frame(ajustados, residuos)
      
      # analisando os residuos para testar linearidade
      
      ggplot(aj.residuos, aes(x = ajustados, y = residuos))+
        geom_point(size=2)
      
      ## resultado: nao parece violar muito colinearidade ou homocestaticidade
      
      # normalidade de residuos: qqplot
      
      ggplot(aj.residuos, aes(sample=residuos))+
        stat_qq()+
        stat_qq_line()
      
      ## resultado: distribuicao normal

# Escrever seus resultados como se fosse um artigo
      
  ## Ajustamos um modelo linear com peso como variavel resposta e altura e idade como efeitos fixos. Esse modelo foi significativo (F(2, 58) = 21.35, p < 0.0001), mas a analise dos coeficientes mostrou apenas um efeito de altura (p < 0.0001) e nao de idade (p = 0.97).
