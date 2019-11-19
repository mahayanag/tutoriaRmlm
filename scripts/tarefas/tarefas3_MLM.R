
##################################
### Tarefa: entendendo artigos ###
##################################

#Antes de seguirmos nosso tutorial, vamos fazer uma pausa para fixar o que você aprendeu. Abaixo ha a descricao de modelos lineares feitas por dois artigos. Tente escrever como seria a sintaxe que esses artigos estao descrevendo.

#Modelo 1

# lmer(residual.reading.time ~ region + (1|item) + (1|participant))

#Modelo 2

# lm(body.N ~ body.N.condition + language)

#Modelo 3

# lmer(inverse.RTs ~ body.N + (1|item) + (1|participant))

########################################
### Pratica: modelos lineares mistos ###
########################################

# pacotes

library(lme4)
library(ggplot2)
library(dplyr)

# importe o conjunto de dados;

prediction = read.csv("dados/prediction.csv")

prediction = prediction%>%
  filter(resposta == "sim")%>%
  droplevels()

# inspecione a distribuicao dos tempos de resposta para a palavra critica e elimine outliers e observacoes com tempo de resposta maior que 2000ms;

        ggplot(prediction, aes(sample = rt))+
          stat_qq(alpha=0.3)+
          facet_wrap(~contexto+palavra)
        
        # filtrando apenas rt < 2000
        
        prediction = prediction%>%
          filter(rt < 2000)
        
        # revendo distribuicao
        
        ggplot(prediction, aes(sample = rt))+
          stat_qq(alpha=0.3)+
          stat_qq_line()+
          facet_wrap(~contexto+palavra)
        
        # com boxplot, se preferir 
        
        ggplot(prediction, aes(x = palavra, y = rt))+
          geom_boxplot()+
          facet_wrap(~contexto)
        
        # com histograma, se preferir
        
        ggplot(prediction, aes(x = rt))+
          geom_histogram()+
          facet_wrap(~contexto+palavra)
        
        ## conclusao: dados nao sao simetricos

# transforme os dados se achar necessario;
        
prediction$rt.log = log(prediction$rt)

        # revendo distribuicao
        
        ggplot(prediction, aes(sample = rt.log))+
          stat_qq(alpha=0.3)+
          stat_qq_line()+
          facet_wrap(~contexto+palavra)
        
        # distribuicao melhorou (poderia ser pior, vamor ver os residuos do modelo depois...)
        
# construa um modelo linear misto para analisar se tipo de contexto, tipo de palavra e a interacao entre esses fatores consegue explicar a distribuicao de dados;

#faca analise por modelos aninhados se necessario.
        
        # modelo completo
        
        modelo.full <- lmer(rt ~ contexto*palavra + (1+contexto*palavra|participante) + (1+contexto*palavra|cod), data = prediction, REML = FALSE)

        # problema de convergência, retirando slopes por participante
        modelo.full <- lmer(rt ~ contexto*palavra + (1|participante) + (1+contexto*palavra|cod), data = prediction, REML = FALSE)
        
        # problema de convergência, retirando slopes por item
        
        modelo.full <- lmer(rt ~ contexto*palavra + (1|participante) + (1|cod), data = prediction, REML = FALSE)

        # modelo sem interacao
        
        modelo.palavra.contexto <- lmer(rt ~ contexto+palavra + (1|participante) + (1|cod), data = prediction, REML = FALSE)
        
        # comparacao de modelos p/testar interacao
        
        anova(modelo.full, modelo.palavra.contexto)
            
              ## resultado: interacao nao significativa
        
        # modelo sem contexto
        
        modelo.palavra <- lmer(rt ~ palavra + (1|participante) + (1|cod), data = prediction, REML = FALSE)
        
        # comparacao de modelos p/testar contexto
        
        anova(modelo.palavra.contexto, modelo.palavra)
        
              ## resultado: contexto nao significativo
        
        # modelo sem palavra
        
        modelo.contexto <- lmer(rt ~ contexto + (1|participante) + (1|cod), data = prediction, REML = FALSE)
        
        # comparacao de modelos p/testar palavra
        
        anova(modelo.contexto, modelo.palavra.contexto)
        
        # modelo sem efeito fixo
        
        modelo.1 <- lmer(rt ~ 1 + (1|participante) + (1|cod), data = prediction, REML = FALSE)
        
        # comparacao de modelos p/testar palavra
        
        anova(modelo.palavra.contexto, modelo.1)
        
### Resposta: Ajustamos um modelo linear misto com tempo de resposta como variavel resposta e contexto, palavra e sua interacao como efeitos fixos. O modelo tambem continha interceptos aleatorios por participantes e itens. Uma comparacao por modelos aninhados indicou que nenhum dos efeitos fixos explicou significativamente a distribuicao dos tempos de resposta.
        
        ## (se desejado, eh possivel colocar as informacoes estatisticas de cada teste de comparacao de modelo aninhado)