####################################
### Tarefa: contrastes e modelos ###
####################################

# Antes de continuarmos, vamos fazer uma pausa e ver se você agora consegue entender um pouco melhor a descricao da secao de analise de alguns artigos. Leia o trecho abaixo e responda:

#Modelo1

# Que tipo de contraste os autores usaram para os efeitos fixos?

# dummy ou treatment: 0, 1

# Como seria a equacao do modelo ajustado?

# rate.cr ~ aspect + (1+aspect|participants) + (1+aspect|item)

#Modelo2

# Que tipo de contraste os autores usaram para os efeitos fixos?

# sum: 0.5, -0.5

# Como seria a equacao do modelo ajustado?

# rate.cr ~ prosodic.stress*context + (1|participants) + (1+prosodic.stress*context|item)

### NOTA: os dois experimentos descritos aqui analisaram dados em distribuicao binomial, e portanto usaram a funcao glmer (veremos na secao 5).


########################################
### Pratica: modelos lineares mistos ###
########################################

# pacotes

library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(emmeans)

# importe o conjunto de dados

pronome = read.csv("dados/proinf.csv")

# inspecionar conjunto de dados

str(pronome)

# inspecione a distribuicao dos tempos de resposta para a palavra critica e elimine outliers e observacoes com tempo de resposta maior que 2000ms

ggplot(pronome, aes(tempo.adverbio))+
  geom_histogram()+
  facet_wrap(~ pronome + referencia)

        # eliminando valores extremos
        
        pronome = pronome%>%
          filter(tempo.adverbio < 2000)
        
        # revendo distribuicao
        
        ggplot(pronome, aes(tempo.adverbio))+
          geom_histogram()+
          facet_wrap(~ pronome + referencia)
        
        ggplot(pronome, aes(sample = tempo.adverbio))+
          stat_qq()+
          stat_qq_line()+
          facet_wrap(~pronome+referencia)

          ## dados nao sao normais


# transforme os dados se achar necessario

pronome$log.adverbio = log(pronome$tempo.adverbio)

ggplot(pronome, aes(log.adverbio))+
  geom_histogram()+
  facet_wrap(~ pronome + referencia)

ggplot(pronome, aes(sample = log.adverbio))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~pronome + referencia)

    ## normalidade nao esta tao boa, vamos ver os residuos depois...

# ajuste o contraste mais adequado para a analise;

        # ajustando e conferindo sum coding para variavel referencia
        
        contrasts(pronome$referencia) = c(-0.5, 0.5)
        contrasts(pronome$referencia)
        
        # ajustando e conferindo sum coding para variavel pronome
        
        contrasts(pronome$pronome) = c(-0.5,0.5)
        contrasts(pronome$pronome)



# construa um modelo linear misto para analisar se tipo de contexto, tipo de palavra e a interacao entre esses fatores conseguem explicar a distribuicao de dados


    ## modelo pronome*referencia full (problema de convergência)
      model.full = lmer(log.adverbio ~ referencia*pronome + (1+referencia*pronome|participante) + (1+referencia*pronome|item), data = pronome, REML = FALSE)

      ## modelo pronome*referencia sem slopes por participante(problema de convergência)
      model.full = lmer(log.adverbio ~ referencia*pronome + (1|participante) + (1+referencia*pronome|item), data = pronome, REML = FALSE)

      ## modelo pronome*referencia sem slopes por item (convergiu)
      model.full = lmer(log.adverbio ~ referencia*pronome + (1|participante) + (1|item), data = pronome, REML = FALSE)

      
# faca analise por modelos aninhados se necessario
            
      ## modelo aninhado pronome+referencia
      model.pro.ref = lmer(log.adverbio ~ referencia+pronome + (1|participante) + (1|item), data = pronome, REML = FALSE)
      
      # comparacao de modelos para testar interacao
      
      anova(model.full, model.pro.ref)
      
      ## resultado: interacao foi significativa 
      
      
# faca analises post-hoc se necessario
      
      ph = emmeans(model.full, ~ referencia*pronome)
      
      pairs(ph, adjust="tukey")
      
# faca analise dos residuos
      
      ajustados = fitted(model.full)
      residuos  = residuals(model.full)
      
      aj.residuos = data.frame(ajustados, residuos)
      
      # analisando os residuos
      
      ggplot(aj.residuos, aes(ajustados, residuos))+
        geom_point(size=3)
      
      ggplot(aj.residuos, aes(sample = residuos))+
        stat_qq()+
        stat_qq_line()
    
      ## distribuicao de residuos nao esta normal; talvez valha a pena fazer uma analise por modelo linear generalizado usando gama como familia de distribuicao
      
### RESPOSTA: Ajustamos um modelo linear misto com tempo de leitura do adverbio como variavel resposta e tipo de pronome, referência e sua interacao como efeitos fixos. Os efeitos fixos receberam contraste por sum coding (-0.5, 0.5). O modelo tambem continha interceptos aleatorios por participantes e itens. Uma comparacao por modelos aninhados indicou que esse foi melhor modelo ajustado (Chisq = 8.6754, p = 0.003225). Uma analise post-hoc nao apontou diferenca entre o tempo de leitura na condicao plural indefinido e plural correferencial (p = 0.5719), mas indicou que o tipo de referência afetou a leitura de pronomes singulares. Nesses casos, o tempo de leitura de pronomes indefinidos foi maior que os correferenciais (p = 0.0213) (p-valores ajustados por Tukey).