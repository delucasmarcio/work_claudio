##########################################################################
##                                                                      ##
##                Análise de Indicadores Criminológicos                 ##
##                                                                      ##
##                      Marcio de Lucas Cunha Gomes                     ##
##                        delucasmarcio@gmail.com                       ##  
##                                                                      ##          
##########################################################################

# Carregando bibliotecas
library(foreign)
library(descr)
library(stringi)
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)

# Movendo para diretório de arquivos
setwd('source')

# Carregando Toolbox de Claúdio Monteiro
## evaluate empty
vector.is.empty <- function(x) return(length(x) ==0 )

## limpar string
cleanStr <- function(string){
  string = str_replace_all(string, ' ', '_')
  string = str_replace_all(string, '-', '_')
  string = str_replace_all(string, '%', '')
  string = str_replace_all(string, '/', '_')
  string = stri_trans_general(string, "latin-ascii")
  string = tolower(string)
  return(string)
}

## ggplot theme
tema_massa <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.ticks = element_line(size = 1, colour = "grey70" ),
          axis.text.x = element_text(colour= "black",size=10,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=10,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=11,angle=90,hjust=.5,vjust=0.5,face="plain"),
          title = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(6, "mm"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"),
          plot.background = element_blank())
}

# meta161_indicador1 : "Crimes violentos letais intencionais"
d1 = read.csv('dados/d1/cvli_2007_2017.csv',
              sep = ',',fileEncoding = 'UTF-8')

## Ordenando por Ano
d1 = d1[order(d1$Year.of.Ano..data.),]

## Renonando variáveis
colnames(d1) = c('variavel','uf','ano','valor')

## Eliminando dados agregados por Brasil
d1 = d1[d1$uf != 'Brasil',]

## Número Absoluto de CVLI
n_cvli = d1[d1$variavel == 'Números absolutos',]

## Taxa de CVLI
tx_cvli = d1[d1$variavel == 'Taxa / 100 mil habitantes',]

## Gráficos
### Selecionando apenas UFs em que mudança > |10%|
var_uf = n_cvli$valor[n_cvli$ano == 2007]
var_uf = (n_cvli$valor[n_cvli$ano == 2017] - var_uf) / 
  var_uf
names(var_uf) = n_cvli$uf[n_cvli$ano == 2007]

  
ggplot(n_cvli[n_cvli$uf %in% names(var_uf[var_uf > 3]),],
       aes(x = ano, y = valor, colour = uf)) +
  labs(colour = "UF", y = 'CVLI total', x = 'Ano') + 
  geom_line() + tema_massa()

# meta161_indicador2



n_cvli = reshape(n_cvli, idvar = 'uf', 
                 timevar = 'ano', direction = "wide")
