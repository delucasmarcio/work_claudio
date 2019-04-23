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

##########################################################################
##                                                                      ##    
##               Crimes violentos letais intencionais                   ##
##                                                                      ##          
##########################################################################

# Carregando dados
d1 = read.csv('dados/indicador_1/cvli_2007_2017.csv',
              sep = ',',fileEncoding = 'UTF-8')

# Ordenando por Ano
d1 = d1[order(d1$Year.of.Ano..data.),]

# Renonando variáveis
colnames(d1) = c('variavel','uf','ano','valor')

# Eliminando dados agregados por Brasil
d1 = d1[d1$uf != 'Brasil',]
d1$uf = droplevels(d1$uf)

# Número Absoluto de CVLI
n_cvli = d1[d1$variavel == 'Números absolutos',]

# Taxa de CVLI
tx_cvli = d1[d1$variavel == 'Taxa / 100 mil habitantes',]

# Gráficos e Tabelas
## CVLI absoluto
ag_uf = tapply(n_cvli$valor, n_cvli$uf, sum, na.rm = T)
ag_uf = sort(ag_uf)
ag_uf = data.frame(uf = names(ag_uf),
                   cvli = ag_uf)

### Total CVLI em 10 anos
png('graficos/meta161_indicador1_uf_total_cvli_2007_2017.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -cvli), y = cvli)) +
  labs(y = 'CVLI total', x = 'UF') + 
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Total de CVli em 10 anos (2007 à 2017)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador1_uf_total_cvli_2007_2017.csv')


## CVLI taxa
var_uf = tx_cvli %>% group_by(uf) %>%
  mutate(lag.valor = dplyr::lag(valor, n = 1, default = NA))

var_uf$crescimento = with(var_uf,{
  (valor - lag.valor) / lag.valor
}) 

### Crescimento Acumulado por UF
var_uf = data.frame(uf = levels(var_uf$uf),
                    cresc.acum = tapply(var_uf$crescimento, 
                                        var_uf$uf, sum, na.rm = T),
                    sd.cresc = tapply(var_uf$crescimento, 
                                      var_uf$uf, sd, na.rm = T))

png('graficos/meta161_indicador1_uf_crescimento_acumulado_cvli_taxa.png')

ggplot(var_uf,
       aes(x = reorder(uf, -cresc.acum), y = cresc.acum)) +
  labs(y = 'Crescimento acumulado Outros Letais', x = 'UF') + 
  geom_bar(stat = 'identity', position=position_dodge(), fill = 'steelblue') + 
  geom_errorbar(aes(ymin = cresc.acum - sd.cresc, 
                    ymax = cresc.acum + sd.cresc),
                width = .2, position = position_dodge(.9)) +
  tema_massa()

dev.off()

tab_uf = var_uf

colnames(tab_uf) = c('UF', 'Crescimento Acumulado CVLI (2007 à 2017)')
write_excel_csv(tab_uf,'tabelas/meta161_indicador1_cres_acumulado_cvli_2007_2017.csv')

var_uf = tapply(var_uf$cresc.acum, var_uf$uf, sum, na.rm = T)
var_uf  = sort(var_uf)

### Maior tendência de redução 
png('graficos/meta161_indicador1_uf_menor_crescimento_acumulado_cvli_taxa.png')

ggplot(tx_cvli[tx_cvli$uf %in% names(var_uf[1:5]),],
       aes(x = ano, y = valor, colour = uf)) +
  labs(colour = "UF", y = 'CVLI por 100 mil hab.', x = 'Ano') + 
  geom_line() + tema_massa()

dev.off()

xx = tx_cvli[tx_cvli$uf %in% names(var_uf[1:5]),]
xx = xx[2:4]
xx = reshape(xx, idvar = 'uf',timevar = 'ano', direction = 'wide')

xx = cbind('Taxa de CVLI por 100 mil habitantes',xx)

colnames(xx) = c('uf','variável',as.character(2007:2017))

write_excel_csv(xx,'tabelas/meta161_indicador1_cvli_top_rank_2007_2017.csv')

### Maior tendência de crescimento 
png('graficos/meta161_indicador1_uf_maior_crescimento_acumulado_cvli_taxa.png')

ggplot(tx_cvli[tx_cvli$uf %in% names(var_uf[22:27]),],
       aes(x = ano, y = valor, colour = uf)) +
  labs(colour = "UF", y = 'CVLI por 100 mil hab.', x = 'Ano') + 
  geom_line() + tema_massa()

dev.off()

xx = tx_cvli[tx_cvli$uf %in% names(var_uf[22:27]),]
xx = xx[2:4]
xx = reshape(xx, idvar = 'uf',timevar = 'ano', direction = 'wide')

xx = cbind('Taxa de CVLI por 100 mil habitantes',xx)

colnames(xx) = c('uf','variável',as.character(2007:2017))

write_excel_csv(xx,'tabelas/meta161_indicador1_cvli_botton_rank_2007_2017.csv')

##########################################################################
##                                                                      ##    
##                    Taxa de mortes por agressão                       ##
##                                                                      ##          
##########################################################################

# faltam dados

##########################################################################
##                                                                      ##    
##                        Taxa de outros letais                         ##
##                                                                      ##          
##########################################################################

# Carregando dados
d21 = read.csv('dados/indicador_3/hom_culposo_transito_fbsp.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d21) = c('m1','var1','uf','ano','v1')

d22 = read.csv('dados/indicador_3/morte_acidentais_transito_fbsp.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d22) = c('m2','var2','uf','ano','v2')

d23 = read.csv('dados/indicador_3/mortes_esclarecer_fbsp.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d23) = c('m3','var3','uf','ano','v3')

d24 = read.csv('dados/indicador_3/outras_acidentais_transito_fbsp.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d24) = c('m4','var4','uf','ano','v4')

d25 = read.csv('dados/indicador_3/outros_hom_culposos_fbsp.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d25) = c('m5','var5','uf','ano','v5')

d26 = read.csv('dados/indicador_3/outros_resultantes_mortes_fbsp.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d26) = c('m6','var6','uf','ano','v6')

d27 = read.csv('dados/indicador_3/suicidio_fbsp.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d27) = c('m7','var7','uf','ano','v7')

# Concatenando dados
d2 = merge(d21,d22, by = c('uf','ano'))
d2 = merge(d2,d23, by = c('uf','ano'))
d2 = merge(d2,d24, by = c('uf','ano'))
d2 = merge(d2,d25, by = c('uf','ano'))
d2 = merge(d2,d26, by = c('uf','ano'))
d2 = merge(d2,d27, by = c('uf','ano'))

d2$tx_letais = 0

for(v in paste0('v',1:7)){
  
  d2$tx_letais = d2$tx_letais + d2[v]
}

d2$tx_letais = d2$tx_letais$v1

d2 = d2[c('ano','uf','tx_letais')]

d2 = d2[d2$uf != 'Brasil',]

d2$uf = droplevels(d2$uf)

## Taxa de outros letais
var_uf = d2 %>% group_by(uf) %>%
  mutate(lag.tx = dplyr::lag(tx_letais, n = 1, default = NA))

var_uf$crescimento = with(var_uf,{
  (tx_letais - lag.tx) / lag.tx
}) 

### Crescimento Acumulado por UF
var_uf = data.frame(uf = levels(var_uf$uf),
                    cresc.acum = tapply(var_uf$crescimento, 
                                        var_uf$uf, sum, na.rm = T),
                    sd.cresc = tapply(var_uf$crescimento, 
                                      var_uf$uf, sd, na.rm = T))

png('graficos/meta161_indicador3_uf_crescimento_acumulado_outros_letais_taxa.png')

ggplot(var_uf,
       aes(x = reorder(uf, -cresc.acum), y = cresc.acum)) +
  labs(y = 'Crescimento acumulado Outros Letais', x = 'UF') + 
  geom_bar(stat = 'identity', position=position_dodge(), fill = 'steelblue') + 
  geom_errorbar(aes(ymin = cresc.acum - sd.cresc, 
                    ymax = cresc.acum + sd.cresc),
                width = .2, position = position_dodge(.9)) +
  tema_massa()

dev.off()

tab_uf = var_uf

colnames(tab_uf) = c('UF', 'Crescimento Acumulado Outros Letais (2007 à 2015)')
write_excel_csv(tab_uf,'tabelas/meta161_indicador3_cres_acumulado_outros_letais_2007_2015.csv')

## Taxa Suicídio
var_uf = d27 %>% group_by(uf) %>%
  mutate(lag.suic = dplyr::lag(v7, n = 1, default = NA))

var_uf$crescimento = with(var_uf,{
  (v7 - lag.suic) / lag.suic
}) 

### Crescimento Acumulado por UF
var_uf = data.frame(uf = levels(var_uf$uf),
                    cresc.acum = tapply(var_uf$crescimento, 
                                        var_uf$uf, sum, na.rm = T),
                    sd.cresc = tapply(var_uf$crescimento, 
                                      var_uf$uf, sd, na.rm = T))

png('graficos/meta161_indicador3_uf_crescimento_acumulado_suicidio_taxa.png')

ggplot(var_uf,
       aes(x = reorder(uf, -cresc.acum), y = cresc.acum)) +
  labs(y = 'Crescimento acumulado Suicídio', x = 'UF') + 
  geom_bar(stat = 'identity', position=position_dodge(), fill = 'steelblue') + 
  geom_errorbar(aes(ymin = cresc.acum - sd.cresc, 
                    ymax = cresc.acum + sd.cresc),
                width = .2, position = position_dodge(.9)) +
  tema_massa()

dev.off()

tab_uf = var_uf

colnames(tab_uf) = c('UF', 'Crescimento Acumulado Suicídio (2007 à 2015)')
write_excel_csv(tab_uf,'tabelas/meta161_indicador3_cres_acumulado_suicidio_2007_2015.csv')

### Média de Suicídio ao longo de 2007 à 2015
ag_uf = tapply(d27$v7, d27$uf, mean, na.rm = T)
ag_uf = sort(ag_uf)
ag_uf = data.frame(uf = names(ag_uf),
                   tx_letais = ag_uf)

png('graficos/meta161_indicador3_uf_media_suicidio_2007_2015.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -tx_letais), y = tx_letais)) +
  labs(y = 'Média Suicídio', x = 'UF') + 
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Média da Taxa Suicídio (2007 à 2015)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador3_uf_media_suicidio_2007_2015.csv')

### Média de Outros Letais ao longo de 2007 à 2015
ag_uf = tapply(d2$tx_letais, d2$uf, mean, na.rm = T)
ag_uf = sort(ag_uf)
ag_uf = data.frame(uf = names(ag_uf),
                   tx_letais = ag_uf)

png('graficos/meta161_indicador3_uf_media_outros_letais_2007_2015.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -tx_letais), y = tx_letais)) +
  labs(y = 'Média Outros Letais', x = 'UF') + 
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Média da Taxa Outros Letais (2007 à 2015)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador3_uf_media_outros_letais_2007_2015.csv')

### Média de 'Não esclarecidos' ao longo de 2007 à 2015
ag_uf = tapply(d23$v3, d23$uf, mean, na.rm = T)
ag_uf = sort(ag_uf)
ag_uf = data.frame(uf = names(ag_uf),
                   n_escl = ag_uf)

png('graficos/meta161_indicador3_uf_media_nao_esclarecidos_2007_2015.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -n_escl), y = n_escl)) +
  labs(y = 'Média Não Esclarecidos', x = 'UF') + 
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Média da Taxa Outros Letais (2007 à 2015)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador3_uf_media_outros_letais_2007_2015.csv')

##########################################################################
##                                                                      ##    
##            Taxa de crimes contra a liberdade sexual                  ##
##                                                                      ##          
##########################################################################

# Carregando dados
d31 = read.csv('dados/indicador_4/estupro_2009_2017.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d31) = c('m1','var1','uf','ano','v1')

d32 = read.csv('dados/indicador_4/tentativa_estupro_2009_2017.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d32) = c('m2','var2','uf','ano','v2')

# Concatenando dados
d3 = merge(d31,d32, by = c('uf','ano'))

d3$crime_sexual = d3$v1 + d3$v2

