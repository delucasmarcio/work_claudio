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
library(reshape2)

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
  labs(y = 'Crescimento acumulado (%) CVLI', x = 'UF') + 
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

# Ordenando por Ano
d2 = d2[order(d2$ano),]

# Criando Variável 'Outros Letais'
d2$tx_letais = 0

for(v in paste0('v',1:7)){
  
  d2$tx_letais = d2$tx_letais + d2[v]
}

d2$tx_letais = d2$tx_letais$v1

d2 = d2[c('ano','uf','tx_letais')]

d2 = d2[d2$uf != 'Brasil',]

d2$uf = droplevels(d2$uf)

# Gráficos e Tabelas
## Taxa de outros letais
var_uf = d2 %>% group_by(uf) %>%
  mutate(lag.tx = dplyr::lag(tx_letais, n = 1, default = NA))

var_uf$crescimento = with(var_uf,{
  (tx_letais - lag.tx) / lag.tx
}) 

### Crescimento Acumulado outros letais por UF
var_uf = data.frame(uf = levels(var_uf$uf),
                    cresc.acum = tapply(var_uf$crescimento, 
                                        var_uf$uf, sum, na.rm = T),
                    sd.cresc = tapply(var_uf$crescimento, 
                                      var_uf$uf, sd, na.rm = T))

png('graficos/meta161_indicador3_uf_crescimento_acumulado_outros_letais_taxa.png')

ggplot(var_uf,
       aes(x = reorder(uf, -cresc.acum), y = cresc.acum)) +
  labs(y = 'Crescimento acumulado (%) Outros Letais', x = 'UF') + 
  geom_bar(stat = 'identity', position=position_dodge(), fill = 'steelblue') + 
  geom_errorbar(aes(ymin = cresc.acum - sd.cresc, 
                    ymax = cresc.acum + sd.cresc),
                width = .2, position = position_dodge(.9)) +
  tema_massa()

dev.off()

tab_uf = var_uf

colnames(tab_uf) = c('UF', 'Crescimento Acumulado (%) Outros Letais (2007 à 2015)')
write_excel_csv(tab_uf,'tabelas/meta161_indicador3_cres_acumulado_outros_letais_2007_2015.csv')

### Média de Outros Letais ao longo de 2007 à 2015
ag_uf = tapply(d2$tx_letais, d2$uf, mean, na.rm = T)
ag_uf = sort(ag_uf)
ag_uf = data.frame(uf = names(ag_uf),
                   tx_letais = ag_uf)

png('graficos/meta161_indicador3_uf_media_outros_letais_2007_2015.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -tx_letais), y = tx_letais)) +
  labs(y = 'Média Outros Letais por 100 mil hab.', x = 'UF') + 
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Média da Taxa Outros Letais por 100 mil hab. (2007 à 2015)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador3_uf_media_outros_letais_2007_2015.csv')

## Taxa Suicídio
var_uf = d27 %>% group_by(uf) %>%
  mutate(lag.suic = dplyr::lag(v7, n = 1, default = NA))

var_uf$crescimento = with(var_uf,{
  (v7 - lag.suic) / lag.suic
}) 

### Crescimento Acumulado Suicídio por UF
var_uf = data.frame(uf = levels(var_uf$uf),
                    cresc.acum = tapply(var_uf$crescimento, 
                                        var_uf$uf, sum, na.rm = T),
                    sd.cresc = tapply(var_uf$crescimento, 
                                      var_uf$uf, sd, na.rm = T))

png('graficos/meta161_indicador3_uf_crescimento_acumulado_suicidio_taxa.png')

ggplot(var_uf,
       aes(x = reorder(uf, -cresc.acum), y = cresc.acum)) +
  labs(y = 'Crescimento acumulado (%) Suicídio', x = 'UF') + 
  geom_bar(stat = 'identity', position=position_dodge(), fill = 'steelblue') + 
  geom_errorbar(aes(ymin = cresc.acum - sd.cresc, 
                    ymax = cresc.acum + sd.cresc),
                width = .2, position = position_dodge(.9)) +
  tema_massa()

dev.off()

tab_uf = var_uf

colnames(tab_uf) = c('UF', 'Crescimento Acumulado (%) Suicídio (2007 à 2015)')
write_excel_csv(tab_uf,'tabelas/meta161_indicador3_cres_acumulado_suicidio_2007_2015.csv')

### Média de Suicídio ao longo de 2007 à 2015
ag_uf = tapply(d27$v7, d27$uf, mean, na.rm = T)
ag_uf = sort(ag_uf)
ag_uf = data.frame(uf = names(ag_uf),
                   tx_letais = ag_uf)

png('graficos/meta161_indicador3_uf_media_suicidio_2007_2015.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -tx_letais), y = tx_letais)) +
  labs(y = 'Média Suicídio por 100 mil hab.', x = 'UF') + 
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Média da Taxa Suicídio por 100 mil hab. (2007 à 2015)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador3_uf_media_suicidio_2007_2015.csv')

### Média de 'Não esclarecidos' ao longo de 2007 à 2015
ag_uf = tapply(d23$v3, d23$uf, mean, na.rm = T)
ag_uf = sort(ag_uf)
ag_uf = data.frame(uf = names(ag_uf),
                   n_escl = ag_uf)

png('graficos/meta161_indicador3_uf_media_nao_esclarecidos_2007_2015.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -n_escl), y = n_escl)) +
  labs(y = 'Média Mortes Não Esclarecidos por 100 mil hab.', x = 'UF') + 
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Média da Mortes Não Esclarecidas por 100 mil hab. (2007 à 2015)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador3_uf_media_mortes_nao_esclarecidas_2007_2015.csv')

##########################################################################
##                                                                      ##    
##            Taxa de crimes contra a liberdade sexual                  ##
##                                                                      ##          
##########################################################################

# Carregando dados
d31 = read.csv('dados/indicador_4/estupro_taxa_2009_2017.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d31) = c('m1','var1','uf','ano','v1')

d32 = read.csv('dados/indicador_4/tentativa_estupro_taxa_2009_2017.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d32) = c('m2','var2','uf','ano','v2')

d33 = read.csv('dados/indicador_4/estupro_n_2009_2017.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d33) = c('m3','var3','uf','ano','v3')

d34 = read.csv('dados/indicador_4/tentativa_estupro_n_2009_2017.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d34) = c('m4','var4','uf','ano','v4')

# Concatenando dados
d3 = merge(d31,d32, by = c('uf','ano'))
d3 = merge(d3,d33, by = c('uf','ano'))
d3 = merge(d3,d34, by = c('uf','ano'))

d3$tx.crime_sexual = d3$v1 + d3$v2
d3$n.crime_sexual = d3$v3 + d3$v4

d3 = d3[c('uf','ano','tx.crime_sexual','n.crime_sexual')]

# Ordenando por Ano
d3 = d3[order(d3$ano),]

# Gráficos e Tabelas
## Crimes Sexuais absoluto
ag_uf = tapply(d3$n.crime_sexual, d3$uf, sum, na.rm = T)
ag_uf = sort(ag_uf)
ag_uf = data.frame(uf = names(ag_uf),
                   crime.sex = ag_uf)

### Total CVLI em 10 anos
png('graficos/meta161_indicador4_uf_total_crimes_sexuais_2009_2017.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -crime.sex), y = crime.sex)) +
  labs(y = 'Crimes Sexuais total', x = 'UF') + 
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Total de Crimes Sexuais (2009 à 2017)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador4_uf_total_crimes_sexuais_2009_2017.csv')

## Crimes Sexuais taxa
var_uf = d3 %>% group_by(uf) %>%
  mutate(lag.crime.sex = dplyr::lag(tx.crime_sexual, n = 1, default = NA))

var_uf$crescimento = with(var_uf,{
  (tx.crime_sexual - lag.crime.sex) / lag.crime.sex
}) 

### Crescimento Acumulado Crimes Sexuais por UF
var_uf = data.frame(uf = levels(var_uf$uf),
                    cresc.acum = tapply(var_uf$crescimento, 
                                        var_uf$uf, sum, na.rm = T),
                    sd.cresc = tapply(var_uf$crescimento, 
                                      var_uf$uf, sd, na.rm = T))

png('graficos/meta161_indicador4_uf_crescimento_acumulado_crimes_sexuais_taxa.png')

ggplot(var_uf,
       aes(x = reorder(uf, -cresc.acum), y = cresc.acum)) +
  labs(y = 'Crescimento acumulado (%) Crimes Sexuais', x = 'UF') + 
  geom_bar(stat = 'identity', position=position_dodge(), fill = 'steelblue') + 
  geom_errorbar(aes(ymin = cresc.acum - sd.cresc, 
                    ymax = cresc.acum + sd.cresc),
                width = .2, position = position_dodge(.9)) +
  tema_massa()

dev.off()

tab_uf = var_uf

colnames(tab_uf) = c('UF', 'Crescimento Acumulado (%) Crimes Sexuais (2009 à 2017)')
write_excel_csv(tab_uf,'tabelas/meta161_indicador4_cres_acumulado_crimes_sexuais_2007_2017.csv')

##########################################################################
##                                                                      ##    
##              Taxa de crimes intencionais não letais                  ##
##                                                                      ##          
##########################################################################

# faltam dados

##########################################################################
##                                                                      ##    
##        Taxa de crimes violentos não letais contra o patrimônio       ##
##                                                                      ##          
##########################################################################

# Carregando dados
## Valores Absolutos
d41 = read.csv('dados/indicador_6/crimes_patrimoniais_n_fbsp.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d41) = c('medida','variavel','uf','ano','valor')

ag_uf = data.frame(uf = levels(d41$uf),
                   veiculo = with(d41[d41$variavel %in% c('Furto de veículo',
                                                          'Roubo de veículo',
                                                          'Roubo e furto de veículos'),],{
                     tapply(valor, uf, sum, na.rm = T)
                   }),
                   carga = with(d41[d41$variavel == 'Roubo de carga',],{
                     tapply(valor, uf, sum, na.rm = T)
                   }),
                   outros = with(d41[d41$variavel == 'Roubo (outros)',],{
                     tapply(valor, uf, sum, na.rm = T)
                   }))

ag_uf = melt(ag_uf, id.var = 'uf')

## Gráficos e Tabelas
png('graficos/meta161_indicador6_uf_total_crimes_patrimoniais_2007_2016.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -value), y = value)) +
  labs(y = 'Crimes Patrimoniais total', x = 'UF') +   
  geom_bar(aes(fill = variable), stat="identity") + 
  lege(labels = c("Veículo", "Carga",'Outros')) +
  tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Total de Crimes Patrimoniais (2007 à 2016)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador6_uf_total_crimes_patrimoniais_2007_2016.csv')

ag_uf = data.frame(uf = levels(d41$uf),
                   instituicao = with(d41[d41$variavel == 'Roubo a instituição financeira',],
                                      {tapply(valor, uf, sum, na.rm = T)}))

png('graficos/meta161_indicador6_uf_total_roubo_instituicao_2007_2016.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -instituicao), y = instituicao)) +
  labs(y = 'Roubo a Instituição Financeira total', x = 'UF') + 
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Total de Roubo a Instituição Financeira (2007 à 2016)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador6_uf_total_roubo_instituicao_2007_2016.csv')

## Valores Relativos
d42 = read.csv('dados/indicador_6/crimes_patrimoniais_instituicao_financeira_taxa_fbsp.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d42) = c('m1','var1','uf','ano','v1')

d43 = read.csv('dados/indicador_6/crimes_patrimoniais_pessoa_taxa_fbsp.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d43) = c('m2','var2','uf','ano','v2')

d44 = read.csv('dados/indicador_6/crimes_patrimoniais_veiculos_taxa_fbsp.csv',
               sep = ',', fileEncoding = 'UTF-8')

colnames(d44) = c('m3','var3','uf','ano','v3')

## Concatenando dados relativos
d4 = merge(d42,d43, by = c('ano','uf'))
d4 = merge(d4,d44, by = c('ano','uf'))

## Crimes Sexuais taxa
var_uf = d4 %>% group_by(uf) %>%
  mutate(lag.instituicao = dplyr::lag(v1, n = 1, default = NA)) %>%
  mutate(lag.pessoa = dplyr::lag(v2, n = 1, default = NA)) %>%
  mutate(lag.veiculo = dplyr::lag(v3, n = 1, default = NA))

var_uf$cresc.inst = with(var_uf,{
  (v1 - lag.instituicao) / lag.instituicao
}) 

var_uf$cresc.pessoa = with(var_uf,{
  (v2 - lag.pessoa) / lag.pessoa
}) 

var_uf$cresc.veiculo = with(var_uf,{
  (v3 - lag.veiculo) / lag.veiculo
}) 

### Crescimento Acumulado Crimes Sexuais por UF
var_uf = data.frame(uf = levels(var_uf$uf),
                    ca.inst = tapply(var_uf$cresc.inst, 
                                        var_uf$uf, sum, na.rm = T),
                    ca.pessoa = tapply(var_uf$cresc.pessoa, 
                                     var_uf$uf, sum, na.rm = T),
                    ca.veiculo = tapply(var_uf$cresc.veiculo, 
                                        var_uf$uf, sum, na.rm = T))


# Ajustando Erros
var_uf[var_uf == Inf] <- NA

## Gráficos e Tabelas
png('graficos/meta161_indicador6_uf_crescimento_acumulado_roubo_instituicao_2007_2016.png')

ggplot(var_uf,
       aes(x = reorder(uf, -ca.inst), y = ca.inst)) +
  labs(y = 'Crescimento Acumulado (%) Roubo a Instituição Financeira', x = 'UF') +   
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

png('graficos/meta161_indicador6_uf_crescimento_acumulado_outros_roubos_2007_2016.png')

ggplot(var_uf,
       aes(x = reorder(uf, -ca.pessoa), y = ca.pessoa)) +
  labs(y = 'Crescimento Acumulado (%) Outros Roubos', x = 'UF') +   
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

png('graficos/meta161_indicador6_uf_crescimento_acumulado_roubo_veiculo_2007_2016.png')

ggplot(var_uf,
       aes(x = reorder(uf, -ca.veiculo), y = ca.veiculo)) +
  labs(y = 'Crescimento Acumulado (%) Roubo ou Furto de Veículo', x = 'UF') +   
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(var_uf) = c('UF', 
                     'Crescimento Acumulado (%) Roubo a Instituição Financeira (2007 à 2016)',
                     'Crescimento Acumulado (%) Outros Roubos (2007 à 2016)',
                     'Crescimento Acumulado (%) Roubo ou Furto e Veículos (2007 à 2016)')

write_excel_csv(ag_uf,'tabelas/meta161_indicador6_uf_cresc_acumulado_crimes_patrimoniais_2007_2016.csv')

##########################################################################
##                                                                      ##    
##                      Mortes por Arma de Fogo                         ##
##                                                                      ##          
##########################################################################

# Carregando dados
d5 = read.csv('dados/indicador_7/obito_por_af_2002_2012.csv',
              sep = ';', fileEncoding = 'latin1')

d5 = gather(d5,ano,hom_af,2:12)

d5$ano = as.numeric(substr(d5$ano,2,5))

# Gráficos e Tabelas
ag_uf = tapply(d5$hom_af, d5$uf, sum, na.rm = T)
ag_uf = sort(ag_uf)
ag_uf = data.frame(uf = names(ag_uf),
                   hom_af = ag_uf)

### Total Homicídio por AF em 10 anos
png('graficos/meta161_indicador7_uf_total_homicidio_af_2002_2012.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -hom_af), y = hom_af)) +
  labs(y = 'Homicídio por AF total', x = 'UF') + 
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Total de Homicídio por AF em 10 anos (2002 à 2012)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador7_uf_total_homicidio_af_2002_2012.csv')

## Total Homicídio por AF taxa
var_uf = d5 %>% group_by(uf) %>%
  mutate(lag.valor = dplyr::lag(hom_af, n = 1, default = NA))

var_uf$crescimento = with(var_uf,{
  (hom_af - lag.valor) / lag.valor
}) 

### Crescimento Acumulado por UF
var_uf = data.frame(uf = levels(var_uf$uf),
                    cresc.acum = tapply(var_uf$crescimento, 
                                        var_uf$uf, sum, na.rm = T),
                    sd.cresc = tapply(var_uf$crescimento, 
                                      var_uf$uf, sd, na.rm = T))

png('graficos/meta161_indicador1_uf_crescimento_acumulado_homicidio_af_taxa.png')

ggplot(var_uf,
       aes(x = reorder(uf, -cresc.acum), y = cresc.acum)) +
  labs(y = 'Crescimento acumulado (%) Homicídio por AF', x = 'UF') + 
  geom_bar(stat = 'identity', position=position_dodge(), fill = 'steelblue') + 
  geom_errorbar(aes(ymin = cresc.acum - sd.cresc, 
                    ymax = cresc.acum + sd.cresc),
                width = .2, position = position_dodge(.9)) +
  tema_massa()

dev.off()

tab_uf = var_uf

colnames(tab_uf) = c('UF', 'Crescimento Acumulado Homicídio por AF (2002 à 2012)')
write_excel_csv(tab_uf,'tabelas/meta161_indicador7_cres_acumulado_homicidio_af_2002_2012.csv')

var_uf = tapply(var_uf$cresc.acum, var_uf$uf, sum, na.rm = T)
var_uf  = sort(var_uf)

### Maior tendência de crescimento 
png('graficos/meta161_indicador7_uf_maior_crescimento_acumulado_homicidio_af_taxa.png')

ggplot(d5[d5$uf %in% names(var_uf[22:27]),],
       aes(x = ano, y = hom_af, colour = uf)) +
  labs(colour = "UF", y = 'Homicídio por AF por 100 mil hab.', x = 'Ano') + 
  geom_line() + tema_massa()

dev.off()

xx = d5[d5$uf %in% names(var_uf[22:27]),]
xx = xx[2:4]
xx = reshape(xx, idvar = 'uf',timevar = 'ano', direction = 'wide')

xx = cbind('Taxa de Homicídio por AF por 100 mil habitantes',xx)

colnames(xx) = c('uf','variável',as.character(2002:2012))

write_excel_csv(xx,'tabelas/meta161_indicador7_maior_homicidio_af_2007_2017.csv')

##########################################################################
##                                                                      ##    
##                  Número de Homicídio entre jovens                    ##
##                                                                      ##          
##########################################################################

# Carregando dados
d6j = read.csv('dados/indicador_8/THOMICMJ.1980-2015.csv',
               sep = ';', fileEncoding = 'UTF-16')

d6j = gather(d6j,ano,hom_jov,2:37)

d6j$ano = as.numeric(substr(d6j$ano,2,5))

d6m = read.csv('dados/indicador_8/THOMICMJ.1980-2015.csv',
               sep = ';', fileEncoding = 'UTF-16')

d6m = gather(d6m,ano,hom_mas,2:37)

d6m$ano = as.numeric(substr(d6m$ano,2,5))

d6f = read.csv('dados/indicador_8/THOMICFJ.1980-2015.csv',
               sep = ';', fileEncoding = 'UTF-16')

d6f = gather(d6f,ano,hom_fem,2:37)

d6f$ano = as.numeric(substr(d6f$ano,2,5))

d6 = merge(d6m,d6f, by = c('ano','uf'))
d6 = merge(d6j,d6, by = c('ano','uf'))

d6$ratio= d6$hom_mas / d6$hom_fem

# Gráficos e Tabelas
### Taxa de Homicídio da População Jovem
ag_uf = tapply(d6$hom_jov, d6$uf, mean, na.rm = T)
ag_uf = sort(ag_uf)
ag_uf = data.frame(uf = names(ag_uf),
                   hom_jov = ag_uf)

png('graficos/meta161_indicador8_uf_taxa_homicidio_jovens_1980_2015.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -hom_jov), y = hom_jov)) +
  labs(y = 'Razão Tx. Homicídio de Jovens', x = 'UF') + 
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Razão Tx. Homicídio de Jovens (1980 à 2015)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador8_uf_taxa_homicidio_jovens_1980_2015.csv')

### Razão do Homicídio Masculino por Feminino 
ag_uf = tapply(d6$ratio, d6$uf, mean, na.rm = T)
ag_uf = sort(ag_uf)
ag_uf = data.frame(uf = names(ag_uf),
                   hom_ratio = ag_uf)

png('graficos/meta161_indicador8_uf_razao_hom_jovens_sexos_1980_2015.png')

ggplot(ag_uf,
       aes(x = reorder(uf, -hom_ratio), y = hom_ratio)) +
  labs(y = 'Razão Tx. Homicídio (Jovens) entre Sexos', x = 'UF') + 
  geom_bar(stat = 'identity', fill = 'steelblue') + tema_massa()

dev.off()

colnames(ag_uf) = c('UF', 'Razão Tx. Homicídio (Jovens) entre Sexos (1980 à 2015)')
write_excel_csv(ag_uf,'tabelas/meta161_indicador8_uf_razao_hom_jovens_sexos_1980_2015.csv')

### Crescimento Acumulado por UF
var_uf = d6 %>% group_by(uf) %>%
  mutate(lag.valor = dplyr::lag(ratio, n = 1, default = NA))

var_uf$crescimento = with(var_uf,{
  (ratio - lag.valor) / lag.valor
}) 

var_uf = data.frame(uf = levels(var_uf$uf),
                    cresc.acum = tapply(var_uf$crescimento, 
                                        var_uf$uf, sum, na.rm = T),
                    sd.cresc = tapply(var_uf$crescimento, 
                                      var_uf$uf, sd, na.rm = T))

png('graficos/meta161_indicador8_uf_crescimento_acumulado_razao_hom_jovens_sexos.png')

ggplot(var_uf,
       aes(x = reorder(uf, -cresc.acum), y = cresc.acum)) +
  labs(y = 'Crescimento acumulado (%) Razão Tx. Homicídio (Jovens) entre Sexos', x = 'UF') + 
  geom_bar(stat = 'identity', position=position_dodge(), fill = 'steelblue') + 
  geom_errorbar(aes(ymin = cresc.acum - sd.cresc, 
                    ymax = cresc.acum + sd.cresc),
                width = .2, position = position_dodge(.9)) +
  tema_massa()

dev.off()

tab_uf = var_uf

colnames(tab_uf) = c('UF', 'Crescimento Acumulado da Razão Tx. Homicídio (Jovens) entre Sexos (1980 à 2015)')
write_excel_csv(tab_uf,'tabelas/meta161_indicador8_cres_acumulado_razao_hom_jovens_sexos_1980_2015.csv')

##########################################################################
##                                                                      ##    
##                  Número de Homicídios Dolosos                        ##
##                                                                      ##          
##########################################################################

# parece já ter sido trabalhado no indicador 1

##########################################################################
##                                                                      ##    
##            Número de Ocorrências Criminais Registradas               ##
##                                                                      ##          
##########################################################################

# sem dados disponíveis
