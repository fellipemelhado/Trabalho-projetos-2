graphics.off()
rm(list=ls())
load.lib <- c('readxl', 'tidyverse', 'reshape2', "basedosdados", "googlesheets4", "ggthemes",
              'DBI', 'bigrquery', 'xts', 'bayesforecast', 'aTSA' , 'vars', 'tseries',
              'dygraphs', 'lubridate', 'geobr', 'sf', 'data.table', 'glue', 'ggtext', 'paletteer',
              'scales', 'gt', 'gtsummary', 'gapminder', 'knitr')
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

# --------------------------------------------------------------------------------
# Baixando as bases com Bigquery manualmente (demora muito, nao precisa rodar)
bq_auth("C:/Users/admin/Downloads/binbon-368014-963d4ff3d38b.json")
con <- dbConnect(
  bigrquery::bigquery(),
  billing = "binbon-368014",
  project = "basedosdados"
)
query = "SELECT * FROM `basedosdados.br_anp_precos_combustiveis.microdados`"
df.sim = dbGetQuery(con, query)

# Exportando e importanto de volta a base
saveRDS(df.sim, file = 'df.sim.rds')
# ---------------------------------------------------------------------------------
df.sim = read_rds('df.sim.rds') # Rodar a partir daqui

# Tirando colunas desnecessarias e tirando os NAs da base
df = df.sim[,-c(5,6,7,8)]
df = na.omit(df)


#Limpando o espaco, devido ao grande processamento pra baixar a base
rm(df.sim)
gc()

# Sample na base pra usar de maneira mais pratica enquanto o codigo eh feito
set.seed(295)
df = sample_n(df, 500000)


#Criando vetores para fazer novas colunas dps
bandeiras = c('IPIRANGA', 'PETROBRAS DISTRIBUIDORA S.A.', 
              'ALE COMBUSTÍVEIS', 'BRANCA')
diesel = c('diesel s10', 'diesel s50')
etanol = 'etanol'
gasolina = c('gasolina', 'gasolina aditivada')
gases = c('glp', 'gnv')
capitais = c("1100205","1302603","1200401","5002704","1600303","5300108","1400100","5103403","1721000","3550308","2211001","3304557","1501402","5208707","2927408","4205407","2111300","2704302","4314902","4106902","3106200","2304400","2611606","2507507","2800308","2408102","3205309")
    #Nao tinha como importar o codigo das capitais usando o geobr

#Dummies importantes para fazer regressao dps
df$dummyb = ifelse(df$bandeira_revenda %in% bandeiras, 1, 0) # importante para uma futura regressao
df$dummyc = ifelse(df$id_municipio %in% capitais, 1, 0) 

#Renomendoando as colunas e criando uma coluna resumindo os tipos de bandeiras
colnames(df) = c('ano', 'sigla', 'id', 'bairro', 'bandeira', 'data_coleta', 'produto', 'unidade', 'compra', 'preco', 'dummyb', 'capital')
df$band = ifelse(df$bandeira %in% bandeiras, df$bandeira, 'Bandeiras Menores')

#Lendo e criando uma dataframe com informacoes dos estados e regioes do brazil
regiao = read_state(year = 2019, showProgress = F, simplified = T) %>% dplyr::select(abbrev_state, name_region) %>% 
  rename(sigla = abbrev_state, regiao = name_region)

#Criando uma dataframe sem a coluna de geolocalizacao, oq deixa a base muito pesada
estados = as.data.frame(cbind(as.character(regiao$sigla), as.character(regiao$regiao))) %>%
  rename(sigla = V1, regiao = V2)

#Arrumando os valores das colunas com a devida classe das vairiaveis
df = mutate(df, sigla = as.character(sigla), id = as.numeric(id),
            bairro = as.character(bairro), bandeira = as.character(bandeira),
            data_coleta = as.Date(data_coleta), produto = as.character(produto),
            unidade = as.character(unidade), compra = as.numeric(compra), preco = as.numeric(preco))


#Merge da maneira mais pesada, porem mais pratica
df = left_join(df, estados, by = 'sigla')

#usando data table, se nao rodar usando o left join usar como data frame
#estados <- data.table(estados, key = "sigla")
#df <- data.table(df, key = "sigla")
#df = as.data.frame(merge(df, estados))


#mini bases referente a cada tipo de produto
dfd = filter(df, produto == 'diesel')
dfds = filter(df, produto %in% diesel)
dfe = filter(df, produto == etanol)
dfg = filter(df, produto %in% gasolina)
dfglp = filter(df, produto == 'glp')
dfgnv = filter(df, produto == 'gnv')



#Graficos#

#historico de preco medio de cada combustivel ao decorrer dos anos
  #Sempre sera criada uma base nova para cada grafico
dfph = filter(df, produto != 'glp')
dfph = aggregate(dfph$preco, list(dfph$ano), FUN = mean) %>% 
  rename(data = Group.1, media = x) 
ggplot(dfph, aes(x = data, y = media)) + geom_line() + ggtitle('Histórico dos precos', subtitle = 'Fonte: Agência Nacional do Petróleo, Gás Natural e Biocombustíveis') +
  theme(plot.title = element_text(face = 'bold'),legend.title = element_text(face = 'bold',),
        legend.text = ggtext::element_markdown(), legend.key.height = unit(2, 'cm')) + xlab('Data') + ylab('Preco R$')

graphhp = function(base, domaterial){
  base = aggregate(base$preco, list(base$ano), FUN = mean) %>% 
    rename(data = Group.1, media = x)
  
  ggplot(base, aes(x = data, y = media)) + geom_line() + ggtitle(glue('Histórico do preco {domaterial}'), subtitle = 'Fonte: Agência Nacional do Petróleo, Gás Natural e Biocombustíveis') +
    theme(plot.title = element_text(face = 'bold'),legend.title = element_text(face = 'bold',)) + xlab('Data') + ylab('Preco R$')
}
graphhp(dfd, 'do diesel')
graphhp(dfds, 'do diesel s10 e s50')
graphhp(dfe, 'do entalo')
graphhp(dfg, 'da gasolina')
graphhp(dfglp, 'dp glp')
graphhp(dfgnv, 'dp gnv')


#Graficos do Mapa do brasil
  #Sempre sera criada uma base nova para cada grafico
no_axis <- theme(axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(), legend.title = element_text(face = 'bold',))
  # Usando isso para nao tem nada no fundo do grafico


#preco do combustivel geral por estado
df13 = filter(df, ano == 2019) %>% filter(produto != 'glp')
dfbr1 =  aggregate(df13$preco, list(df13$sigla), FUN = mean) %>% 
  rename(sigla = Group.1, media = x) %>%
  left_join(regiao, by = 'sigla')

ggplot() +
  geom_sf(data=dfbr1, aes(fill=media, geometry = geom), color= NA, size=.15) +
  scale_fill_distiller(palette = 'YlOrRd', name="Preco dos combustiveis", direction = 1) +
  theme_minimal() + no_axis + 
  ggtitle('Preco por estado', subtitle = 'Fonte: Agência Nacional do Petróleo, Gás Natural e Biocombustíveis')

# Criando a funcao para cada tipo de combustivel
graphbr = function(base, material) {
  base = filter(base, ano == 2019)
  base = aggregate(base$preco, list(base$sigla), FUN = mean) %>% 
    rename(sigla = Group.1, media = x) %>%
    left_join(regiao, by = 'sigla')
  ggplot() +
    geom_sf(data=base, aes(fill=media, geometry = geom), color= NA, size=.15) +
    scale_fill_distiller(palette = "YlOrRd", name = glue('Preco R$'), direction = 1) +
    theme_minimal() +
    no_axis +
    ggtitle(glue('Preco por estado {material}'), subtitle = 'Fonte: Agência Nacional do Petróleo, Gás Natural e Biocombustíveis')
}

#preco de diesel por estado
graphbr(dfd, 'do diesel')

#preco de diesel s10 e s50 por estado
graphbr(dfds, 'do diesel s10 e s50')


#preco do etanl por estado
graphbr(dfe, 'do entaol')


#preco de gasolina
graphbr(dfg, 'da gasolina')


# Preco do glp
graphbr(dfglp, 'do glp')


# Preco do gnv
graphbr(dfgnv, 'do gnv')


##################################################
labelsband <- c(
  `ALE COMBUSTÍVEIS` = "<img src = 'ale.jpg' width='50' /><br>", 
  `Bandeiras Menores` = "Bandeiras Menores", BRANCA = 'Branca', 
  IPIRANGA = "<img src='ipiranga.jpg' width='50' /><br>",
  `PETROBRAS DISTRIBUIDORA S.A.` = "<img src='petrobras.jpg' width='50' /><br>"
)

#Porcentagem do tipo de produto, grafico pizza
dfpc2 = as.data.frame(table(df$produto)) %>% rename(produto = Var1, numero = Freq) %>% mutate(numero = percent(numero/sum(numero) ,accuracy = 0.01),)
ggplot(dfpc2, aes(x = '', y = numero, fill = produto)) + geom_bar(width = 1, stat = 'identity') +
  coord_polar('y', start = 0) + ggtitle('Porcentagem do tipo de produto', subtitle = 'Fonte: Agência Nacional do Petróleo, Gás Natural e Biocombustíveis') +
  scale_fill_discrete(name = 'Tipo de combustíveis', labels = labelsband) + geom_text(aes(label = numero),position = position_stack(vjust = 0.5)) +
  labs(fill = 'Tipo de combustíveis') + theme(plot.title = element_text(face = 'bold'),legend.title = element_text(face = 'bold'), legend.text = ggtext::element_markdown(),
                                   axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(), panel.grid = element_blank())


#######################################################
# Preco historico bandeira com todas
dfphb = filter(df, produto != 'glp') %>% aggregate(preco ~ band + ano, FUN = mean)

ggplot(dfphb, aes(x = ano, y = preco, color = band)) + geom_line() +
  scale_color_discrete(name = 'Bandeiras', labels = labelsband) +
  theme_minimal() + ggtitle('Preco histórico por bandeira', subtitle = 'Fonte: Agência Nacional do Petróleo, Gás Natural e Biocombustíveis') +
  theme(plot.title = element_text(face = 'bold'),legend.title = element_text(face = 'bold',),
        legend.text = ggtext::element_markdown(), legend.key.height = unit(2, 'cm')) + xlab('Data') + ylab('Preco R$')

# Preco historico por tipo de produto
graphphb = function(base, domaterial){
  base = aggregate(preco ~ band + ano, data = base, FUN = mean)
  ggplot(base, aes(x = ano, y = preco, color = band)) + geom_line() +
    scale_color_discrete(name = 'Bandeiras', labels = labelsband) + 
    theme_minimal() + ggtitle(glue('Preco histórico {domaterial} por bandeira'), subtitle = 'Fonte: Agência Nacional do Petróleo, Gás Natural e Biocombustíveis') +
    theme(plot.title = element_text(face = 'bold'),legend.title = element_text(face = 'bold',),legend.text = ggtext::element_markdown()) +
    xlab('Data') + ylab('Preco R$')
  
  
  
  }
graphphb(dfd, 'do diesel')
graphphb(dfds, 'do diesel s10 e s50')
graphphb(dfe, 'do etanol')
graphphb(dfg, 'da gasolina')
graphphb(dfglp, 'do glp')
graphphb(dfgnv, 'do gnv')

# Pie chart da porcentagem das bandeiras(bandeiras grandes especificadas e nao especificadas)
dfpc = as.data.frame(table(df$band)) %>% rename(bandeiras = Var1, numero = Freq) %>% mutate(numero = percent(numero/sum(numero) ,accuracy = 0.001),)
ggplot(dfpc, aes(x = '', y = numero, fill = bandeiras)) + geom_bar(width = 1, stat = 'identity') +
  coord_polar('y', start = 0) + ggtitle('Porcentagem dos postos por bandeira', subtitle = 'Fonte: Agência Nacional do Petróleo, Gás Natural e Biocombustíveis') +
  scale_fill_discrete(name = 'Bandeiras', labels = labelsband) + geom_text(aes(label = numero),position = position_stack(vjust = 0.5)) +
  labs(fill = 'Bandeiras') + theme(plot.title = element_text(face = 'bold'),legend.title = element_text(face = 'bold'), legend.text = ggtext::element_markdown(),
                                   axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(), panel.grid = element_blank())

###############################

#Fazer tabelinhasc

tabl1 = filter(df, produto != 'glp') %>%  filter(ano == 2019) %>%                             # Summary by group using dplyr
  group_by(sigla) %>% 
  summarize(min = min(preco),
            mediana = median(preco),
            media = mean(preco),
            max = max(preco),
            devpd = sd(preco),
            var = var(preco)) %>% rename(UF = sigla)
kable(tabl1, format = 'latex', align = 'lcccccc')


tabl2 = filter(df, produto != 'glp') %>%  filter(ano == 2019) %>%                              # Summary by group using dplyr
  group_by(band) %>% 
  summarize(min = min(preco),
            mediana = median(preco),
            media = mean(preco),
            max = max(preco),
            devpd = sd(preco),
            var = var(preco)) %>% rename(Bandeira = band)
kable(tabl2, format = 'latex', align = 'lcccccc')

tabl3 = df   %>%  filter(ano == 2019) %>%                    # Summary by group using dplyr
  group_by(produto) %>% 
  summarize(min = min(preco),
            mediana = median(preco),
            media = mean(preco),
            max = max(preco),
            devpd = sd(preco),
            var = var(preco)) %>% rename(Produto = produto)
kable(tabl3, format = 'latex', align = 'lcccccc')

tabl4 = df   %>%  filter(ano == 2019) %>%                    # Summary by group using dplyr
  group_by(regiao) %>% 
  summarize(min = min(preco),
            mediana = median(preco),
            media = mean(preco),
            max = max(preco),
            devpd = sd(preco),
            var = var(preco)) %>% rename(Regiao = regiao)
kable(tabl4, format = 'latex', align = 'lcccccc')
#------------------------------------

