library("data.table")
library("readxl")
library(dplyr)
library(ggplot2)
library(viridis)
library(lubridate)
library("knitr")
library(extrafont)
library(purrr)
options(scipen=9999)
loadfonts()


temaMobills <- theme(
    text = element_text(family="CM Roman"),
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill="black"),
    axis.title = element_text(size=10),
    panel.grid.major = element_line(linetype=2,size=0.1,colour="grey60"),
    panel.grid.minor = element_line(linetype=2,size=0.1,colour="grey20"),
    axis.text = element_text(colour="white"),
    axis.title.x  = element_text(colour="white"),
    plot.title = element_text(colour="white",family = "CM Roman",size=14),
    plot.subtitle = element_text(size=10,colour="white"),
    legend.position = "none")




##------------------------------ DATA READING --------------------------------------------
Receitas <- data.table::fread("~/Área de trabalho/dadosReceitasUsers/Receitas20190711.csv")
Receitas2018 <- data.table::fread("~/Área de trabalho/dadosReceitasUsers/Receitas20180107.csv")
Despesas <- data.table::fread("~/Área de trabalho/dadosReceitasUsers/Despesas20190711.csv")
Despesas2018 <- data.table::fread("~/Área de trabalho/dadosReceitasUsers/Despesas20180107.csv")
ReceitasCat <-data.table::fread("~/Área de trabalho/dadosReceitasUsers/TipoREceitas.csv") 
DespesasCat <-data.table::fread("~/Área de trabalho/dadosReceitasUsers/TipoDespesas.csv")

Receitas <- do.call("rbind", list(Receitas,Receitas2018))
Despesas <- do.call("rbind", list(Despesas,Despesas2018))
dim(Receitas)
dim(Despesas)

Despesas %>% mutate(ano= lubridate::year(DataDespesa)) -> Despesas


Despesas$Valor <- gsub(pattern = ",",replacement = ".", Despesas$Valor)
Receitas$Valor <- gsub(pattern = ",",replacement = ".", Receitas$Valor)
Despesas$Valor <- as.numeric(Despesas$Valor)
Receitas$Valor <- as.numeric(Receitas$Valor)
par(mfrow=c(1,1))
##-----------------------------------------------------------------------------------------

##------------------------ Revenue analisys -----------------------------------------------

## --------------- POOLED HISTOGRAM ------------------
summary(Despesas$Valor)
hist(Despesas$Valor[Despesas$Valor < 4000 & Despesas$Valor > 0])
## --------------- Hist by year ----------------------

Despesas %>% dplyr::filter(Valor > 0 & Valor < 2000) %>%
    ggplot(aes(Valor))+
    geom_histogram(fill="white",alpha=0.8)+
    facet_grid(~ano)+temaMobills+
        labs(title="Distribuição das despesas")
    
Despesas
r
Despesas$Valor %>% summary()

##-------------------------------------------------------------------------------------------
Despesas %>%
  group_by(UsuarioId,
           dia = lubridate::day(DataDespesa),
           mes = lubridate::month(DataDespesa)) %>%
    summarise(count = n(), valorSoma= sum(Valor)) %>%
    arrange(desc(valorSoma)) -> desp

desp %>%
  group_by(mes) %>%
  summarise(contagem=n()) %>%
  ggplot(aes(mes, contagem,labs=contagem))+geom_col(aes(fill=contagem),width = 0.5)+
  scale_fill_viridis(option="magma",begin=0.5)+
  labs(title="Quantidade de Despesas por mês",
       subtitle = "Usuarios de 16 à 24 anos, no periodo de 11 de Julho à 10 de Outubro de 2019",
       x="Mês",
       y="Quantidade de despesas")+
  theme(
    text = element_text(family="CM Roman"),
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill="black"),
    axis.title = element_text(size=12),
    panel.grid.major = element_line(linetype=2,size=0.1,colour="grey60"),
    panel.grid.minor = element_line(linetype=2,size=0.1,colour="grey20"),
    axis.text = element_text(colour="white"),
    axis.title.x  = element_text(colour="white"),
    plot.title = element_text(colour="white",family = "CM Roman",size=14),
    plot.subtitle = element_text(size=10,colour="white"),
    legend.position = "none")+
  scale_y_continuous(limits = c(0,100000),expand=c(0.01009,0.000000001),breaks = seq(0,100000,10000))+
    geom_text(aes(label=contagem),margin=-2,size=3.5,colour="white",vjust=-0.2)

##-----------------------------------------------------------------------------------------

Despesas %>%
    group_by(Descricao) %>%
    summarise(count = n(), valorSoma= sum(Valor)) 
    %>% top_n(20) %>% arrange(desc(count))

desp %>% mutate(chave = paste0(UsuarioId,mes)) -> desp
rec %>% mutate(chave = paste0(UsuarioId,mes)) -> rec



Receitas %>%
group_by(UsuarioId, mes = lubridate::month(DataReceita)) %>%
    summarise(count = n(), valorSoma= sum(Valor)) %>%
    arrange(desc(valorSoma)) 

left_join(desp,rec, by = c('chave' = 'chave')) -> teste
teste
teste %>%
    na.omit() %>%
    mutate(superavit = valorSoma.y-valorSoma.x,supVezes = valorSoma.x/valorSoma.y)%>%
    mutate(sup = ifelse(superavit<0,-1,1)) -> teste

teste %>% group_by(sup) %>% summarise(count= n())

teste$superavit
teste %>% group_by(sup)%>% summarise(count=n())

teste %>% filter(supVezes <1) %>%View()

Despesas %>%
    group_by(Descricao,Valor) %>%
    summarise(count = n()) %>%
    arrange(desc(Valor))%>% top_n(30) %>% filter(Valor<10000) %>% View()

teste %>% group_by(chave,mes.x) %>% summarise(somames = sum(valorSoma.x)) -> dadosts

teste %>% group_by(chave,mes.y) %>% summarise(somames = sum(valorSoma.y)) -> dadoRec
plot.ts(dadosts$mes.x,dadosts$somames)
hist(dadoRec$somames[dadoRec$somames<20000])

hist(dadosts$somames[dadosts$somames<30000])
dev.new()

Despesas %>% group_by(DataDespesa) %>%
    summarise(count = n()) %>%
    unique() %>%
    arrange(desc(count)) ->serie

e##Receitas x Despesas


plot.ts(serie$DataDespesa,serie$count)

serie$DataDespesa
regressao <- unique(teste)
plot(regressao$valorSoma.y[1:10],regressao$valorSoma.x[1:10])
as.ts(serie) %>%plot.ts()
ts(data=serie)
serie %>% ggplot(aes(x=1:length(count),y=count))+
    geom_line()+
    theme(axis.text.x=element_blank())

unique(serie$DataDespesa) %>% length()


somaDespesas <- Despesas %>% group_by(UsuarioId,DataDespesa)
cruzamento <- inner_join(Receitas,Despesas,by=c('UsuarioId' = 'UsuarioId'))

length(Receitas)
SampleReceita <- Receitas[,sample(length(Receitas$Anexo),size=1000)]

View(Receitas)
u

length(unique(teste$UsuarioId))

teste %>% seleect()
