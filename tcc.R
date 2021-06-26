library(tidyr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(hms)

#leitura dos bancos de dados

d1 <- read.csv2("~/Downloads/movimentacao_sobe_desce_29-11-2018-001.csv")
d2 <- read.csv2("C:/Users/allan/Downloads/movimentacao_sobe_desce_29-11-2018-002.csv")
d3<- read.csv2("~/Downloads/movimentacao_sobe_desce_29-11-2018-003.csv")
d4<- read.csv2("~/Downloads/movimentacao_sobe_desce_29-11-2018-004.csv")

dataset <- rbind(d1, d2, d3, d4) #união de todos

dataset$Data.Pesquisa <- dmy(dataset$Data.Pesquisa) ##para entender como data

##vendo quais datas temos
datas <- unique(dataset[c("Data.Pesquisa")])
arrange(datas, desc(datas))



dataset <- dataset %>% 
  dplyr::mutate(Hora.Viagem = format(strptime(lubridate::parse_date_time(Hora.Viagem, 'H:M'),
                                       "%Y-%m-%d %H:%M:%S"),'%H:%M'))

dataset$DT <- with(dataset, ymd(Data.Pesquisa) + hm(Hora.Viagem))

table(is.na(dataset$Hora.Viagem)) ##37 NA

########testando só em d1

d1$Data.Pesquisa <- dmy(d1$Data.Pesquisa)

d1 <- d1 %>% 
  dplyr::mutate(Time = format(strptime(lubridate::parse_date_time(Hora.Viagem, 'H:M'),
                                       "%Y-%m-%d %H:%M:%S"),'%H:%M'))

d1$DT <- with(d1, ymd(Data.Pesquisa) + hm(Time))



##para ver pq deu erradooooooooooo

d2$Data.Pesquisa <- dmy(d2$Data.Pesquisa)

d2$Time<- format(strptime(hm(d2$Hora.Viagem),'%H:%M'))

d2 <- d2 %>% 
  dplyr::mutate(Time = format(strptime(hm(d2$Hora.Viagem)), '%H:%M'))


d2 <- d2 %>% 
  dplyr::mutate(Time = format(strptime(lubridate::parse_date_time(Hora.Viagem, 'H:M'),
                                       "%Y-%m-%d %H:%M:%S"),'%H:%M'))


d2$DT <- with(d2, ymd(Data.Pesquisa) + hm(Time))





##Quero olhar só terca no d1
d1$Data.Pesquisa <- dmy(d1$Data.Pesquisa)

d1 <- d1 %>% 
  dplyr::mutate(Time = format(strptime(lubridate::parse_date_time(Hora.Viagem, 'H:M'),
                                       "%Y-%m-%d %H:%M:%S"),'%H:%M'))

d1$DT <- with(d1, ymd(Data.Pesquisa) + hm(Time))

teste <- subset(d1, d1$Dia.Semana == "Terca     ") ##não usar coluna de dias semana T.T

test <- teste %>% filter(Data.Pesquisa == "2009-02-10")


##Olhando tb tudo que há nesse dia
test2 <- dataset %>% filter(Data.Pesquisa == "2009-02-10")

##Reajustando coordenadas incorretas

pos=which(dataset$Endereco.do.PED=="AVE PADRE JOSE MAURICIO 46                                            ")
dataset$Latitude[pos]=-19.94964
dataset$Longitude[pos]=-43.99045

pos=which(dataset$Endereco.do.PED=="RUA FORMIGA 294                                                       ")
dataset$Latitude[pos]=-19.90432
dataset$Longitude[pos]=-43.94531

pos=which(dataset$Endereco.do.PED=="AVE BERNARDO MONTEIRO 390                                             ")
dataset$Latitude[pos]=-19.92130
dataset$Longitude[pos]=-43.92722

dias=union(as.character(dataset$Dia.Semana),as.character(dataset$Dia.Semana))

rotas=intersect(dataset$Rota,dataset$Rota)





