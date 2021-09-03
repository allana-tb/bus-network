##TRATAMENTO DE DADOS E FORMAÇÃO DA REDE

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


#Agora aqui uma coisa interessante - dado o dia temos o dataset do dia.


load("dataset completo.Rda")

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
df=subset(dataset,Dia.Semana==dias[3])
DF=df
df=subset(dataset,Rota==rotas[1])
linha=intersect(as.character(df$Linha.SubLinha),as.character(df$Linha.SubLinha))
d=data.frame(de="a",para="b")

peso=c()

for(onibus in linha){
  
  df1=subset(df,Linha.SubLinha==onibus)
  periodo=nrow(df1)/max(df1$Sequencia.do.PED)
  v=df1$Endereco.do.PED[1:max(df1$Sequencia.do.PED)]
  
  d=rbind(d,data.frame(de=v[1:(length(v)-1)],para=v[2:length(v)]))
  
}

d=d[2:nrow(d),]
d=na.omit(d)

g=graph.data.frame(d,directed=TRUE)
g=simplify(g)
elos=get.edgelist(g)

peso=c()

for(rua in elos[,1]){
  
  peso=c(peso,sum(df$Saldo[which(df$Endereco.do.PED==rua)]))
  
}


E(g)$weight=peso
g1=g
df=subset(dataset,Rota==rotas[2])
linha=intersect(as.character(df$Linha.SubLinha),as.character(df$Linha.SubLinha))
d=data.frame(de="a",para="b")

peso=c()

for(onibus in linha){
  
  df1=subset(df,Linha.SubLinha==onibus)
  periodo=nrow(df1)/max(df1$Sequencia.do.PED)
  v=df1$Endereco.do.PED[1:max(df1$Sequencia.do.PED)]
  
  d=rbind(d,data.frame(de=v[1:(length(v)-1)],para=v[2:length(v)]))
  
}

d=d[2:nrow(d),]
d=na.omit(d)
g=graph.data.frame(d,directed=TRUE)
g=simplify(g)
elos=get.edgelist(g)

peso=c()

for(rua in elos[,1]){
  
  peso=c(peso,sum(df$Saldo[which(df$Endereco.do.PED==rua)]))
  
}

E(g)$weight=peso
g2=g
df=subset(dataset,Rota==rotas[3])
linha=intersect(as.character(df$Linha.SubLinha),as.character(df$Linha.SubLinha))
d=data.frame(de="a",para="b")

peso=c()

for(onibus in linha){
  
  df1=subset(df,Linha.SubLinha==onibus)
  periodo=nrow(df1)/max(df1$Sequencia.do.PED)
  v=df1$Endereco.do.PED[1:max(df1$Sequencia.do.PED)]
  
  d=rbind(d,data.frame(de=v[1:(length(v)-1)],para=v[2:length(v)]))
  
}

d=d[2:nrow(d),]
d=na.omit(d)

g=graph.data.frame(d,directed=TRUE)
g=simplify(g)
elos=get.edgelist(g)

peso=c()

for(rua in elos[,1]){
  
  peso=c(peso,sum(df$Saldo[which(df$Endereco.do.PED==rua)]))
  
}


E(g)$weight=peso
g3=g
e1=as.data.frame(get.edgelist(g1))
e1=rbind(e1,as.data.frame(get.edgelist(g2)))
e1=rbind(e1,as.data.frame(get.edgelist(g3)))

g=graph.data.frame(e1)
E(g)$weight=c(E(g1)$weight,E(g2)$weight,E(g3)$weight)
g=simplify(g)
G=g

#plot(E(g)$weight)

#Agora vamos fazer a figura da rede

df=DF
v=union(names(V(G)),names(V(G)))
lat=c()
long=c()

for(j in v){
  
  lat=c(lat,dataset$Latitude[which(dataset$Endereco.do.PED==j)[1]])
  long=c(long,dataset$Longitude[which(dataset$Endereco.do.PED==j)[1]])
  
  print(length(lat)-length(V(G)))
  
}


require(leaflet)
require(sp)


meta=data.frame(node=v,lat=lat,lon=long)

#meta=data.frame(node=1:length(D1[,1]),lat=D1[["lat"]],lon=D1[["lon"]])

str(meta)

g=graph.data.frame(as.data.frame(get.edgelist(G)),directed=TRUE,vertices=meta)

gg <- get.data.frame(g, "both")
vert <- gg$vertices
coordinates(vert) <- ~lon+lat

edges <- gg$edges

edges <- lapply(1:nrow(edges), function(i) {
  as(rbind(vert[vert$name == edges[i, "from"], ], 
           vert[vert$name == edges[i, "to"], ]), 
     "SpatialLines")
})


for (i in seq_along(edges)) {
  
  edges[[i]] <- spChFIDs(edges[[i]], as.character(i))
  
}

edges <- do.call(rbind, edges)

leaflet(vert) %>% addTiles() %>% 
  addPolylines(data = edges, weight = 1.5, opacity = 0.15, color = "black", fillOpacity = 0.9) %>% 
  addCircleMarkers(color = rgb(1,0,0,0.3), alpha = quantile(degree(g)), radius=0.5)


# Informação da rede

c(length(V(G)),length(E(G)))







