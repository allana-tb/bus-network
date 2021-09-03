setwd("D:/")
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
df=subset(dataset,Dia.Semana==dias[1]) ##para considerar apenas um dos dias
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
  addCircleMarkers(color = rgb(1,0,0,0.3),radius=0.5)


# Informação da rede

c(length(V(G)),length(E(G)))
## 9572 14865

##faz-se o mesmo para todos os dias, acrescentando esse parâmetro no início

#grau dos pontos da rede completa
summary(degree(g))

# the diameter of each component in a random graph
components <- decompose(g, min.vertices=2)
sapply(components, diameter)


library(hrbrthemes)
library(ggplot2)

# dataset:
datatest= as.data.frame(E(g)$weight)

# basic histogram
p <- ggplot(datatest, aes(x=E(g)$weight)) + 
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
p

summary(peso)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.0   105.2   260.5   367.0   548.0  1738.0 




###dia 1 - sábado
sabado=subset(dataset,Dia.Semana==dias[1])
saldo = sabado$Saldo
saldos2 <- as.data.frame(saldo)

# basic histogram
p <- ggplot(saldos2, aes(x=saldo)) + 
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  ggtitle( "Sábado")+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
p


segunda=subset(dataset,Dia.Semana==dias[7])
saldo = segunda$Saldo
saldos3 <- as.data.frame(saldo)

# basic histogram
p <- ggplot(saldos3, aes(x=saldo)) + 
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  ggtitle( "Segunda")+
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
p

##comparação domingo com e sem campanha de popularizaão do teatro
dd <- unique(domingo$Data.Pesquisa)

dplyr::arrange(dd, desc(dd)) 

##tratando os dados
cdd<- filter(domingo, Data.Pesquisa == c("2011-02-20", "2011-02-27") )
ccdd <- filter(domingo, Data.Pesquisa == c("2011-03-13", "2011-10-16") )

compdomingo = data.frame(
  x=cdd$Saldo[1:825],
  y= ccdd$Saldo
)

# Create 2 vectors
a <- cdd$Saldo[1:825]
b <- ccdd$Saldo

# Make a list of these 2 vectors
C <- list(a,b)

# Change the names of the elements of the list :
names(C) <- c(paste("Campanha"), paste("Controle"))

# Change the mgp argument: avoid text overlaps axis
par(mgp=c(1,1,0))

# Final Boxplot
boxplot(C , col="#69b3a2", main = "Saldo de pessoas - 2011")



##tratando os dados
cdd<- filter(domingo, Data.Pesquisa == c( "2016-02-21", "2016-02-28") )
ccdd <- filter(domingo, Data.Pesquisa == c("2016-03-13", "2016-03-20") )

compdomingo = data.frame(
  x=cdd$Saldo[1:4337],
  y= ccdd$Saldo
)

# Create 2 vectors
a <- cdd$Saldo[1:4337]
b <- ccdd$Saldo

# Make a list of these 2 vectors
C <- list(a,b)

# Change the names of the elements of the list :
names(C) <- c(paste("Campanha"), paste("Controle"))

# Change the mgp argument: avoid text overlaps axis
par(mgp=c(1,1,0))

# Final Boxplot
boxplot(C , col="#69b3a2", main = "Saldo de pessoas - 2016")



##tratando os dados
cdd<- filter(domingo, Data.Pesquisa == c( "2018-02-25", "2018-03-04") )
ccdd <- filter(domingo, Data.Pesquisa == c( "2018-03-11", "2018-03-18") )

compdomingo = data.frame(
  x=cdd$Saldo,
  y= ccdd$Saldo[1:524]
)

# Create 2 vectors
a <- cdd$Saldo
b <- ccdd$Saldo[1:524]

# Make a list of these 2 vectors
C <- list(a,b)

# Change the names of the elements of the list :
names(C) <- c(paste("Campanha"), paste("Controle"))

# Change the mgp argument: avoid text overlaps axis
par(mgp=c(1,1,0))

# Final Boxplot
boxplot(C , col="#69b3a2", main = "Saldo de pessoas - 2018")







##mapa com valores do dia

difund1 <- domingo
cordomingod <- difund1 %>% group_by(Endereco.do.PED) %>% summarise(avg = mean(Desembarque)) %>%
  arrange(desc(avg))
cordomingod2 <- difund1 %>% group_by(Endereco.do.PED) %>% summarise(avg2 = mean(Embarque)) %>%
  arrange(desc(avg2))
cordomingod2 <- as.data.frame(cordomingod2)
cordomingod <- as.data.frame(cordomingod)
colnames(cordomingod)[1] <- "name"
colnames(cordomingod2)[1] <- "name"
cordomingods <- merge(cordomingod, cordomingod2)


##mapa com valores 

difund <- dataset
difund %>% group_by(Endereco.do.PED) %>% summarise(avg = mean(Desembarque)) %>% arrange(desc(avg))

#retirando dados
corbus1 <- difund %>% group_by(Endereco.do.PED) %>% summarise(avg1 = mean(Desembarque)) %>%
  arrange(desc(avg1))
corbus2 <- difund %>% group_by(Endereco.do.PED) %>% summarise(avg2 = mean(Embarque)) %>%
  arrange(desc(avg2))
corbus3 <- difund %>% group_by(Endereco.do.PED) %>% summarise(avg = mean(Saldo)) %>%
  arrange(desc(avg))

corbus1 <- as.data.frame(corbus1)
corbus2 <- as.data.frame(corbus2)
corbus3 <- as.data.frame(corbus3)

colnames(corbus1)[1] <- "name"
colnames(corbus2)[1] <- "name"
colnames(corbus3)[1] <- "name"

corbus <- full_join(corbus1, corbus2, by= "name")
corbus <- full_join(corbus, corbus3, by= "name")

##colocando no mapa
vertbus <- merge(vert,corbus, by="name")

##criando paleta
#spalette <- colorBin(palette = "YlGnBu", domain=vertbus$avg2, na.color="transparent", bins=mybins)
mybins <- quantile(vertbus$avg)
pal <- colorBin(palette = "Purples", domain = vertbus$avg, na.color="transparent", alpha = 0.9, bins = mybins)

mybins1 <- quantile(vertbus$avg1)
pal1 <- colorBin(palette = "YlGnBu", domain = vertbus$avg1, na.color="transparent", alpha = 0.9, bins = mybins1)

mybins2 <- quantile(vertbus$avg2)
pal2 <- colorBin(palette = "YlOrBr", domain = vertbus$avg2, na.color="transparent", alpha = 0.9, bins = mybins2)



leaflet(vertbus) %>% addTiles() %>% 
  addPolylines(data = edges, weight = 1.5, opacity = 0.15, color = "black", fillOpacity = 0.9) %>% 
  addCircleMarkers(fillColor =  ~pal(avg), fillOpacity = 0.7, color="white", radius=3, stroke=FALSE, group="Saldo") %>%
  addCircleMarkers(fillColor = ~pal1(avg1), fillOpacity = 0.7, color="white", radius=3, stroke=FALSE, group="Desembarque")%>%
  addCircleMarkers(fillColor = ~pal2(avg2), fillOpacity = 0.7, color="white", radius=3, stroke=FALSE, group="Embarque")%>%
  
  #legenda
  addLegend( pal=pal, values=~avg, opacity=0.9, title = "Saldo", position = "bottomright", group="Saldo") %>% 
  addLegend( pal=pal1, values=~avg1, opacity=0.9, title = "Desembarque", position = "bottomright", group="Desembarque" ) %>%
  addLegend( pal=pal2, values=~avg2, opacity=0.9, title = "Embarque", position = "bottomright", group="Embarque" ) %>%
  
  addLayersControl(overlayGroups = c("Saldo","Desembarque", "Embarque"), options = layersControlOptions(collapsed = FALSE))




