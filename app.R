library(shiny)
library(bslib)
library(tidyr)
library(tidyverse)
library(dplyr)
library(sass)
library(leaflet)
library(sp)
library(igraph)
library(rmarkdown)

dataset <- readRDS("dataset.RDS")
vertbus <- readRDS("vertbus.RDS")
edges <- readRDS("edges.RDS")


dias=union(as.character(dataset$Dia.Semana),as.character(dataset$Dia.Semana))
rotas=intersect(dataset$Rota,dataset$Rota)
df

bs_global_theme()
bs_global_add_variables( "body-bg" = "")


##define ui
ui <- fluidPage(
    titlePanel("Rede de transporte coletivo de Belo Horizonte"),
##agora, mostrando o mapa gerado
     leafletOutput("map",  width = "1000", height = "1000"),
 absolutePanel(top = 10, right= 10, 
               selectInput("Pesos", label = "Pesos",
                           choices = c("Embarque", "Desembarque", "Saldo")
            ))
)


##server

server <- function(input, output){
    
    ##criando o mapa
    output$map <- renderLeaflet({ 
        
        leaflet() %>% addTiles()%>% 
            addPolylines(data = edges, weight = 1.5, opacity = 0.15, color = "black", fillOpacity = 0.9)  
    })
    
        observe({
            
            peso <- input$Pesos
            if (is.null(peso))             
                return()
        
            mybins <- quantile(vertbus$avg)
            pal <- colorBin(palette = "Purples", domain = vertbus$avg, na.color="transparent", alpha = 0.9, bins = mybins)
            
            mybins1 <- quantile(vertbus$avg1)
            pal1 <- colorBin(palette = "YlGnBu", domain = vertbus$avg1, na.color="transparent", alpha = 0.9, bins = mybins1)
            
            mybins2 <- quantile(vertbus$avg2)
            pal2 <- colorBin(palette = "YlOrBr", domain = vertbus$avg2, na.color="transparent", alpha = 0.9, bins = mybins2)
            
            leafletProxy("map", data = vertbus) %>% 
                addCircleMarkers(fillColor =  ~pal(avg), fillOpacity = 0.7, color="white", radius=3, stroke=FALSE, group="Saldo") %>%
                addCircleMarkers(fillColor = ~pal1(avg1), fillOpacity = 0.7, color="white", radius=3, stroke=FALSE, group="Desembarque")%>%
                addCircleMarkers(fillColor = ~pal2(avg2), fillOpacity = 0.7, color="white", radius=3, stroke=FALSE, group="Embarque") %>%
                #legenda
                addLegend( pal=pal, values=~avg, opacity=0.9, title = "Saldo", position = "bottomright", group="Saldo") %>% 
                addLegend( pal=pal1, values=~avg1, opacity=0.9, title = "Desembarque", position = "bottomright", group="Desembarque" ) %>%
                addLegend( pal=pal2, values=~avg2, opacity=0.9, title = "Embarque", position = "bottomright", group="Embarque" ) %>%
                addLayersControl(overlayGroups = peso, options = layersControlOptions(collapsed = FALSE))
        
         })
}


shinyApp(ui = ui, server = server)