library(shiny)
library(ggplot2)
#############################################################pacotes da leitura do mapa
library(tidyverse)
library(sf)
library(stringr)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(DT)
library(leafpop)
library(readxl)
library(rgeos) #https://statistique-et-logiciel-r.com/premiers-pas-en-cartographie-avec-r/
library(viridis)
library(rainbow)
library(httr)
library(curl)
library(abjutils)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(scales)
library(lubridate)
library(shinyEffects)
library(shinyalert)
library(shinyBS)

#options(OutDec= ",")
# infelizmente esse comando causa um erro nas barras de progresso
# enquanto não se arranjar solução vou deixar comentado o comando

source("criando_banco_covid_2.0.R", encoding = "UTF-8")
source("my_ui.R", encoding = "UTF-8")
source("my_server.R", encoding = "UTF-8")



shinyApp(ui, server)

