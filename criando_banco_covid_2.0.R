# novo metodo de dados covid 
# pegando dados do brasil.io

# lendo dados brasil.io

library(tidyverse)

dados_brasil_io <- NULL
dados_brasil_io <- read_csv("https://brasil.io/dataset/covid19/caso?format=csv")

if(is.null(dados_brasil_io)) {
  dados_brasil_io <- read_csv("bancos/covid/brasil.io_reserva.csv")
} else {
  write_csv(dados_brasil_io,"bancos/covid/brasil.io_reserva.csv")
}

dados_covid_poa <- dados_brasil_io %>%
  mutate(date = as.Date(date)) %>%
  filter(state == "RS") %>%
  mutate(municipio = str_to_title(city)) %>%
  filter(municipio == "Porto Alegre") %>%
  select(date,confirmed,deaths,is_last,confirmed_per_100k_inhabitants,death_rate,place_type,estimated_population_2019)

# adicionando semana epidemiologica

semana_epidemio <- read_csv("bancos/semana_epidemio_dia.csv")

dados_covid_poa <- dados_covid_poa %>%
  left_join(semana_epidemio, by = c("date" = "dia"))

rm(dados_brasil_io, semana_epidemio)

