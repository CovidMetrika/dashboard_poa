# Data wrangling

# novo metodo de dados covid 
# pegando dados da SES-RS

library(tidyverse)
library(httr)
library(lubridate)
library(httr)
library(jsonlite)

# lendo dados da SES-RS

dados_ses <- NULL

path <- "http://ti.saude.rs.gov.br/covid19/download"
request <- try(GET(url = path))

if(class(request) == "try-error") {
  dados_ses <- read_csv("bancos/covid/ses_reserva.csv")
} else if(request$status_code == 404){
  dados_ses <- read_csv("bancos/covid/ses_reserva.csv")
} else {
  dados_ses <- read_csv2("http://ti.saude.rs.gov.br/covid19/download", 
                        locale = readr::locale(encoding = "latin1"))
  write_csv(dados_ses,"bancos/covid/ses_reserva.csv")
}


names(dados_ses) <- c("codigo_ibge_6_digitos","municipio","codigo_regiao_covid","regiao_covid",
                      "sexo","faixa_etaria","tipo_teste",
                      "data_confirmacao","data_sintomas","data_evolucao","evolucao","hospitalizacao",
                      "sintoma_febre","sintoma_tosse",
                      "sintoma_garganta","sintoma_dispneia","sintomas_outros","comorbidades",
                      "data_inclusao_obito","data_evolucao_estimada","raca_cor", "nao_sei_o_que_e")

dados_covid_poa <- dados_ses %>%
  mutate(data_confirmacao = as_date(data_confirmacao, format = "%d/%m/%y"),
         data_sintomas = as_date(data_sintomas, format = "%d/%m/%y"),
         data_evolucao = as_date(data_evolucao, format = "%d/%m/%y"),
         municipio = str_to_title(municipio))

# arrumando os 3 municipios com inconssistências nos nomes

dados_covid_poa[dados_covid_poa$municipio=="Westfalia","municipio"] <- "Westfália"
dados_covid_poa[dados_covid_poa$municipio=="Vespasiano Correa","municipio"] <- "Vespasiano Corrêa"
dados_covid_poa[dados_covid_poa$municipio=="Santana Do Livramento","municipio"] <- "Sant'ana Do Livramento"

# pegando o código ibge de 7 dígitos pelo nome do municipio

dados_covid_poa <- dados_covid_poa %>%
  filter(municipio == "Porto Alegre") %>%
  mutate(populacao_estimada = 1483771) %>%
  select(-c(codigo_ibge_6_digitos,codigo_regiao_covid,regiao_covid))

dados_covid_poa <- dados_covid_poa %>%
  mutate(data_evolucao = ifelse(evolucao == "RECUPERADO", ifelse(is.na(data_evolucao),data_sintomas+days(14),data_evolucao),data_evolucao)) %>%
  mutate(data_evolucao = as_date(data_evolucao))

# adicionando semana epidemiologica

semana <- read_csv("bancos/semana_epidemio_dia.csv")

dados_covid_poa <- dados_covid_poa %>%
  left_join(semana, by = c("data_confirmacao" = "dia")) %>%
  mutate(semana_epidemiologica_confirmacao = semana_epidemiologica) %>%
  select(-semana_epidemiologica) %>%
  left_join(semana, by = c("data_sintomas" = "dia")) %>%
  mutate(semana_epidemiologica_sintomas = semana_epidemiologica) %>%
  select(-semana_epidemiologica) %>%
  left_join(semana, by = c("data_evolucao" = "dia")) %>%
  mutate(semana_epidemiologica_evolucao = semana_epidemiologica) %>%
  select(-semana_epidemiologica)



##############################################################################
# Dados dos leitos de poa

# base da prefeitura

# download dos dados vem em .zip

uti <- NULL

path <- "https://infografico-covid-api.procempa.com.br/infograficocovid/api/v1/download/saude"
request <- try(GET(url = path))

if(class(request) == "try-error") {
  uti <- read_csv("bancos/leitos/banco_uti_reserva.csv")
} else if(request$status_code == 404){
  uti <- read_csv("bancos/leitos/banco_uti_reserva.csv")
} else {
  temp <- tempfile()
  download.file("https://infografico-covid-api.procempa.com.br/infograficocovid/api/v1/download/saude", temp, mode = "wb")
  
  uti <- read_csv2(unz(temp, "leitos_uti.csv"), locale = readr::locale(encoding = "latin1"))
  
  unlink(temp)
  write_csv(uti,"bancos/leitos/banco_uti_reserva.csv")
}

# arrumando variáveis

names(uti) <- c("local","tipo","data_atualizacao","leitos_total","leitos_bloqueados","leitos_ocupados",
                "leitos_covid_suspeito","leitos_covid")

uti <- uti %>%
  mutate(data_atualizacao = as_date(data_atualizacao, format = "%d/%m/%Y"))

teste <- uti %>%
  filter(data_atualizacao == max(data_atualizacao))



# adicionando semana epidemiologica

semana_epidemio <- read_csv("bancos/semana_epidemio_dia.csv")

leitos <- bind_rows(leitos_antigos,leitos_novos) %>%
  left_join(semana_epidemio, by = c("data_atualizacao" = "dia"))





rm(list=setdiff(ls(),c("dados_covid_poa")))
