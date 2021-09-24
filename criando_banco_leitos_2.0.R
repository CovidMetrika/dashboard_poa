# Data wrangling

# pegando dados do estado sobre os leitos

# pegando último banco atualizado na nossa base

ultima_atualizacao <- read_csv("bancos/leitos/ultima_atualizacao2.csv")

# dados das semanas epidemiológicas

semana_20_21 <- read_csv("bancos/semana_20_21.csv")

# pegando novos dados CSV

new_data <- NULL
new_data <- try(read_csv2("https://secweb.procergs.com.br/isus-covid/api/v1/export/csv/hospitais"))

# pegando novos dados ZIP

# temp <- tempfile()
# download.file("https://secweb.procergs.com.br/isus-covid/api/v1/export/csv/hospitais",temp, mode = 'wb')
# aux <- unzip(temp, list = T)[1]
# new_data <- read_csv2(unz(temp, filename = aux$Name), locale = readr::locale(encoding = "latin1"))
# 
# unlink(temp)

if (sum(new_data$IBGE == "\n")!=0) {
  new_data <- NULL
  new_data <- ultima_atualizacao
} else {
  names(new_data)[1:30] <-  c("codigo_ibge_6_digitos", "municipio", "lat_mun", "lon_mun", "estado", "crs", "regiao_saude",
                              "codigo_macrorregiao_saude", "macrorregiao_saude", "codigo_regiao_covid",
                              "regiao_covid", "cnes", "hospital", "latitude", "longitude", "data_atualizacao",
                              "leitos_uti_adulto","leitos_clinicos_adulto","leitos_uti_pediatrico","respiradores", 
                              "uti_adulto_suspeitos_covid", "uti_adulto_confirmados_covid","uti_pediatrico_suspeitos_covid", "uti_pediatrico_confirmados_covid",
                              "clinico_adulto_suspeitos_covid", "clinico_adulto_confirmados_covid","clinico_pediatrico_suspeitos_covid", "clinico_pediatrico_confirmados_covid",
                              "uti_adulto_respiradores","uti_adulto_internacoes")
  
  # criando e manipulando variaveis 
  
  new_data <- new_data %>%
    filter(municipio == "Porto Alegre") %>%
    mutate(leitos_total = leitos_uti_adulto,
           leitos_internacoes = uti_adulto_internacoes,
           leitos_covid = uti_adulto_confirmados_covid,
           lotacao = ifelse(leitos_uti_adulto != 0,uti_adulto_internacoes/leitos_uti_adulto,NA),
           leitos_disponiveis = leitos_uti_adulto-uti_adulto_internacoes) %>%
    mutate(data_atualizacao = as_date(data_atualizacao)) 
  
  
  # preenchendo os dias vazios
  # descobri que tem uma função do pacote padr que da pra utilizar aqui
  # é usado para séries temporais e percebi que funcionaria aqui
  
  new_data <- new_data %>%
    group_by(cnes) %>%
    padr::pad(start_val = min(new_data$data_atualizacao), end_val = max(new_data$data_atualizacao)) %>%
    group_by(cnes) %>%
    fill(everything(),.direction = "updown") %>%# preecnhendo os missings conforme grupo
    left_join(semana_20_21, by = c("data_atualizacao" = "dia")) %>%
    ungroup() %>%
    select(names(ultima_atualizacao)) %>%
    mutate(codigo_regiao_covid = as.numeric(codigo_regiao_covid),
           latitude = as.numeric(str_c(str_extract(latitude, "^..."),".",str_remove(latitude, "^..."))),
           longitude = as.numeric(str_c(str_extract(longitude, "^..."),".",str_remove(longitude, "^...")))) 
  
  # verficando se há algum novo hospital 
  
  if(sum(!unique(new_data$cnes) %in% unique(ultima_atualizacao$cnes)) > 0) {
    novos_hospitais <- new_data[!new_data$cnes %in% unique(ultima_atualizacao$cnes),] %>%
      mutate(hospital = str_to_title(hospital))
    
    ultima_atualizacao <- ultima_atualizacao %>%
      add_case(novos_hospitais)
    
  } 
  
  
  new_data <- new_data %>%
    select(-hospital) %>%
    left_join(unique(ultima_atualizacao[,c("hospital","cnes")]), by = "cnes")
  
}



# retirando os dados que serão inseridos para não ocorrer duplicatas

ultima_atualizacao <- ultima_atualizacao %>%
  filter(!data_atualizacao %in% unique(new_data$data_atualizacao))

if(nrow(ultima_atualizacao) != 0) {
  leitos_uti <- new_data %>%
    add_case(ultima_atualizacao) %>%
    distinct(cnes,data_atualizacao,.keep_all = T)
} else {
  leitos_uti <- new_data %>%
    distinct(cnes,data_atualizacao,.keep_all = T)
}

write_csv(new_data,"bancos/leitos/ultima_atualizacao2.csv")


