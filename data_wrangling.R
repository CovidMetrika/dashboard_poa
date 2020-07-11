# Data wrangling

# novo metodo de dados covid 
# pegando dados da SES-RS


# lendo dados da SES-RS

dados_ses <- NULL
dados_ses <- try(read_csv2("http://ti.saude.rs.gov.br/covid19/download", 
                           locale = readr::locale(encoding = "latin1")))


path <- "http://ti.saude.rs.gov.br/covid19/download"
request <- GET(url = path)

if(request$status_code == 404) {
  dados_ses <- read_csv("bancos/covid/ses_reserva.csv")
} else {
  write_csv(dados_ses,"bancos/covid/ses_reserva.csv")
}

names(dados_ses)[1:23] <- c("codigo_ibge_6_digitos","municipio","codigo_regiao_covid","regiao_covid",
                            "sexo","faixa_etaria","tipo_teste",
                            "data_confirmacao","data_sintomas","data_evolucao","evolucao","hospitalizacao",
                            "sintoma_febre","sintoma_tosse",
                            "sintoma_garganta","sintoma_dispneia","sintomas_outros","comorbidades",
                            "data_inclusao_obito","data_evolucao_estimada","raca_cor", "profissional_de_saude",
                            "bairro")

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
  mutate(data_evolucao = ifelse(evolucao == "RECUPERADO", ifelse(hospitalizacao == "NAO", ifelse(is.na(data_evolucao),ifelse(data_sintomas+days(14)<Sys.Date(),data_sintomas+days(14),NA),data_evolucao),data_evolucao),data_evolucao)) %>%
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



# arrumando a variável de bairro

# pegando shapefile com os bairro de poa da prefeitura

shp_poa <- sf::st_read("bancos/shapefile_bairros_poa/Bairros_LC12112_16.shp", quiet = TRUE) %>%
  mutate(NOME = as.character(NOME)) %>%
  mutate(NOME = ifelse(NOME == "VILA  ASSUNÇÃO","VILA ASSUNÇÃO", NOME))

# para visualizar os bairros melhor

a <- tibble(estado = sort(unique(dados_covid_poa$bairro)))

b <- tibble(shp = levels(shp_poa$NOME))

# View(a)
# View(b)

# pegando banco com vetor de labels dos bairros que vou trocar o nome para ter compatibilidade
# com o nome dos bairros no shapefile

labels_para_troca <- read_csv("bancos/vetor_nomes_bairros.csv")

# apenas rode os 2 comandos abaixos comentados caso queira atualizar os nomes dos bairros

# temp <- tibble(labels = levels(factor(dados_covid_poa$bairro)))
# 
# write_csv(temp,"bancos/vetor_nomes_bairros.csv")

labels_novo <- c("ABERTA DOS MORROS","ABERTA DOS MORROS","AGRONOMIA","MORRO SANTANA","MORRO SANTANA","MORRO SANTANA",
                 "TERESÓPOLIS","MORRO SANTANA",NA,"ANCHIETA",NA,"CEL. APARICIO BORGES","ARQUIPÉLAGO","VILA ASSUNÇÃO",
                 "VILA ASSUNÇÃO","AUXILIADORA","AZENHA","BELA VISTA","BELÉM NOVO","BELÉM NOVO","BELÉM VELHO",NA,NA,
                 "BOA VISTA","BOA VISTA DO SUL","BOM FIM","BOM JESUS","BOM FIM",NA,NA,NA,"CIDADE BAIXA","CENTRO HISTÓRICO",
                 "CAMAQUÃ","CAMAQUÃ","CAMPO NOVO",NA,"CASCATA","CASCATA","CAVALHADA","CAVALHADA","CEL. APARICIO BORGES",
                 "CEL. APARICIO BORGES","CENTRO HISTÓRICO","CENTRO HISTÓRICO","CENTRO HISTÓRICO","CENTRO HISTÓRICO","CENTRO HISTÓRICO",
                 "CHÁCARA DAS PEDRAS","CHÁCARA DAS PEDRAS","CHÁCARA DAS PEDRAS","CHÁCARA DAS PEDRAS","CHAPÉU DO SOL",
                 "CIDADE BAIXA","CIDADE BAIXA","CAVALHADA",NA,"CEL. APARICIO BORGES","CEL. APARICIO BORGES","CEL. APARICIO BORGES",
                 "CEL. APARICIO BORGES","COSTA E SILVA","CRISTAL","CRISTO REDENTOR",NA,"ESPÍRITO SANTO",NA,"LOMBA DO PINHEIRO",
                 "EXTREMA",NA,"FARRAPOS","FARROUPILHA","FARROUPILHA",NA,"FLORESTA","FLORESTA","GLÓRIA","GLÓRIA","GUARUJÁ",
                 "GUARUJÁ","HIGIENÓPOLIS","HIGIENÓPOLIS","HIGIENÓPOLIS","HIGIENÓPOLIS","HÍPICA","HÍPICA","HUMAITÁ","HUMAITÁ",
                 NA,"ARQUIPÉLAGO","INDEPENDÊNCIA","INDEPENDÊNCIA","PARTENON","IPANEMA","JARDIM ITU","JARDIM FLORESTA",
                 "JARDIM LEOPOLDINA","JARDIM BOTÂNICO","JARDIM BOTÂNICO","JARDIM CARVALHO","JARDIM CARVALHO","CASCATA",
                 "JARDIM CARVALHO","JARDIM DO SALSO","JARDIM DO SALSO","JARDIM LEOPOLDINA","JARDIM EUROPA","JARDIM FLORESTA",
                 NA,NA,"JARDIM ISABEL","JARDIM ITU","JARDIM SABARÁ","JARDIM SABARÁ","JARDIM SABARÁ","JARDIM SABARÁ",
                 "JARDIM SABARÁ","JARDIM LEOPOLDINA","JARDIM LINDÓIA","JARDIM LINDÓIA",NA,NA,"JARDIM SABARÁ","JARDIM SÃO PEDRO",
                 "JARDIM SÃO PEDRO",NA,"JARDIM BOTÂNICO","JARDIM CARVALHO","CASCATA","JARDIM ITU","JARDIM ITU","JARDIM LEOPOLDINA",
                 NA,"JARDIM SABARÁ","JARDIM DO SALSO","JARDIM SÃO PEDRO","LAGEADO","LAMI","LOMBA DO PINHEIRO",NA,"MENINO DEUS",
                 NA,NA,"MÁRIO QUINTANA","MÁRIO QUINTANA",NA,"MEDIANEIRA","MENINO DEUS","MOINHOS DE VENTO","MOINHOS DE VENTO",
                 "MOINHOS DE VENTO","MONTSERRAT","MONTSERRAT","MONTSERRAT","MONTSERRAT","MONTSERRAT","SANTA TEREZA",
                 "MORRO SANTANA",NA,NA,"NAVEGANTES","INDEPENDÊNCIA","NONOAI","NONOAI",NA,NA,"CENTRO HISTÓRICO",NA,
                 "RUBEM BERTA","RUBEM BERTA","SÃO SEBASTIÃO","PARQUE SANTA FÉ","SÃO SEBASTIÃO","PARTENON","PASSO DA AREIA",
                 "PASSO DA AREIA","PASSO DA AREIA",NA,"PASSO DA AREIA","PASSO DAS PEDRAS","PASSO DAS PEDRAS","PASSO DAS PEDRAS",
                 "PASSO DAS PEDRAS",NA,"PEDRA REDONDA","PETRÓPOLIS","PETRÓPOLIS","PETRÓPOLIS","MEDIANEIRA","PONTA GROSSA",
                 NA,"RUBEM BERTA","SÃO SEBASTIÃO","PRAIA DE BELAS","MORRO SANTANA","MORRO SANTANA","MÁRIO QUINTANA","RUBEM BERTA",
                 "RIO BRANCO","RESTINGA","RESTINGA","RESTINGA","RIO BRANCO","RUBEM BERTA","RUBEM BERTA","RUBEM BERTA","SANTA CECÍLIA",
                 "SANTA MARIA GORETTI","SANTA MARIA GORETTI","SANTA ROSA DE LIMA","SANTA ROSA DE LIMA","SANTA TEREZA",
                 "SANTA TEREZA","SANTA TEREZA","SANTA TEREZA","SANTANA","SANTO ANTÔNIO","SANTO ANTÔNIO",NA,"SÃO CAETANO",
                 "SÃO GERALDO","SÃO GERALDO","SÃO JOÃO","SÃO JOÃO","VILA SÃO JOSÉ","VILA SÃO JOSÉ",NA,NA,"SÃO SEBASTIÃO",
                 "SÃO SEBASTIÃO","SARANDI",NA,NA,"SERRARIA",NA,"SANTA ROSA DE LIMA","SANTA TEREZA","SANTA TEREZA",
                 NA,"SANTO ANTÔNIO",NA,"TERESÓPOLIS","TERESÓPOLIS","TERESÓPOLIS","TERESÓPOLIS",NA,"TRÊS FIGUEIRAS",
                 "TRÊS FIGUEIRAS","TRISTEZA","TRISTEZA","VILA NOVA",NA,"VILA ASSUNÇÃO","VILA ASSUNÇÃO",NA,NA,"VILA CONCEIÇÃO",
                 "FARRAPOS","VILA IPIRANGA","VILA JARDIM","VILA JOÃO PESSOA","VILA JOÃO PESSOA",NA,"VILA NOVA",NA,
                 "VILA SÃO JOSÉ","VILA IPIRANGA","VILA ASSUNÇÃO","VILA JARDIM","VILA JOÃO PESSOA",NA)

# View(cbind(labels_para_troca$labels,labels_novo))

dados_covid_poa <- dados_covid_poa %>%
  mutate(
    bairro = plyr::mapvalues(
      bairro, 
      from = labels_para_troca$labels,
      to = labels_novo))

# pegando dados da população de cada bairro

pop_bairros <- read_csv("bancos/populacao_bairros.csv") %>%
  mutate(populacao_bairro = 1483771/1409351*populacao) %>% # adicionando um multiplicador de correção dos dados de 2010 para a estimativa de 2019
  select(-populacao)

dados_covid_poa <- dados_covid_poa %>%
  left_join(pop_bairros, by = c("bairro")) 


# banco para o join com shp

dados_covid_poa_join <- dados_covid_poa %>%
  mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
         acompanhamento = ifelse(evolucao == "EM ACOMPANHAMENTO", 1, 0),
         recuperados = ifelse(evolucao == "RECUPERADO", 1, 0)) %>% 
  filter(!is.na(bairro)) %>%
  group_by(bairro) %>%
  summarise(confirmados = n(), confirmados_taxa = n()*100000/first(populacao_bairro),
            obitos = sum(obitos, na.rm = T), obitos_taxa = sum(obitos, na.rm = T)*100000/first(populacao_bairro), 
            acompanhamento = sum(acompanhamento, na.rm = T), acompanhamento_taxa  = sum(acompanhamento, na.rm = T)*100000/first(populacao_bairro),
            recuperados = sum(recuperados, na.rm = T), recuperados_taxa = sum(recuperados, na.rm = T)*100000/first(populacao_bairro)) %>%
  ungroup()

mapa_poa <- shp_poa %>%
  left_join(dados_covid_poa_join, by = c("NOME" = "bairro"))

mapa_poa <- sf::st_transform(mapa_poa, "+proj=longlat +datum=WGS84")



rm(list=setdiff(ls(),c("dados_covid_poa","mapa_poa")))


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
