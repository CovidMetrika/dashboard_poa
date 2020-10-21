# novo metodo de dados covid 
# pegando dados da SES-RS


# lendo dados da SES-RS

dados_ses <- NULL
dados_ses <- try(read_csv2("http://ti.saude.rs.gov.br/covid19/download"))


path <- "http://ti.saude.rs.gov.br/covid19/download"
request <- GET(url = path)

if(request$status_code == 404) {
  dados_ses <- read_csv("bancos/covid/ses_reserva.csv")
} else {
  write_csv(dados_ses,"bancos/covid/ses_reserva.csv")
}

names(dados_ses)[1:25] <- c("codigo_ibge_6_digitos","municipio","codigo_regiao_covid","regiao_covid",
                      "sexo","faixa_etaria","tipo_teste",
                      "data_confirmacao","data_sintomas","data_evolucao","evolucao","hospitalizacao",
                      "sintoma_febre","sintoma_tosse",
                      "sintoma_garganta","sintoma_dispneia","sintomas_outros","comorbidades","gestante",
                      "data_inclusao_obito","data_evolucao_estimada","raca_cor", "etnia_indigena","profissional_de_saude",
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

b <- tibble(shp = sort(unique(shp_poa$NOME)))

# View(a)
# View(b)

# pegando banco com vetor de labels dos bairros que vou trocar o nome para ter compatibilidade
# com o nome dos bairros no shapefile

labels_para_troca <- read_csv("bancos/vetor_nomes_bairros.csv")

# apenas rode os 3 comandos abaixos comentados caso queira atualizar os nomes dos bairros

#temp <- tibble(labels = levels(factor(dados_covid_poa$bairro)))
#
#write_csv(temp,"bancos/vetor_nomes_bairros.csv")

# labels_novo_2 <- c(rep(NA,7),rep("ABERTA DOS MORROS",9),NA,NA,rep("AGRONOMIA",2),NA,"ABERTA DOS MORROS","ABERTA DOS MORROS",NA,"MORRO SANTANA",NA,rep("MORRO SANTANA",4),
#                    rep("TERESÓPOLIS",2),"MORRO SANTANA",NA,NA,NA,"ANCHIETA",NA,NA,rep("CEL. APARICIO BORGES",2),rep(NA,4),rep("ARQUIPÉLAGO",4),NA,rep("VILA ASSUNÇÃO",3),
#                    "MORRO SANTANA","AUXILIADORA","AUXILIADORA",NA,NA,"AZENHA","AZENHA","JARDIM BOTÂNICO",NA,"RUBEM BERTA","BELA VISTA",rep(NA,2),"CAMAQUÃ","CASCATA",
#                    "CENTRO HISTÓRICO","VILA CONCEIÇÃO","CRISTAL",NA,NA,NA,"PARTENON","PETRÓPOLIS","SANTANA",rep(NA,10),rep("BELA VISTA",4),rep("BELÉM NOVO",2),
#                    rep("BELÉM VELHO",2),"BELÉM NOVO","BELA VISTA",NA,NA,"ABERTA DOS MORROS","JARDIM SÃO PEDRO","RUBEM BERTA",NA,"BOA VISTA","BOA VISTA DO SUL",
#                    rep("BOM FIM",3),rep("BOM JESUS",23),NA,rep("BOM FIM",2),rep(NA,4),"CRISTO REDENTOR",NA,NA,NA,NA,NA,"CIDADE BAIXA","CENTRO HISTÓRICO",rep("CAMAQUÃ",5),
#                    rep(NA,5),"CAMPO NOVO",NA,NA,"CASCATA","CASCATA",
#                    "CAVALHADA","CAVALHADA","CAVALHADA",NA,NA,"CEL. APARICIO BORGES","CEL. APARICIO BORGES","CEL. APARICIO BORGES",
#                    "CENTRO HISTÓRICO","CENTRO HISTÓRICO","CENTRO HISTÓRICO","CENTRO HISTÓRICO","CENTRO HISTÓRICO","CHÁCARA DAS PEDRAS",
#                    "MORRO SANTANA","CHÁCARA DAS PEDRAS","CHÁCARA DAS PEDRAS","CHÁCARA DAS PEDRAS",NA,"CHAPÉU DO SOL","CHAPÉU DO SOL",
#                    "CIDADE BAIXA","CIDADE BAIXA",NA,"CAVALHADA",NA,NA,"CEL. APARICIO BORGES","CEL. APARICIO BORGES","CEL. APARICIO BORGES",
#                    "CEL. APARICIO BORGES","COSTA E SILVA","CRISTAL","CRISTO REDENTOR",NA,NA,NA,"ESPÍRITO SANTO","ESPÍRITO SANTO","ESPÍRITO SANTO",
#                    NA,NA,"LOMBA DO PINHEIRO",
#                    "EXTREMA",NA,"FARRAPOS","FARROUPILHA","FARROUPILHA",NA,NA,"FLORESTA","FLORESTA",NA,"GLÓRIA","GLÓRIA",NA,"GUARUJÁ",
#                    "GUARUJÁ",NA,"HIGIENÓPOLIS","HIGIENÓPOLIS","HIGIENÓPOLIS","HIGIENÓPOLIS","HÍPICA","HÍPICA","HUMAITÁ","HUMAITÁ","PASSO DA AREIA",
#                    NA,"ARQUIPÉLAGO","INDEPENDÊNCIA","INDEPENDÊNCIA","PARTENON",NA,"IPANEMA",NA,NA,"JARDIM ITU","JARDIM FLORESTA",
#                    "JARDIM LEOPOLDINA",NA,NA,"JARDIM BOTÂNICO","JARDIM BOTÂNICO","JARDIM CARVALHO","CASCATA",NA,
#                    "JARDIM CARVALHO","JARDIM DO SALSO","JARDIM DO SALSO","JARDIM LEOPOLDINA","JARDIM EUROPA","JARDIM FLORESTA",
#                    NA,NA,"JARDIM ISABEL",NA,"JARDIM ITU","JARDIM SABARÁ","JARDIM SABARÁ","JARDIM SABARÁ","JARDIM SABARÁ",
#                    "JARDIM SABARÁ","JARDIM LEOPOLDINA","JARDIM LINDÓIA","JARDIM LINDÓIA",NA,NA,NA,NA,"JARDIM SÃO PEDRO","JARDIM SABARÁ","JARDIM SABARÁ","JARDIM SÃO PEDRO",
#                    "JARDIM SÃO PEDRO",NA,NA,"VILA NOVA",NA,"JARDIM LEOPOLDINA","JARDIM BOTÂNICO","JARDIM CARVALHO","CASCATA",NA,NA,"JARDIM ITU",
#                    "JARDIM ITU","JARDIM LEOPOLDINA",
#                    NA,"JARDIM SABARÁ","JARDIM DO SALSO","JARDIM SÃO PEDRO",NA,NA,"JARDIM LEOPOLDINA","JARDIM LINDÓIA","JARDIM CARVALHO","LAGEADO",
#                    "LAGEADO","LAMI","JARDIM LEOPOLDINA","JARDIM LINDÓIA","LOMBA DO PINHEIRO","LOMBA DO PINHEIRO",NA,"MENINO DEUS","MENINO DEUS",
#                    NA,NA,NA,"MÁRIO QUINTANA","MÁRIO QUINTANA",NA,"MEDIANEIRA","MENINO DEUS",NA,"MOINHOS DE VENTO","MOINHOS DE VENTO",
#                    "MOINHOS DE VENTO","MONTSERRAT","MONTSERRAT","MONTSERRAT",NA,"MONTSERRAT","MONTSERRAT","HÍPICA",NA,NA,"SANTA TEREZA",
#                    "MORRO SANTANA","SANTA TEREZA","SANTA TEREZA",NA,NA,"NAVEGANTES",NA,"INDEPENDÊNCIA","NONOAI","NONOAI","NONOAI",NA,NA,NA,"CENTRO HISTÓRICO",NA,
#                    "RUBEM BERTA","RUBEM BERTA",NA,"SÃO SEBASTIÃO","PARQUE SANTA FÉ","PARTENON","PARTENON","PASSO DA AREIA",
#                    "PASSO DA AREIA","PASSO DA AREIA","PASSO DA AREIA",NA,"PASSO DAS PEDRAS","PASSO DA AREIA","PASSO DAS PEDRAS","PASSO DAS PEDRAS","PASSO DAS PEDRAS",
#                    "PASSO DAS PEDRAS","PASSO DA AREIA","PASSO DA AREIA","PEDRA REDONDA","PETRÓPOLIS","PETRÓPOLIS","PETRÓPOLIS",NA,"MEDIANEIRA","PONTA GROSSA",
#                    NA,"RUBEM BERTA","SÃO SEBASTIÃO","PRAIA DE BELAS",NA,"MORRO SANTANA","MORRO SANTANA","MÁRIO QUINTANA","RUBEM BERTA",
#                    "RIO BRANCO",NA,"RESTINGA","RESTINGA","RESTINGA","RESTINGA","RIO BRANCO",NA,"RUBEM BERTA","RUBEM BERTA","RUBEM BERTA",NA,"SANTA CECÍLIA",
#                    "SANTA CECÍLIA",NA,"SANTA MARIA GORETTI","SANTA MARIA GORETTI",NA,"SANTA ROSA DE LIMA","SANTA ROSA DE LIMA","SANTA TEREZA",
#                    "SANTA TEREZA","SANTA TEREZA","SANTA TEREZA","SANTANA","SANTO ANTÔNIO","SANTO ANTÔNIO",NA,NA,"SÃO CAETANO","SÃO CAETANO",
#                    "SÃO GERALDO","SÃO GERALDO","SÃO JOÃO","SÃO JOÃO","VILA SÃO JOSÉ","VILA SÃO JOSÉ",NA,NA,NA,NA,"SÃO SEBASTIÃO","SÃO SEBASTIÃO",
#                    "SÃO SEBASTIÃO","SARANDI",NA,NA,NA,"SERRARIA",NA,"SANTA CECÍLIA","SANTA ROSA DE LIMA","SANTA TEREZA","SANTA TEREZA",
#                    NA,"SANTO ANTÔNIO",NA,"TERESÓPOLIS","TERESÓPOLIS","TERESÓPOLIS",NA,"TRÊS FIGUEIRAS",
#                    "TRÊS FIGUEIRAS","TRISTEZA","TRISTEZA","VILA NOVA",NA,"VILA ASSUNÇÃO","VILA ASSUNÇÃO","VILA ASSUNÇÃO",NA,"BOM JESUS",NA,NA,"VILA CONCEIÇÃO",
#                    NA,"FARRAPOS",NA,NA,"VILA IPIRANGA","VILA JARDIM","VILA JOÃO PESSOA","VILA JOÃO PESSOA",NA,NA,"VILA NOVA",NA,NA,NA,
#                    "VILA SÃO JOSÉ","VILA SÃO JOSÉ","VILA IPIRANGA","VILA ASSUNÇÃO",NA,"VILA JARDIM","VILA JOÃO PESSOA","VILA NOVA","VILA JARDIM",NA)

labels_novo <- c(NA,"ABERTA DOS MORROS","ABERTA DOS MORROS","ABERTA DOS MORROS","AGRONOMIA","MORRO SANTANA","MORRO SANTANA","MORRO SANTANA",
                 "TERESÓPOLIS","MORRO SANTANA",NA,NA,"ANCHIETA",NA,"CEL. APARICIO BORGES",NA,"ARQUIPÉLAGO",NA,"VILA ASSUNÇÃO","VILA ASSUNÇÃO",
                 "VILA ASSUNÇÃO","AUXILIADORA","AUXILIADORA","AZENHA","AZENHA","BELA VISTA",NA,NA,NA,"BELA VISTA","BELÉM NOVO","BELÉM NOVO",
                 "BELÉM VELHO","BELÉM VELHO",NA,NA,"BOA VISTA","BOA VISTA DO SUL","BOM FIM","BOM JESUS","BOM JESUS","BOM JESUS","BOM JESUS",
                 "BOM JESUS","BOM FIM",NA,NA,NA,"CIDADE BAIXA","CENTRO HISTÓRICO","CAMAQUÃ","CAMAQUÃ","CAMPO NOVO",NA,"CASCATA","CASCATA",
                 "CAVALHADA","CAVALHADA","CAVALHADA",NA,NA,"CEL. APARICIO BORGES","CEL. APARICIO BORGES","CEL. APARICIO BORGES",
                 "CENTRO HISTÓRICO","CENTRO HISTÓRICO","CENTRO HISTÓRICO","CENTRO HISTÓRICO","CENTRO HISTÓRICO","CHÁCARA DAS PEDRAS",
                 "MORRO SANTANA","CHÁCARA DAS PEDRAS","CHÁCARA DAS PEDRAS","CHÁCARA DAS PEDRAS",NA,"CHAPÉU DO SOL","CHAPÉU DO SOL",
                 "CIDADE BAIXA","CIDADE BAIXA",NA,"CAVALHADA",NA,NA,"CEL. APARICIO BORGES","CEL. APARICIO BORGES","CEL. APARICIO BORGES",
                 "CEL. APARICIO BORGES","COSTA E SILVA","CRISTAL","CRISTO REDENTOR",NA,NA,NA,"ESPÍRITO SANTO","ESPÍRITO SANTO","ESPÍRITO SANTO",
                 NA,NA,"LOMBA DO PINHEIRO",
                 "EXTREMA",NA,"FARRAPOS","FARROUPILHA","FARROUPILHA",NA,NA,"FLORESTA","FLORESTA",NA,"GLÓRIA","GLÓRIA",NA,"GUARUJÁ",
                 "GUARUJÁ",NA,"HIGIENÓPOLIS","HIGIENÓPOLIS","HIGIENÓPOLIS","HIGIENÓPOLIS","HÍPICA","HÍPICA","HUMAITÁ","HUMAITÁ","PASSO DA AREIA",
                 NA,"ARQUIPÉLAGO","INDEPENDÊNCIA","INDEPENDÊNCIA","PARTENON",NA,"IPANEMA",NA,NA,"JARDIM ITU","JARDIM FLORESTA",
                 "JARDIM LEOPOLDINA",NA,NA,"JARDIM BOTÂNICO","JARDIM BOTÂNICO","JARDIM CARVALHO","CASCATA",NA,
                 "JARDIM CARVALHO","JARDIM DO SALSO","JARDIM DO SALSO","JARDIM LEOPOLDINA","JARDIM EUROPA","JARDIM FLORESTA",
                 NA,NA,"JARDIM ISABEL",NA,"JARDIM ITU","JARDIM SABARÁ","JARDIM SABARÁ","JARDIM SABARÁ","JARDIM SABARÁ",
                 "JARDIM SABARÁ","JARDIM LEOPOLDINA","JARDIM LINDÓIA","JARDIM LINDÓIA",NA,NA,NA,NA,"JARDIM SÃO PEDRO","JARDIM SABARÁ","JARDIM SABARÁ","JARDIM SÃO PEDRO",
                 "JARDIM SÃO PEDRO",NA,NA,"VILA NOVA",NA,"JARDIM LEOPOLDINA","JARDIM BOTÂNICO","JARDIM CARVALHO","CASCATA",NA,NA,"JARDIM ITU",
                 "JARDIM ITU","JARDIM LEOPOLDINA",
                 NA,"JARDIM SABARÁ","JARDIM DO SALSO","JARDIM SÃO PEDRO",NA,NA,"JARDIM LEOPOLDINA","JARDIM LINDÓIA","JARDIM CARVALHO","LAGEADO",
                 "LAGEADO","LAMI","JARDIM LEOPOLDINA","JARDIM LINDÓIA","LOMBA DO PINHEIRO","LOMBA DO PINHEIRO",NA,"MENINO DEUS","MENINO DEUS",
                 NA,NA,NA,"MÁRIO QUINTANA","MÁRIO QUINTANA",NA,"MEDIANEIRA","MENINO DEUS",NA,"MOINHOS DE VENTO","MOINHOS DE VENTO",
                 "MOINHOS DE VENTO","MONTSERRAT","MONTSERRAT","MONTSERRAT",NA,"MONTSERRAT","MONTSERRAT","HÍPICA",NA,NA,"SANTA TEREZA",
                 "MORRO SANTANA","SANTA TEREZA","SANTA TEREZA",NA,NA,"NAVEGANTES",NA,"INDEPENDÊNCIA","NONOAI","NONOAI","NONOAI",NA,NA,NA,"CENTRO HISTÓRICO",NA,
                 "RUBEM BERTA","RUBEM BERTA",NA,"SÃO SEBASTIÃO","PARQUE SANTA FÉ","PARTENON","PARTENON","PASSO DA AREIA",
                 "PASSO DA AREIA","PASSO DA AREIA","PASSO DA AREIA",NA,"PASSO DAS PEDRAS","PASSO DA AREIA","PASSO DAS PEDRAS","PASSO DAS PEDRAS","PASSO DAS PEDRAS",
                 "PASSO DAS PEDRAS","PASSO DA AREIA","PASSO DA AREIA","PEDRA REDONDA","PETRÓPOLIS","PETRÓPOLIS","PETRÓPOLIS",NA,"MEDIANEIRA","PONTA GROSSA",
                 NA,"RUBEM BERTA","SÃO SEBASTIÃO","PRAIA DE BELAS",NA,"MORRO SANTANA","MORRO SANTANA","MÁRIO QUINTANA","RUBEM BERTA",
                 "RIO BRANCO",NA,"RESTINGA","RESTINGA","RESTINGA","RESTINGA","RIO BRANCO",NA,"RUBEM BERTA","RUBEM BERTA","RUBEM BERTA",NA,"SANTA CECÍLIA",
                 "SANTA CECÍLIA",NA,"SANTA MARIA GORETTI","SANTA MARIA GORETTI",NA,"SANTA ROSA DE LIMA","SANTA ROSA DE LIMA","SANTA TEREZA",
                 "SANTA TEREZA","SANTA TEREZA","SANTA TEREZA","SANTANA","SANTO ANTÔNIO","SANTO ANTÔNIO",NA,NA,"SÃO CAETANO","SÃO CAETANO",
                 "SÃO GERALDO","SÃO GERALDO","SÃO JOÃO","SÃO JOÃO","VILA SÃO JOSÉ","VILA SÃO JOSÉ",NA,NA,NA,NA,"SÃO SEBASTIÃO","SÃO SEBASTIÃO",
                 "SÃO SEBASTIÃO","SARANDI",NA,NA,NA,"SERRARIA",NA,"SANTA CECÍLIA","SANTA ROSA DE LIMA","SANTA TEREZA","SANTA TEREZA",
                 NA,"SANTO ANTÔNIO",NA,"TERESÓPOLIS","TERESÓPOLIS","TERESÓPOLIS",NA,"TRÊS FIGUEIRAS",
                 "TRÊS FIGUEIRAS","TRISTEZA","TRISTEZA","VILA NOVA",NA,"VILA ASSUNÇÃO","VILA ASSUNÇÃO","VILA ASSUNÇÃO",NA,"BOM JESUS",NA,NA,"VILA CONCEIÇÃO",
                 NA,"FARRAPOS",NA,NA,"VILA IPIRANGA","VILA JARDIM","VILA JOÃO PESSOA","VILA JOÃO PESSOA",NA,NA,"VILA NOVA",NA,NA,NA,
                 "VILA SÃO JOSÉ","VILA SÃO JOSÉ","VILA IPIRANGA","VILA ASSUNÇÃO",NA,"VILA JARDIM","VILA JOÃO PESSOA","VILA NOVA","VILA JARDIM",NA)

# View(cbind(labels_para_troca$labels,labels_novo))

# verificano quantidade de cada NA

temp2 <- tibble(antigo = labels_para_troca$labels,novo = labels_novo) %>%
  filter(is.na(novo))

dados_covid_poa %>%
  filter(bairro %in% temp2$antigo) %>%
  group_by(bairro) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

dados_covid_poa <- dados_covid_poa %>%
  mutate(
    bairro = plyr::mapvalues(
      bairro,
      from = labels_para_troca$labels,
      to = labels_novo))

# ultima verificacao para ver se todos labels novos estão no shapefile

sort(unique(dados_covid_poa$bairro))[!sort(unique(dados_covid_poa$bairro)) %in% b$shp]



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


