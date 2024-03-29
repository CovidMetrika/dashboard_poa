library(tidyverse)
library(pdftools)

### criando o banco

#######################################
## para atualizar com novos dados!!! ##
#######################################

# mudar objeto primeira(primeira página do pdf q iniciam os dados), 
# caminho do pdf, adionar novos locais com novas latitudes e longitudes
# verificar se os labels conferem porque cada vez descobrem um jeito novo
# de dizer que é o clínicas ou o são lucas

######################################

# dados poa

# lendo o pdf, separando as linhas, e tirando os whitespaces

raw_pdf <- pdf_text("bancos/pdf/2020_05_11_boletim_covid_sms_50.pdf") %>%
  map(strsplit, split="\r\n") %>%
  map(map, str_squish)

# pegando só as páginas q tem os dados msm

primeira <- 17 # selecionar página do pdf q inicia o banco

raw_pdf <- raw_pdf[primeira:length(raw_pdf)]

raw_pdf[[1]][[1]]  # verificar quantas linhas são preciso retirar do inicio (ultima vez foi: 3) 

raw_pdf[[1]][[1]] <- raw_pdf[[1]][[1]][-c(1:3)]

raw_pdf[[length(raw_pdf)]][[1]] # verificar quantas linhas do fim serão preciso retirar(lembrando que a linha que contém o número da página
                                # será retirada depois na função de leitura). última vez foram: 2 linhas retiradas

ultimas <- c((length(raw_pdf[[length(raw_pdf)]][[1]])-1):length(raw_pdf[[length(raw_pdf)]][[1]]))

raw_pdf[[length(raw_pdf)]][[1]] <- raw_pdf[[length(raw_pdf)]][[1]][-c(ultimas)] # tirando as linhas finais

raw_pdf[[1]][[1]]

leitura <- function(x){

  x <- x[-length(x)] # sempre verificar se é preciso esse código(muitas vzs n é pra rodar)
  
  dados <- tibble(.rows = length(x))
  
  dados$id <- ifelse(test = str_detect(x,"^\\d"),
                     yes = str_extract(x,"^\\d+"),
                     NA)
  
  dados$data <- ifelse(test = str_detect(x,"^\\d"),
                       yes = str_extract(str_trim(str_remove(x,"^\\d+")),"^.*?\\s"),
                       no = NA)
  
  dados$sexo <- ifelse(test = str_detect(x,"^\\d"),
                       yes = str_extract(str_trim(str_remove(str_trim(str_remove(x,"^...")),"^.*?\\s")),"^."),
                       no = NA)
  
  dados$idade <- ifelse(test = str_detect(x,"^\\d"),
                        yes = str_extract(str_trim(str_remove(str_trim(str_remove(x,"^...")),"^.*?\\s")),"\\d+"),
                        no = NA)
  
  dados$fonte <- ifelse(test = str_detect(x,"^\\d"),
                        yes = str_trim(str_remove(str_trim(str_remove(str_trim(str_remove(str_trim(str_remove(x,"^...")),"^.*?\\s")),"^.")), "^.*?\\s")),
                        no = x)
  
  return(dados)
}

# lendo os dados

dados <- raw_pdf %>%
  map(map, leitura) %>%
  unlist(recursive = FALSE) %>%
  bind_rows()

# arrumando casos em que os dados estavam em duas linhas

for (i in 1:length(dados$id)) {
  if(is.na(dados$id[i])){
    dados$fonte[i+1] <- str_c(dados$fonte[i], " ", dados$fonte[i+1])
  }
}

dados <- dados %>%
  filter(!is.na(id)) %>%
  mutate(fonte = ifelse(fonte=="NI", NA, fonte)) %>%
  mutate(sexo = ifelse((sexo=="F"|sexo=="M"),sexo, NA)) %>%
  mutate(fonte = ifelse(fonte == "RS", "COE RS", fonte)) %>%
  mutate(fonte = ifelse(fonte == "Secretaria Municipal de Saude de Porto legre", "Secretaria Municipal de Saude de Porto Alegre", fonte)) %>%
  mutate(fonte = ifelse(fonte == "de Saude Morro Santana", "Unidade de Saude Morro Santana", fonte)) %>%
  mutate(fonte = ifelse(fonte == "PORTO ALEGRE", "SMS PORTO ALEGRE", fonte)) %>%
  mutate(fonte = ifelse(fonte == "29 ASSOCIACAO HOSPITALAR VILA NOVA", "ASSOCIACAO HOSPITALAR VILA NOVA", fonte)) %>%
  mutate(fonte = ifelse(fonte == "30", NA, fonte)) %>%
  mutate(fonte = ifelse(fonte == "36", NA, fonte))



# renomeando os valores das fontes

lbl <- levels(as.factor(dados$fonte))
lvl <- c("unidade_de_saude_iapi","hospital_vila_nova","unidade_de_saude_iapi","centro_de_operacoes_de_emergencias_rs","consultorio_privado","equipe_de_vigilância_de_doencas_transmissiveis","laboratorio_endocrimeta","equipe_de_vigilância_de_doencas_transmissiveis","farmacia_sao_joao","laboratorio_fleury","hospital_conceicao",
         "hospital_da_brigada_militar","hospital_de_clinicas_de_porto_alegre","hospital_de_clinicas_de_porto_alegre","hospital_de_clinicas_de_porto_alegre","hospital_divina_providencia","hospital_divina_providencia",
         "hospital_ernesto_dornelles","hospital_geral_de_porto_alegre","hospital_geral_de_porto_alegre","hospital_mae_de_deus","hospital_mae_de_deus","hospital_moinhos_de_vento",
         "hospital_conceicao","hospital_conceicao","hospital_conceicao","hospital_porto_alegre","hospital_restinga_extremo_sul","hospital_sao_lucas","hospital_sao_lucas","hospital_sao_lucas",
         "hospital_univesitario_de_pelotas","hospital_univesitario_de_pelotas","hospital_vila_nova","instituto_de_cardiologia","hospital_santa_casa","hospital_santa_casa","hospital_santa_casa",
         "laboratorio_endocrimeta","laboratorio_analysis","laboratorio_endocrimeta","laboratorio_exame","laboratorio_felippe","laboratorio_felippe","laboratorio_fleury","laboratorio_fleury","laboratorio_jeffman","laboratorio_montserrat","laboratorio_montserrat","laboratorio_pardini","unimed","laboratorio_weinmann","laboratorio_zanol","laboratorio_fleury","hospital_univesitario_de_pelotas","pronto_atendimento_cruzeiro_do_sul","pronto_atendimento_lomba_do_pinheiro","secretaria_municipal_de_saude_porto_alegre","secretaria_municipal_de_saude_porto_alegre",
         "secretaria_municipal_da_saude_de_sao_leopoldo","hospital_univesitario_de_pelotas","unidade_de_saude_iapi","unidade_de_saude_jardim_itu","unidade_de_saude_jardim_leopoldina","unidade_de_saude_modelo",
         "unidade_de_saude_morro_santana","unidade_de_saude_nova_brasilia","unidade_de_saude_panorama","unidade_de_saude_passo_das_pedras","unidade_de_saude_santa_maria","unidade_de_saude_sesc","unidade_de_saude_tristeza","unidade_de_saude_tristeza","unimed","hospital_univesitario_de_pelotas","unidade_de_pronto_atendimento_zona_norte","unidade_de_saude_torres", "unidade_de_saude_guaruja","unidade_de_saude_iapi",
         "unidade_de_saude_navegantes","unidade_de_saude_nonoai", "unidade_de_saude_santa_cecilia","vigilicancia_em_saude_sms_novo_hamburgo","laboratorio_weinmann")

View(cbind(lbl,lvl)) # verificar os labels que não batem e renomeá-los

dados$fonte <- plyr::mapvalues(dados$fonte, from = lbl, to = lvl)

# adicionando as coordenadas geográficas(se tiver um novo local, adicionar ao final 
# do objeito locais o novo nome, e ao final da latitude e longitude as suas coordenadas

levels(as.factor(dados$fonte))

locais <- c("centro_de_operacoes_de_emergencias_rs","consultorio_privado","equipe_de_vigilância_de_doencas_transmissiveis",
            "hospital_conceicao","hospital_de_clinicas_de_porto_alegre","hospital_divina_providencia","hospital_ernesto_dornelles",
            "hospital_mae_de_deus","hospital_moinhos_de_vento","hospital_restinga_extremo_sul","hospital_santa_casa","hospital_sao_lucas","paciente",
            "pronto_atendimento_cruzeiro_do_sul", "secretaria_municipal_da_saude_de_sao_leopoldo","unidade_de_pronto_atendimento_zona_norte",
            "unidade_de_saude_guaruja", "unidade_de_saude_iapi","unidade_de_saude_modelo","unidade_de_saude_nonoai",
            "unidade_de_saude_santa_cecilia", "unidade_de_saude_torres","unimed","hospital_cristo_redentor","hospital_de_pronto_socorro",
            "hospital_femina","hospital_independencia","hospital_materno_infantil_presidente_vargas","hospital_santa_ana",
            "hospital_vila_nova","instituto_de_cardiologia","pronto_atendimento_bom_jesus","pronto_atendimento_lomba_do_pinheiro",
            "secretaria_municipal_de_saude_porto_alegre","unidade_de_saude_sesc","unidade_de_saude_jardim_itu","unidade_de_saude_morro_santana",
            "hospital_univesitario_de_pelotas","laboratorio_analysis","hospital_da_brigada_militar","laboratorio_endocrimeta",
            "vigilicancia_em_saude_sms_novo_hamburgo","unidade_de_saude_santa_maria","unidade_de_saude_tristeza","unidade_de_saude_navegantes",
            "hospital_geral_de_porto_alegre","associacao_hospitalar_vila_nova","hospital_santa_cruz","laboratorio_pardini",
            "laboratorio_fleury","unidade_de_saude_nova_brasilia","hospital_porto_alegre","csvs","laboratorio_weinmann","laboratorio_felippe",
            "laboratorio_jeffman","unidade_de_saude_panorama","unidade_de_saude_passo_das_pedras","laboratorio_exame",
            "laboratorio_montserrat","farmacia_sao_joao","unidade_de_saude_jardim_leopoldina","laboratorio_zanol")


latitude <- c(-30.030921,NA,-30.062808,-30.015870,-30.038435,-30.084428,-30.047509,-30.058845,-30.025520,-30.142524,-30.030803,
              -30.055122,NA,-30.069603,NA,-30.009546,-30.154413,-30.014581,-30.043194,-30.090800,-30.038625,NA,-30.037586,
              -30.010102,-30.036874,-30.029239,-30.061051,-30.029525,-30.086664,-30.119308,-30.049092,-30.043194,-30.110731,
              -30.035226,-30.035875,-30.023790,-30.039581,NA,-29.994899,-30.097930,-30.029324,NA,NA,-30.1110803,-30.0000107,
              -30.0189824,-30.1184035,NA,-30.008845,NA,-29.9822051,-30.0447464,NA,NA,-30.018273,-30.037910,-30.125296,-30.016829,
              -30.027883,-30.028870,NA,-30.018220,NA)
longitude <- c(-51.227923,NA,-51.231179,-51.158236,-51.206637,-51.188390,-51.212131,-51.228945,-51.208420,-51.128803,-51.221512,
               -51.173819,NA,-51.216537,NA,-51.146056,-51.222489,-51.177908,-51.213917,-51.216208,-51.205120,NA,-51.210648,
               -51.159260,-51.209282,-51.206848,-51.149073,-51.214865,-51.206890,-51.207744,-51.209030,-51.153916,-51.110188,
               -51.221475,-51.148588,-51.142576,-51.128412,NA,-51.129390,-51.251136,-51.222498,NA,NA,-51.2568017,-51.2022416,
               -51.1945255,-51.2084237,NA,-51.1956356,NA,-51.1362364,-51.2185621,NA,NA,-51.197460,-51.203430,-51.104843,-51.126675,
               -51.166056,-51.203878,NA,-51.111303,NA)

lat_long <- tibble(
  local = locais,
  lat = latitude,
  long = longitude
)

dados <- left_join(dados, lat_long, by = c("fonte" = "local"))

# criando faixa etária

dados$idade <- as.numeric(dados$idade)

dados$faixa_etaria[dados$idade < 10] <- "0-9"
dados$faixa_etaria[dados$idade < 20 & dados$idade >= 10] <- "10-19"
dados$faixa_etaria[dados$idade < 30 & dados$idade >= 20] <- "20-29"
dados$faixa_etaria[dados$idade < 40 & dados$idade >= 30] <- "30-39"
dados$faixa_etaria[dados$idade < 50 & dados$idade >= 40] <- "40-49"
dados$faixa_etaria[dados$idade < 60 & dados$idade >= 50] <- "50-59"
dados$faixa_etaria[dados$idade < 70 & dados$idade >= 60] <- "60-69"
dados$faixa_etaria[dados$idade < 80 & dados$idade >= 70] <- "70-79"
dados$faixa_etaria[dados$idade >= 80] <- "Mais de 80"

dados$data <- as.Date(dados$data, "%d/%m/%Y")

# transformando em labels mais bonitinhos

lbl <- levels(as.factor(dados$fonte))
lvl <- c("COE RS", "Consultório Privado", "EVDT/DGVS", "Farmácia São João","Hospital Conceição","Hospital da Brigada Militar", "HCPA", 
         "Hospital Divina Providência","Hospital Ernesto Dornelles", "HMAPA","Hospital Mãe de Deus","Hospital Moinhos de Vento","Hospital Porto Alegre",
         "Hospital Restinga", "Hospital Santa Casa","Hospital São Lucas","Hospital Universitário de Pelotas","Hospital Vila Nova","Instituto de Cardiologia",
         "Laboratório Analysis","Laboratório Endocrimeta","Laboratório Exame","Laboratório Felippe","Laboratório Fleury",
         "Laboratório Jeffman","Laboratório Montserrat","Laboratório Pardini","Laboratório Weinmann","Laboratório Zanol","PA Cruzeiro do Sul","PA Lomba do Pinheiro","SMS São Leopoldo","SMS Porto Alegre",
         "UPA Zona Norte","US Guarujá","US Iapi","US Jardim Itu","US Jardim Leopoldina","US Modelo","US Morro Santana","US Navegantes","US Nonoai",
         "US Nova Brasília","US Panorama","US Passo das Pedras","US Santa Cecília","US Santa Maria","US SESC","US Torres","US Tristeza","Unimed","SMS Novo Hamburgo")

View(cbind(lbl,lvl))

dados$fonte <- plyr::mapvalues(dados$fonte, from = lbl, to = lvl)

# adicionando semana epidemiologica

semana_epidemio <- read_csv("bancos/semana_epidemio_dia.csv")

dados <- dados %>%
  left_join(semana_epidemio, by = c("data" = "dia"))

write_csv(dados, "bancos/covid/dados_covid_poa_11_05.csv")
  





