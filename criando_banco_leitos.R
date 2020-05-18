library(tidyverse)

#########
# leitura
#########

################################
# PARA ATUALIZAR ###############
################################

# pegar o último banco disponivel como banco antigo para dps fazer o merge
# atualizar os 4 bancos de emergências adulto e pediatrico e utis adulto e pediátrico
# conforme o site. Rodar o resto do código até a parte em q salva o banco
#  lá, mudar a data q vai junto com o nome do banco


# hospitais e localização

# emergencias

leitos_antigos <- read_csv("bancos/leitos/leitos_poa_16_05.csv") %>%
  select(-semana_epidemiologica)

dia_atualizacao <- "17-05-2020" # mudar o dia

emergencias_adulto <- tibble(
  local = c("hospital_de_clinicas_de_porto_alegre","hospital_conceicao","instituto_de_cardiologia",
            "hospital_santa_casa","hospital_sao_lucas","hospital_restinga_extremo_sul","hospital_vila_nova",
            "pronto_atendimento_bom_jesus","pronto_atendimento_cruzeiro_do_sul","pronto_atendimento_lomba_do_pinheiro",
            "unidade_de_pronto_atendimento_zona_norte"),
  leitos = c(41,64,21,24,17,18,26,7,12,9,17),
  internados = c(46,19,16,19,10,10,6,8,12,4,7),
  data_atualizacao = rep(dia_atualizacao,11)
)

emergencias_pedia <- tibble(local = c("hospital_de_clinicas_de_porto_alegre","hospital_conceicao","hospital_santa_casa","hospital_sao_lucas",
                                      "hospital_materno_infantil_presidente_vargas","hospital_restinga_extremo_sul",
                                      "pronto_atendimento_bom_jesus","pronto_atendimento_cruzeiro_do_sul","pronto_atendimento_lomba_do_pinheiro"),
                            leitos = c(4,0,11,4,8,6,5,9,4),
                            internados = c(4,0,1,0,2,1,0,0,1),
                            data_atualizacao = rep(dia_atualizacao,9))

locais <- c("centro_de_operacoes_de_emergencias_rs","consultorio_privado","equipe_de_vigilância_de_doencas_transmissiveis",
            "hospital_conceicao","hospital_de_clinicas_de_porto_alegre","hospital_divina_providencia","hospital_ernesto_dornelles",
            "hospital_mae_de_deus","hospital_moinhos_de_vento","hospital_restinga_extremo_sul","hospital_santa_casa","hospital_sao_lucas","paciente",
            "pronto_atendimento_cruzeiro_do_sul", "secretaria_municipal_da_saude_de_sao_leopoldo","unidade_de_pronto_atendimento_zona_norte",
            "unidade_de_saude_guaruja", "unidade_de_saude_iapi","unidade_de_saude_modelo","unidade_de_saude_nonoai",
            "unidade_de_saude_santa_cecilia", "unidade_de_saude_torres","unimed","hospital_cristo_redentor","hospital_de_pronto_socorro",
            "hospital_femina","hospital_independencia","hospital_materno_infantil_presidente_vargas","hospital_santa_ana",
            "hospital_vila_nova","instituto_de_cardiologia","pronto_atendimento_bom_jesus","pronto_atendimento_lomba_do_pinheiro",
            "hospital_porto_alegre")


latitude <- c(-30.030921,NA,-30.062808,-30.015870,-30.038435,-30.084428,-30.047509,-30.058845,-30.025520,-30.142524,-30.030803,
              -30.055122,NA,-30.069603,NA,-30.009546,-30.154413,-30.014581,-30.043194,-30.090800,-30.038625,NA,-30.037586,
              -30.010102,-30.036874,-30.029239,-30.061051,-30.029525,-30.086664,-30.119308,-30.049092,-30.043194,-30.110731,
              -30.04475)
longitude <- c(-51.227923,NA,-51.231179,-51.158236,-51.206637,-51.188390,-51.212131,-51.228945,-51.208420,-51.128803,-51.221512,
               -51.173819,NA,-51.216537,NA,-51.146056,-51.222489,-51.177908,-51.213917,-51.216208,-51.205120,NA,-51.210648,
               -51.159260,-51.209282,-51.206848,-51.149073,-51.214865,-51.206890,-51.207744,-51.209030,-51.153916,-51.110188,
               -51.21856)

lat_long <- tibble(
  local = locais,
  lat = latitude,
  long = longitude
)

emergencias_adulto <- left_join(emergencias_adulto, lat_long, by = "local")
emergencias_pedia <- left_join(emergencias_pedia, lat_long, by = "local")

##### UTI

uti_adulto <- tibble(local = c("hospital_conceicao","hospital_de_clinicas_de_porto_alegre","hospital_santa_casa","hospital_sao_lucas",
                               "hospital_cristo_redentor","hospital_moinhos_de_vento","hospital_ernesto_dornelles","hospital_vila_nova",
                               "hospital_de_pronto_socorro","hospital_independencia","hospital_femina","hospital_divina_providencia",
                               "hospital_restinga_extremo_sul","hospital_santa_ana","hospital_mae_de_deus","hospital_porto_alegre"),
                     leitos = c(69,102,87,59,47,56,40,20,20,10,4,16,10,10,60,7),
                     internados = c(55,75,73,46,27,46,32,19,17,9,4,14,9,7,27,7),
                     covid = c(8,16,0,3,0,4,7,0,0,0,0,2,0,0,0,1),
                     data_atualizacao = rep(dia_atualizacao,16))


uti_pedia <- tibble(local = c("hospital_santa_casa","hospital_de_clinicas_de_porto_alegre",
                              "hospital_de_pronto_socorro","hospital_moinhos_de_vento",
                              "hospital_conceicao","hospital_materno_infantil_presidente_vargas"),
                    leitos = c(37,13,8,11,18,12),
                    internados = c(32,8,2,8,9,5),
                    covid = c(0,0,0,0,0,0),
                    data_atualizacao = rep(dia_atualizacao,6))

uti_adulto <- left_join(uti_adulto, lat_long, by = "local")
uti_pedia <- left_join(uti_pedia, lat_long, by = "local")


# juntando os bancos

leitos_novos <- tibble(
  tipo = c(rep("emergencia", nrow(emergencias_adulto)+nrow(emergencias_pedia)),rep("uti",nrow(uti_adulto)+nrow(uti_pedia))),
  classe = c(rep("adulto", nrow(emergencias_adulto)), rep("pediatrico", nrow(emergencias_pedia)), rep("adulto", nrow(uti_adulto)), rep("pediatrico", nrow(uti_pedia)))
)

aux <- bind_rows(emergencias_adulto,emergencias_pedia,uti_adulto, uti_pedia)

leitos_novos <- bind_cols(leitos_novos,aux)

leitos_novos$data_atualizacao <- as.Date(leitos_novos$data_atualizacao, "%d-%m-%Y")

# transformando em labels mais bonitinhos

lbl <- levels(as.factor(leitos_novos$local))
lvl <- c("Hospital Conceição","Hospital Cristo Redentor","HCPA","HPS","Hospital Divina Providência","Hospital Ernesto Dornelles",
         "Hospital Femina","HospitaL Independência","Hospital Mãe de Deus","HMIPV","Hospital Moinhos de Vento",
         "Hospital Porto Alegre","Hospital Restinga","Hospital Santa Ana","Hospital Santa Casa","Hospital São Lucas",
         "Hospital Vila Nova","Instituto de Cardiologia","PA Bom Jesus","PA Cruzeiro do Sul","PA Lomba do Pinheiro","UPA Zona Norte")

View(cbind(lbl,lvl))

leitos_novos$local <- plyr::mapvalues(leitos_novos$local, from = lbl, to = lvl)

# adicionando semana epidemiologica

semana_epidemio <- read_csv("bancos/semana_epidemio_dia.csv")

leitos <- bind_rows(leitos_antigos,leitos_novos) %>%
  left_join(semana_epidemio, by = c("data_atualizacao" = "dia"))

write_csv(leitos, "bancos/leitos/leitos_poa_17_05.csv")

