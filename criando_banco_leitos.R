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

leitos_antigos <- read_csv("bancos/leitos/base_antiga/leitos_poa_18_04_21.csv") %>%
  select(-semana_epidemiologica)

dia_atualizacao <- "19-04-2021" # mudar o dia

emergencias_adulto <- tibble(
  local = c("hospital_de_clinicas_de_porto_alegre","hospital_conceicao","instituto_de_cardiologia",
            "hospital_santa_casa","hospital_sao_lucas","hospital_restinga_extremo_sul","hospital_vila_nova",
            "pronto_atendimento_bom_jesus","pronto_atendimento_cruzeiro_do_sul","pronto_atendimento_lomba_do_pinheiro",
            "unidade_de_pronto_atendimento_zona_norte"),
  leitos_total = c(41,64,21,24,17,18,26,7,12,9,17),
  internados = c(73,71,22,32,22,67,22,29,36,13,28),
  data_atualizacao = rep(dia_atualizacao,11)
)

emergencias_pedia <- tibble(local = c("hospital_de_clinicas_de_porto_alegre","hospital_conceicao","hospital_santa_casa","hospital_sao_lucas",
                                      "hospital_materno_infantil_presidente_vargas","hospital_restinga_extremo_sul",
                                      "pronto_atendimento_bom_jesus","pronto_atendimento_cruzeiro_do_sul","pronto_atendimento_lomba_do_pinheiro"),
                            leitos_total = c(9,16,11,4,8,6,5,9,4),
                            internados = c(5,4,2,0,4,0,0,1,0),
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

##### UTI

uti_adulto <- tibble(local = c("instituto_de_cardiologia","hospital_de_clinicas_de_porto_alegre","hospital_conceicao","hospital_moinhos_de_vento",
                               "hospital_santa_casa","hospital_sao_lucas","hospital_mae_de_deus","hospital_ernesto_dornelles","hospital_divina_providencia",
                               "hospital_porto_alegre","hospital_cristo_redentor","hospital_vila_nova",
                               "hospital_de_pronto_socorro","hospital_independencia","hospital_femina",
                               "hospital_restinga_extremo_sul","hospital_santa_ana","hospital_beneficencia_portuguesa"),
                     leitos_total = c(58,197,59,66,141,59,60,40,58,18,39,56,17,38,6,20,10,16),
                     internados = c(47,178,84,84,130,70,65,43,46,10,38,40,17,35,4,20,10,13),
                     leitos_covid = c(12,111,78,76,81,35,48,20,42,10,1,34,0,35,0,19,6,11),
                     data_atualizacao = rep(dia_atualizacao,18))


uti_pedia <- tibble(local = c("hospital_santa_casa","hospital_de_clinicas_de_porto_alegre","instituto_de_cardiologia",
                              "hospital_de_pronto_socorro","hospital_moinhos_de_vento",
                              "hospital_conceicao","hospital_materno_infantil_presidente_vargas"),
                    leitos_total = c(34,13,6,8,11,18,10),
                    internados = c(33,11,4,4,10,14,7),
                    leitos_covid = c(0,1,0,0,0,3,0),
                    data_atualizacao = rep(dia_atualizacao,7))


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
lvl <- c("Hospital Beneficência Portuguesa","Hospital Conceição","Hospital Cristo Redentor","HCPA","HPS","Hospital Divina Providência","Hospital Ernesto Dornelles",
         "Hospital Femina","HospitaL Independência","Hospital Mãe de Deus","HMIPV","Hospital Moinhos de Vento",
         "Hospital Porto Alegre","Hospital Restinga","Hospital Santa Ana","Hospital Santa Casa","Hospital São Lucas",
         "Hospital Vila Nova","Instituto de Cardiologia","PA Bom Jesus","PA Cruzeiro do Sul","PA Lomba do Pinheiro","UPA Zona Norte")

#View(cbind(lbl,lvl))

leitos_novos$local <- plyr::mapvalues(leitos_novos$local, from = lbl, to = lvl)

# adicionando as variaveis que se vai usar no app

leitos_novos <- leitos_novos %>%
  mutate(leitos_disponiveis = leitos_total-internados,
         lotacao = internados/leitos_total)

# adicionando semana epidemiologica

semana_epidemio <- read_csv("bancos/semana_epidemio_dia.csv")

leitos <- bind_rows(leitos_antigos,leitos_novos) %>%
  left_join(semana_epidemio, by = c("data_atualizacao" = "dia"))

write_csv(leitos, "bancos/leitos/base_antiga/leitos_poa_19_04_21.csv")





