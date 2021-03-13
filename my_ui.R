# my_ui

dados <- read_csv("bancos/covid/dados_covid_poa_11_05.csv") 

leitos <- read_csv("bancos/leitos/base_antiga/leitos_poa_12_03_21.csv") 

adultos <- leitos %>%
  filter(classe == "adulto")

pediatricos <- leitos %>%
  filter(classe=="pediatrico")

# criando funcao pra barra de progresso do shiny ui

my_progress_bar <- function(value = 0, label = FALSE, color = "aqua", size = NULL,
                            striped = FALSE, active = FALSE, vertical = FALSE) {
  stopifnot(is.numeric(value))
  if (value > 100) {
    value <- 100
  }
  if (value < 0)
    stop("'value' should be in the range from 0 to 100.", call. = FALSE)
  if (!(color %in% shinydashboard:::validColors || color %in% shinydashboard:::validStatuses))
    stop("'color' should be a valid status or color.", call. = FALSE)
  if (!is.null(size))
    size <- match.arg(size, c("sm", "xs", "xxs"))
  text_value <- paste0(value, "%")
  if (vertical)
    style <- htmltools::css(height = text_value, `min-height` = "2em")
  else
    style <- htmltools::css(width = text_value, `min-width` = "2em")
  tags$div(
    class = "progress",
    class = if (!is.null(size)) paste0("progress-", size),
    class = if (vertical) "vertical",
    class = if (active) "active",
    tags$div(
      class = "progress-bar",
      class = paste0("progress-bar-", color),
      class = if (striped) "progress-bar-striped",
      style = style,
      role = "progressbar",
      `aria-valuenow` = value,
      `aria-valuemin` = 0,
      `aria-valuemax` = 100,
      tags$span(class = if (!label) "sr-only", text_value)
    )
  )
}

# criando função personalizada para a caixa do usuário 

widgetUserBoxx <- function (..., title = NULL, subtitle = NULL, type = NULL, background = FALSE, 
                            backgroundUrl = NULL, src = NULL, color = NULL, footer = NULL, 
                            footer_padding = TRUE, width = 6, height = NULL, boxToolSize = "sm", 
                            collapsible = TRUE, closable = FALSE) 
{
  cl <- "widget-user-header"
  if (!is.null(color) && background == FALSE) 
    cl <- paste0(cl, " bg-", color)
  if (isTRUE(background)) 
    cl <- paste0(cl, " bg-black")
  boxCl <- "box box-widget widget-user"
  if (!is.null(type)) 
    boxCl <- paste0(boxCl, "-", type)
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  backgroundStyle <- NULL
  if (isTRUE(background)) {
    backgroundStyle <- paste0("background: url('", 
                              backgroundUrl, "') center center;")
  }
  shiny::column(width = width, shiny::tags$div(class = boxCl, 
                                               style = style, shiny::tags$div(class = cl, style = backgroundStyle, 
                                                                              shiny::tags$div(class = "pull-right box-tools", 
                                                                                              if (collapsible) {
                                                                                                shiny::tags$button(class = paste0("btn", 
                                                                                                                                  " bg-", color, " btn-", boxToolSize), 
                                                                                                                   `data-widget` = "collapse", type = "button", 
                                                                                                                   shiny::tags$i(class = "fa fa-minus"))
                                                                                              }, if (closable) {
                                                                                                shiny::tags$button(class = paste0("btn", 
                                                                                                                                  " bg-", color, " btn-", boxToolSize), 
                                                                                                                   `data-widget` = "remove", type = "button", 
                                                                                                                   shiny::tags$i(class = "fa fa-times"))
                                                                                              }), 
                                                                              shiny::tags$h3(class = "widget-user-username",
                                                                                             title), shiny::tags$h5(class = "widget-user-desc",
                                                                                                                    subtitle)), shiny::tags$div(class = "box-body", 
                                                                                                                                                ...), shiny::tags$div(class = if (isTRUE(footer_padding)) 
                                                                                                                                                  "box-footer"
                                                                                                                                                  else "box-footer no-padding", footer)))
}

# criando função pra criar caixas de cada hospital

caixinha_hospital <- function(var1,var2,var3) {
  
  antigo_dia <- max(leitos$data_atualizacao)-1
  novo_dia <- max(leitos$data_atualizacao)
  
  aux <- leitos %>%
    filter(classe == var2) %>%
    filter(tipo %in% var1) %>%
    filter(local == var3) %>%
    filter(data_atualizacao %in% novo_dia)
  
  aux2 <- leitos %>%
    filter(classe == var2) %>%
    filter(tipo %in% var1) %>%
    filter(local == var3) %>%
    filter(data_atualizacao %in% antigo_dia)
  
  porcentagem <- sum(aux$internados)/sum(aux$leitos_total)
  
  status <- ifelse(porcentagem < 0.5, "primary",
                   ifelse(porcentagem < 0.75, "warning", "danger"))
  dia_anterior <- sum(aux2$internados)/sum(aux2$leitos_total)
  numero_diff <- sum(aux$internados)-sum(aux2$internados)
  porcentagem_diff <- porcentagem-dia_anterior
  icone <- ifelse(porcentagem_diff < 0, "fa fa-caret-down", "fa fa-caret-up")
  cor <- ifelse(porcentagem_diff < 0 , "green", "red")
  
  if(nrow(aux)!=0) {
    box(
      width = 3,
      title = var3,
      background = NULL,
      status = status,
      my_progress_bar(value = porcentagem*100, striped = T, active = T, color = status),
      footer = fluidRow(
        column(
          width = 6,
          descriptionBlock(
            number = numero_diff,
            #Color = "red",
            #numberIcon = icone,
            header = sum(aux$internados),
            text = "Leitos ocupados"
          )
        ),
        column(
          width = 6,
          descriptionBlock(
            number = paste0(round(porcentagem_diff*100, 2),"%"),
            #Color = "red",
            #numberIcon = icone,
            header = paste0(round(porcentagem*100,2),"%"),
            text = "Lotação"
          )
        )
      )
    )
  }
}


ui <- dashboardPagePlus(
  skin = "green", 
  dashboardHeaderPlus(title = "Dados COVID-19 em Porto Alegre",
                      titleWidth = 400
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Leitos - Adultos", tabName = "leitos_adulto"),
      #menuItem("Covid-19 - Prefeitura", tabName = "casos_pref"),
      menuItem("Casos por bairro", tabName = "casos_ses"),
      menuItem("Leitos - Pediátricos", tabName = "leitos_pedia"),
      menuItem("Fonte de dados", tabName = "fonte"),
      menuItem("CovidMetrika", tabName = "sobre")
    ),
    width = 180
  ),
  
  dashboardBody(
    tabItems(
      tabItem("casos_ses",
              fluidPage(
                titlePanel(
                  fluidRow(
                    column(
                      width = 6,
                      h1("Distribuição dos casos de COVID-19 na cidade de Porto Alegre"),
                      #h5("Fonte de dados: Secretaria de Saúde do Estado do Rio Grande do Sul"),
                    ),
                    column(
                      #tags$img(src="ufrgs_logo.png", height = 100, width = 127),tags$img(src="logo_ime2.png", height = 100, width = 400),
                      tags$img(src = "logos.png", 
                               width = "100%"),
                      width = 6
                    )
                  )
                ),
                
                # para a mensagem de popup
                
                useShinyalert(),
                
                # reescrevendo as cores default para as que eu quero nas boxes de óbitos cartório
                
                tags$style(".small-box.bg-lime { background-color: #404040 !important; color: #FFFFFF !important; }"),
                
                fluidRow(
                  column(
                    width = 7,
                    h3("Selecione a variável de interesse"),
                    radioButtons("var_covid",
                                 label = NULL,
                                 choices = list("Confirmados" = "confirmados","Óbitos" = "obitos",
                                                "Recuperados" = "recuperados", "Em acompanhamento" = "acompanhamento"),
                                 selected = "confirmados",
                                 inline = T)
                  ),
                  column(
                    width = 5,
                    h3("Escolha entre taxa por 100 mil habitantes ou número original"),
                    radioButtons("tipo_covid",
                                 label = NULL,
                                 choices = list("Número" = "", "Taxa" = "_taxa"),
                                 selected = "",
                                 inline = T),
                  )
                ),
                fluidRow(
                  valueBoxOutput("box_conf", width = 3),
                  valueBoxOutput("box_obit", width = 3),
                  valueBoxOutput("box_recu", width = 3),
                  valueBoxOutput("box_acom", width = 3)
                ),
                fluidRow(
                  column(
                    width = 7,
                    textOutput("texto"),
                    mainPanel(
                      leafletOutput("mapa_covid", height = "500px"),
                      HTML("<br><br><br>"), # para dar um espaço entre os gráficos
                      width = 12
                    )
                  ),
                  column(
                    width = 5,
                    box(
                      plotlyOutput("grafico_covid", height = "500px"),
                      width = 12
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    tabBox(id = "tab_covid",
                           width = 12,
                           title = NULL,
                           tabPanel("Diário",
                                    plotlyOutput("serie_covid_dia", height = 500)
                           ),
                           tabPanel("Semana Epidemiológica",
                                    plotlyOutput("serie_covid_sem", height = 500)
                           )
                    )
                  ),
                  column(
                    width = 12,
                    h3("Números de casos por bairro"),
                    selectizeInput(
                      "filtro_serie_covid",
                      width = "100%",
                      label = "Selecione os bairros para a série histórica abaixo",
                      choices = sort(unique(dados_covid_poa$bairro)),
                      selected = sort(unique(dados_covid_poa$bairro)),
                      multiple = T
                    ),
                    tabBox(id = "tab_bairro_covid",
                           width = 12,
                           title = NULL,
                           tabPanel("Diário",
                                    plotlyOutput("serie_bairro_covid_dia", height = 500)
                           ),
                           tabPanel("Semana Epidemiológica",
                                    plotlyOutput("serie_bairro_covid_sem", height = 500)
                           )
                    )
                  )
                    
                )
              )
      ),  
      # tabItem("casos_pref",
      #         fluidPage(
      #           fluidRow(
      #             column(
      #               width = 6,
      #               h1("Casos de COVID-19 entre residentes de Porto Alegre"),
      #               h4("Os boletins epidemiológicos disponibilizados pela Prefeitura de POA 
      #               NÀO SÃO MAIS ATUALIZADOS COM ESSAS INFORMAÇÕES DESDE O DIA 12/05/2020, 
      #                  para os dados atualizados sobre o número de casos em Porto Alegre vá para 
      #                  a aba Covid-19 - Estado."),
      #               dateRangeInput(
      #                 "datas",
      #                 label = "Defina o intervalo de datas",
      #                 start = min(dados$data, na.rm = T),
      #                 end = max(dados$data, na.rm = T),
      #                 min = min(dados$data, na.rm = T),
      #                 max = max(dados$data, na.rm = T),
      #                 format = "dd/mm/yyyy",
      #                 language = "pt-BR",
      #                 separator = " até ",
      #                 width = "700px"
      #               ),
      #               tags$img(src="ufrgs_logo.png", height = 75, width = 95),tags$img(src="logo_ime2.png", height = 75, width = 300)
      #             ),
      #             column(
      #               width = 6,
      #               selectizeInput("fonte",
      #                              label = "Digite as fontes notificadoras(por default todas estão já incluidas)",
      #                              choices = levels(as.factor(dados$fonte)),
      #                              selected = levels(as.factor(dados$fonte)),
      #                              multiple = T,
      #                              width = "900px"
      #               )
      #             )
      #           ),
      #           fluidRow(
      #             valueBoxOutput("box_confi", width = 3),
      #             valueBoxOutput("box_ativo", width = 3),
      #             valueBoxOutput("box_morte", width = 3),
      #             valueBoxOutput("box_recup", width = 3)
      #           ),
      #           textOutput("texto"),
      #           column(
      #             width = 7,
      #             mainPanel(
      #               leafletOutput("mapa_poa", height = "700px"),
      #               HTML("<br><br><br>"), # para dar um espaço entre os gráficos
      #               width = 12
      #             )
      #           ),
      #           column(
      #             width = 5,
      #             box(
      #               title = "Número de casos confirmados por fonte notificadora",
      #               background = "green",
      #               plotlyOutput("barras_fonte", height = "700px"),
      #               width = 12
      #             )
      #           ),
      #           fluidRow(
      #             column(
      #               width = 5,
      #               box(
      #                 title = "Número de casos por faixa etária",
      #                 background = "green",
      #                 plotlyOutput("barras_faixa_etaria", height = "500px"),
      #                 width = 12
      #               )
      #             ),
      #             column(
      #               width = 7,
      #               tabBox(id = "tab_serie",
      #                      width = 12,
      #                      title = "Número de casos novos e acumulados",
      #                      tabPanel("Diário",
      #                               plotlyOutput("barras_dia_pref", height = 500)
      #                      ),
      #                      tabPanel("Semana Epidemiológica",
      #                               plotlyOutput("barras_sem_pref", height = 500)
      #                      )
      #                      
      #               )
      #             )
      #           ),
      #           fluidRow(
      #             column(
      #               width = 6,
      #               box(
      #                 title = "Número de casos por sexo",
      #                 background = "green",
      #                 plotlyOutput("barras_sexo", height = "500px"),
      #                 width = 12
      #               )
      #             ),
      #             column(
      #               width = 6,
      #               box(
      #                 title = "Número de casos por faixa etária e sexo",
      #                 background = "green",
      #                 plotlyOutput("barras_faixa_sexo", height = "500px"),
      #                 width = 12
      #               )
      #             )
      #           )
      #         )
      # ),
      tabItem("leitos_adulto",
              
              # incluindo o script do google analytics para acompanhamento de dados
              
              tags$head(includeHTML(("google_analytics.html"))),
              
              # para a mensagem de popup
              
              useShinyalert(),
              
              fluidPage(
                fluidRow(
                  column(
                    width = 6,
                    tags$img(src = "logos.png", 
                             width = "100%"),
                    h3("Para consulta de dados de leitos de UTI do Rio Grande do Sul acesse: 
                       https://mhbarbian.shinyapps.io/covid19_rs"),
                    br(),
                    dateInput(
                      "data_adulto",
                      label = "Defina a data de atualização(o default é a última data disponível)",
                      value = max(adultos$data_atualizacao, na.rm = T),
                      min = min(adultos$data_atualizacao, na.rm = T),
                      max = max(adultos$data_atualizacao, na.rm = T),
                      format = "dd/mm/yyyy",
                      language = "pt-BR", 
                      width = "700px"
                    ),
                    h3("Selecione a variável de interesse"),
                    radioButtons("var_leitos",
                                 label = NULL,
                                 choices = list("Leitos totais" = "leitos_total","Leitos disponíveis" = "leitos_disponiveis","Lotação" = "lotacao", "Leitos ocupados COVID-19" = "leitos_covid"),
                                 selected = "leitos_disponiveis",
                                 inline = T)
                    #tags$img(src="ufrgs_logo.png", height = 100, width = 127),tags$img(src="logo_ime2.png", height = 100, width = 400)
                  ),
                  column(
                    width = 6,
                    selectizeInput("local_adulto",
                                   label = "Digite os hospitais de interesse(por default todas estão já incluídos)",
                                   choices = levels(as.factor(adultos$local)),
                                   selected = levels(as.factor(adultos$local)),
                                   multiple = T,
                                   width = "900px"
                    ),
                    h4("Escolha se os gráficos devem conter leitos de UTI e/ou emergência"),
                    br(),
                    checkboxGroupInput("tipo_adulto",
                                       label = NULL,
                                       choices = list("UTI" = "uti", "EMERGÊNCIA" = "emergencia"),
                                       selected = c("uti")
                    )
                  )
                ),
                fluidRow(
                  valueBoxOutput("box_opera_adulto", width = 3),
                  valueBoxOutput("box_ocupa_adulto", width = 3),
                  valueBoxOutput("box_porce_adulto", width = 3),
                  valueBoxOutput("box_covid_adulto", width = 3)
                  
                ),
                column(
                  width = 7,
                  mainPanel(
                    leafletOutput("mapa_leitos_adulto", height = "700px"),
                    HTML("<br><br><br>"), # para dar um espaço entre os gráficos
                    width = 12
                  )
                ),
                column(
                  width = 5,
                  box(
                    background = "green",
                    plotlyOutput("barras_hosp_adulto", height = "700px"),
                    width = 12
                  )
                ),
                fluidRow(
                  h3("Os dados não foram atualizados no período de 7 de fevereiro até 23, por isso não são apresentados nos gráficos.
                     Caso queira ver a série completa verifique nosso outro painel com dados do estado(lembrando que as bases de dados são diferentes, portanto resultam em números diferentes também."),
                  column(
                    width = 6,
                    tabBox(
                      id = "tab_ocup",
                      title = "Quantidade de Leitos de UTI OCUPADOS ao longo do tempo",
                      width = 12,
                      tabPanel("Diário",
                               plotlyOutput("serie_leitos_ocup_dia", height = 500)),
                      tabPanel("Semana Epidemiológica",
                               plotlyOutput("serie_leitos_ocup_sem", height = 500))
                    )
                  ),
                  column(
                    width = 6,
                    tabBox(
                      id = "tab_disp",
                      title = "Quantidade de Leitos de UTI DISPONÍVEIS ao longo do tempo",
                      width = 12,
                      tabPanel("Diário",
                               plotlyOutput("serie_leitos_disp_dia", height = 500)),
                      tabPanel("Semana Epidemiológica",
                               plotlyOutput("serie_leitos_disp_sem", height = 500))
                    )
                  ),
                  column(
                    width = 12,
                    tabBox(
                      id = "tab_covid",
                      title = "Quantidade de Leitos de UTI OCUPADOS com pacientes com covid-19 ao longo do tempo",
                      width = 12,
                      tabPanel("Diário",
                               plotlyOutput("serie_leitos_covid_dia", height = 500)),
                      tabPanel("Semana Epidemiológica",
                               plotlyOutput("serie_leitos_covid_sem", height = 500))
                    )
                  )
                ),
                fluidRow(
                  h4("A seguir temos as caixas de cada hospital individualizadas, onde os números coloridos indicam a variação com
                     relação ao dia anterior"),
                  uiOutput("conceicao_adulto"),
                  uiOutput("santa_casa_adulto"),
                  uiOutput("hcpa_adulto"),
                  uiOutput("sao_lucas_adulto"),
                  uiOutput("moinhos_adulto"),
                  uiOutput("mae_adulto"),
                  uiOutput("vila_nova_adulto"),
                  uiOutput("ic_adulto"),
                  uiOutput("ernesto_adulto"),
                  uiOutput("cristo_adulto"),
                  uiOutput("restinga_adulto"),
                  uiOutput("hps_adulto"),
                  uiOutput("divina_adulto"),
                  uiOutput("porto_alegre_adulto"),
                  uiOutput("independencia_adulto"),
                  uiOutput("ana_adulto"),
                  uiOutput("femina_adulto"),
                  uiOutput("pabj_adulto"),
                  uiOutput("pacs_adulto"),
                  uiOutput("palp_adulto"),
                  uiOutput("upa_zn_adulto")
                )
              )      
      ),
      tabItem("leitos_pedia",
              fluidPage(
                fluidRow(
                  column(
                    width = 6,
                    dateInput(
                      "data_pedia",
                      label = "Defina a data de atualização(por default está selecionada a última disponível)",
                      value = max(pediatricos$data_atualizacao, na.rm = T),
                      min = min(pediatricos$data_atualizacao, na.rm = T),
                      max = max(pediatricos$data_atualizacao, na.rm = T),
                      format = "dd/mm/yyyy",
                      language = "pt-BR",
                      width = "700px"
                    ),
                    tags$img(src="ufrgs_logo.png", height = 100, width = 127),tags$img(src="logo_ime2.png", height = 100, width = 400)
                  ),
                  column(
                    width = 6,
                    selectizeInput("local_pedia",
                                   label = "Digite os hospitais de interesse(por default todas estão já incluídos)",
                                   choices = levels(as.factor(pediatricos$local)),
                                   selected = levels(as.factor(pediatricos$local)),
                                   multiple = T,
                                   width = "900px"
                    ),
                    h3("Escolha se os gráficos devem conter leitos de UTI e/ou emergência"),
                    br(),
                    checkboxGroupInput("tipo_pedia",
                                       label = NULL,
                                       choices = list("UTI" = "uti", "EMERGÊNCIA" = "emergencia"),
                                       selected = c("uti","emergencia")
                    )
                  )
                ),
                fluidRow(
                  valueBoxOutput("box_opera_pedia", width = 3),
                  valueBoxOutput("box_ocupa_pedia", width = 3),
                  valueBoxOutput("box_porce_pedia", width = 3),
                  valueBoxOutput("box_covid_pedia", width = 3)
                  
                ),
                fluidRow(
                  h3("Mapa com leitos pediátricos disponíveis em Porto Alegre"),
                  column(
                    width = 7,
                    mainPanel(
                      leafletOutput("mapa_leitos_pedia", height = "700px"),
                      HTML("<br><br><br>"), # para dar um espaço entre os gráficos
                      width = 12
                    )
                  ),
                  column(
                    width = 5,
                    box(
                      title = "Número de leitos disponíveis por hospital",
                      background = "green",
                      plotlyOutput("barras_hosp_pedia", height = "700px"),
                      width = 12
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    box(
                      title = "Número de leitos disponíveis por dia",
                      background = "green",
                      plotlyOutput("linhas_serie_pedia", height = "500px"),
                      width = 12
                    )
                  ),
                  column(
                    width = 6,
                    box(
                      title = "Número de leitos ocupados por dia",
                      background = "green",
                      plotlyOutput("linhas_serie_pedia2", height = "500px"),
                      width = 12
                    )
                  ),
                  h4("A seguir temos as caixas de cada hospital individualizadas, onde os números coloridos indicam a variação com
                   relação ao dia anterior"),
                  uiOutput("conceicao_pedia"),
                  uiOutput("santa_casa_pedia"),
                  uiOutput("hcpa_pedia"),
                  uiOutput("sao_lucas_pedia"),
                  uiOutput("moinhos_pedia"),
                  uiOutput("restinga_pedia"),
                  uiOutput("hps_pedia"),
                  uiOutput("pabj_pedia"),
                  uiOutput("pacs_pedia"),
                  uiOutput("palp_pedia"),
                  uiOutput("mipv_pedia")
                )
              )
      ),
      tabItem("fonte",
              fluidPage(
                fluidRow(
                  
                  # pegando de alguma forma htmlzada a versão mais recente dos ícones font awesome
                  
                  tags$script(src = "https://kit.fontawesome.com/ab15947b75.js", crossorigin="anonymous"), 
                  
                  setZoom(id = "dados_covid",class = "small-box"), # dando um zoomzin quando passa o mouse nos links com base de dados
                  setZoom(id = "dados_uti",class = "small-box"),
                  setZoom(id = "dados_emergencias",class = "small-box"),
                  setZoom(id = "licenca", class = "small-box"),
                  
                  column(
                    width = 3,
                    valueBoxOutput("dados_covid",width = 12)
                  ),
                  column(
                    width = 3,
                    valueBoxOutput("dados_uti",width = 12)
                  ),
                  column(
                    width = 3,
                    valueBoxOutput("dados_emergencias",width = 12)
                  ),
                  column(
                    width = 3,
                    valueBoxOutput("licenca", width = 12)
                  )
                )
                
                
              )
      ),
      tabItem("sobre",
              fluidPage(
                
                setZoom(id = "covidMetrika",class = "small-box"),
                setZoom(id = "git_covidMetrika",class = "small-box"),
                
                fluidRow(
                  column(
                    width = 6,
                    valueBoxOutput("covidMetrika",width = 12)
                  ),
                  column(
                    width = 6,
                    valueBoxOutput("git_covidMetrika", width = 12)
                  ),
                  column(
                    width = 6,
                    valueBoxOutput("dashboard_poa",width = 12)
                  ),
                  column(
                    width = 6,
                    valueBoxOutput("dashboard_br", width = 12)
                  )
                ),
                
                fluidRow(
                  setShadow(class = "box"), # aplicando efeito de sombra nas boxes do grupo
                  
                  widgetUserBox(
                    title = tags$b("Franciele Lobo Pallaoro"),
                    subtitle = "Estudante de Estatística da UFRGS",
                    type = 2,
                    width = 4,
                    src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/franciele.jpg?raw=true',
                    color = "green",
                    "Contato: franpallaoro@gmail.com",
                    footer_padding = F
                  ),
                  
                  widgetUserBox(
                    title = tags$b("Gabriel Holmer Saul"),
                    subtitle = "Estudante de Estatística da UFRGS",
                    type = 2,
                    width = 4,
                    src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/gabriel.jpg?raw=true',
                    color = "green",
                    "Contato: gabrielholmersaul@gmail.com",
                    footer_padding = F
                  )
                  ,
                  
                  widgetUserBox(
                    title = tags$b("Gustavo Machado Utpott"),
                    subtitle = "Estudante de Estatística da UFRGS",
                    type = 2,
                    width = 4,
                    src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/gustavo.png?raw=true',
                    color = "green",
                    "Contato: gustavo.utpott@gmail.com",
                    footer_padding = F
                  ),
                  
                  widgetUserBox(
                    title = tags$b("Juliana Sena de Souza"),
                    subtitle = "Estudante de Pós-Graduação em Epidemiologia da UFRGS",
                    type = 2,
                    width = 4,
                    src =  'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/juliana.jpeg?raw=true',
                    color = "green",
                    "Contato: julianass.estatistica@gmail.com",
                    footer_padding = F
                  ),
                  
                  
                  widgetUserBox(
                    title = tags$b("Márcia Helena Barbian"),
                    subtitle = "Professora do Departamento de Estatística da UFRGS",
                    type = 2,
                    width = 4,
                    src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/marcia.png?raw=true',
                    color = "green",
                    "Contato: mhbarbian@ufrgs.br", 
                    footer_padding = F
                  ), 
                  
                  widgetUserBox(
                    title = tags$b("Rodrigo Citton P. dos Reis"),
                    subtitle = "Professor do Departamento de Estatística da UFRGS",
                    type = 2,
                    width = 4,
                    src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/rodrigo.jpg?raw=true',
                    color = "green",
                    "Contato: citton.padilha@ufrgs.br",
                    footer_padding = F
                  ), 
                  
                  tags$img(src = "https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/logos.png?raw=true", 
                           height = "150", width = "1000")
                )
                
                
                
              )
      )
    )
  )
)

