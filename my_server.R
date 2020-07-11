########## 
# my_server

opcoes <- list(
  "confirmados" = list("cor" = "#dd4b39", "paleta" = "Reds", "texto" = "Confirmados"),
  "confirmados_taxa" = list("cor" = "#dd4b39", "paleta" = "Reds", "texto" = "Taxa de confirmados"),
  "obitos" = list("cor" = "#2e2e2e", "paleta" = "Greys", "texto" = "Óbitos"),
  "obitos_taxa" = list("cor" = "#2e2e2e", "paleta" = "Greys", "texto" = "Taxa de óbitos"),
  "recuperados" = list("cor" = "#0073b7", "paleta" = "Blues", "texto" = "Recuperados"),
  "recuperados_taxa" = list("cor" = "#0073b7", "paleta" = "Blues", "texto" = "Taxa de recuperados"),
  "acompanhamento" = list("cor" = "#ff851b", "paleta" = "Oranges", "texto" = "Em acompanhamento"),
  "acompanhamento_taxa" = list("cor" = "#ff851b", "paleta" = "Oranges", "texto" = "Taxa de acompanhamento")
)

dados <- read_csv("bancos/covid/dados_covid_poa_11_05.csv") 

leitos <- read_csv("bancos/leitos/base_antiga/leitos_poa_11_07.csv") 

adultos <- leitos %>%
  filter(classe == "adulto")

pediatricos <- leitos %>%
  filter(classe=="pediatrico")

server <- function(input, output) {
  
  shinyalert("Olá", "Caso você esteja acessando o dashboard pelo celular, sugerimos que o coloque na posição horizontal para uma melhor visualização dos gráficos!", type = "info")
  
  
  #############################
  ####### ABA COVID SES #######
  #############################
  
  # caixas com numeros gerais
  
  # caixas com numeros gerais
  
  # caixa de confirmados
  output$box_conf <- renderValueBox({
    
    aux <- dados_covid_poa %>%
      summarise(confirmados = n(), populacao_estimada = first(populacao_estimada))
    
    numero <- ifelse(input$tipo_covid == "_taxa", 
                     round(sum(aux$confirmados)*100000/sum(aux$populacao_estimada),2),
                     sum(aux$confirmados))
    
    valueBox(
      value = numero,
      subtitle = opcoes[[str_c("confirmados",input$tipo_covid)]][["texto"]],
      icon = icon("virus"),
      color = "red" 
    )
  })
  # caixa de óbitos
  output$box_obit <- renderValueBox({
    aux <- dados_covid_poa %>%
      filter(evolucao == "OBITO") %>%
      summarise(obitos = n(), populacao_estimada = first(populacao_estimada))
    
    numero <- ifelse(input$tipo_covid == "_taxa", 
                     round(sum(aux$obitos)*100000/sum(aux$populacao_estimada),2), 
                     sum(aux$obitos))
    
    valueBox(
      numero,
      opcoes[[str_c("obitos",input$tipo_covid)]][["texto"]],
      icon = icon("heartbeat"),
      color = "lime"
    )
  })
  # caixa de recuperados
  output$box_recu <- renderValueBox({
    aux <- dados_covid_poa %>%
      filter(evolucao == "RECUPERADO") %>%
      summarise(recuperados = n(), populacao_estimada = first(populacao_estimada))
    
    numero <- ifelse(input$tipo_covid == "_taxa", 
                     round(sum(aux$recuperados)*100000/sum(aux$populacao_estimada),2),
                     sum(aux$recuperados))
    
    valueBox(
      numero,
      opcoes[[str_c("recuperados",input$tipo_covid)]][["texto"]],
      icon = icon("virus-slash"),
      color = "blue",
      href = NULL 
    )
  })
  # caixa de em acompanhamento
  output$box_acom <- renderValueBox({
    aux <- dados_covid_poa %>%
      filter(evolucao == "EM ACOMPANHAMENTO") %>%
      summarise(acompanhamento = n(), populacao_estimada = first(populacao_estimada))
    
    numero <- ifelse(input$tipo_covid == "_taxa", 
                     round(sum(aux$acompanhamento)*100000/sum(aux$populacao_estimada),2),
                     sum(aux$acompanhamento))
    
    valueBox(
      numero,
      opcoes[[str_c("acompanhamento",input$tipo_covid)]][["texto"]],
      icon = icon("clinic-medical"),
      color = "yellow"
    )
  })
  
  output$texto <- renderText({
    
    aux <- dados_covid_poa %>%
      filter(is.na(bairro))

    paste("Houve", nrow(aux), "casos que não foram introduzidos ao mapa,
          por não termos informações do bairro")
    
  })
  
  #####################
  # Mapa_covid
  
  output$mapa_covid <- renderLeaflet({
    
    var <- rlang::sym(str_c(input$var_covid,input$tipo_covid))
    
    aux_mapa <- mapa_poa %>%
      mutate(var = replace_na(!!var, 0))
    
    y_quantidade <- aux_mapa$var
    
    variavel <- aux_mapa$NOME
    
    # criando intervalo com uma função muito boa
    
    intervalos <- classInt::classIntervals(var = y_quantidade, n = 7, style = "fisher")
    
    intervalos[["brks"]][1:2] <- c(0,1)
    
    pal <- colorBin(palette=opcoes[[str_c(input$var_covid,input$tipo_covid)]][["paleta"]], domain = y_quantidade, bins = intervalos[["brks"]])
    
    leaflet(aux_mapa) %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
      addPolygons(fillColor = ~pal(y_quantidade), 
                  weight = 1.5,
                  opacity = 0.7,
                  fillOpacity = 0.7,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf("%s - %s", variavel, round(y_quantidade,3)),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "15px",
                    direction = "auto"))   %>%
      addLegend(pal = pal, values = round(y_quantidade,0), labFormat = function(type, cuts, p) {  # legenda para colorQuantile
        n = length(cuts)
        paste0(round(cuts[-n],0), " &ndash; ", round(cuts[-1],0))},
        title = opcoes[[str_c(input$var_covid,input$tipo_covid)]][["texto"]],
        labels = ~variavel,
        position = "bottomright")
    
  })
  
  #############
  # grafico_covid
  
  output$grafico_covid <- renderPlotly({
    
    var <- rlang::sym(str_c(input$var_covid,input$tipo_covid))
    text_tooltip <- names(opcoes)[names(opcoes)!=str_c(input$var_covid,input$tipo_covid)]
    outra_var <- sym(text_tooltip[1])
    outra_var2 <- sym(text_tooltip[2])
    outra_var3 <- sym(text_tooltip[3])
    outra_var4 <- sym(text_tooltip[4])
    outra_var5 <- sym(text_tooltip[5])
    outra_var6 <- sym(text_tooltip[6])
    outra_var7 <- sym(text_tooltip[7])
    
    
    p <- dados_covid_poa %>%
      mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
             acompanhamento = ifelse(evolucao == "EM ACOMPANHAMENTO", 1, 0),
             recuperados = ifelse(evolucao == "RECUPERADO", 1, 0)) %>% 
      group_by(bairro) %>%
      summarise(confirmados = n(), confirmados_taxa = n()*100000/first(populacao_bairro),
                obitos = sum(obitos, na.rm = T), obitos_taxa = sum(obitos, na.rm = T)*100000/first(populacao_bairro), 
                acompanhamento = sum(acompanhamento, na.rm = T), acompanhamento_taxa = sum(acompanhamento, na.rm = T)*100000/first(populacao_bairro),
                recuperados = sum(recuperados, na.rm = T), recuperados_taxa = sum(recuperados, na.rm = T)*100000/first(populacao_bairro),
                populacao = first(populacao_bairro)) %>%
      arrange(desc(!!var)) %>%
      slice_head(n = 25) %>%
      ggplot(aes(x = reorder(bairro, !!var), y = !!var, text = paste(bairro,paste0(str_c(input$var_covid,input$tipo_covid)," ",round(!!var,0)),paste0(text_tooltip[1]," ",round(!!outra_var,0)),paste0(text_tooltip[2]," ",round(!!outra_var2,0)),paste0(text_tooltip[3]," ",round(!!outra_var3,0)),paste0(text_tooltip[4]," ",round(!!outra_var4,0)),paste0(text_tooltip[5]," ",round(!!outra_var5,0)),paste0(text_tooltip[6]," ",round(!!outra_var6,0)),paste0(text_tooltip[7]," ",round(!!outra_var7,0)),paste0("populacao ",!!sym("populacao")),sep = "\n"))) +
      geom_col(fill = opcoes[[str_c(input$var_covid,input$tipo_covid)]][["cor"]]) +
      labs(x = "Bairro", y = opcoes[[str_c(input$var_covid,input$tipo_covid)]][["texto"]]) +
      coord_flip()
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  #############
  # ui_serie_covid
  
  output$ui_serie_covid <- renderUI({
    
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
    
  })
  
  ############
  # serie_covid_dia
  
  output$serie_covid_dia <- renderPlotly({
    
    #input <- list(var_covid = "acompanhamento", agrup_covid = "municipio", filtro_covid = unique(dados_covid_poa$regiao_covid), filtro_serie_covid ="Todos selecionados")
    
    var <- rlang::sym(input$var_covid)
    
    var= 
      
      if(input$var_covid %in% c("confirmados","incidencia")) {
        aux <- dados_covid_poa %>%
          group_by(data_confirmacao) %>%
          summarise(confirmados = n(), incidencia = n()*100000/first(populacao_estimada)) %>%
          mutate(frequencia = !!var) %>%
          arrange(data_confirmacao)
        
        n_days <- max(aux$data_confirmacao)-min(aux$data_confirmacao)
        dias <- min(aux$data_confirmacao)+days(0:n_days)
      } else if(input$var_covid %in% c("obitos","mortalidade","recuperados")){
        aux <- dados_covid_poa %>%
          mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
                 recuperados = ifelse(evolucao == "RECUPERADO", 1, 0)) %>% 
          group_by(data_evolucao) %>%
          summarise(obitos = sum(obitos, na.rm = T), mortalidade = sum(obitos, na.rm = T)*100000/first(populacao_estimada),
                    recuperados = sum(recuperados, na.rm = T)) %>%
          filter(!is.na(data_evolucao)) %>%
          mutate(frequencia = !!var) %>%
          ungroup() %>%
          mutate(data_confirmacao = data_evolucao) %>%
          select(-data_evolucao) %>%
          arrange(data_confirmacao)
        
        n_days <- max(aux$data_confirmacao)-min(aux$data_confirmacao)
        dias <- min(aux$data_confirmacao)+days(0:n_days)
      } else {
        aux <- dados_covid_poa %>%
          mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
                 acompanhamento = ifelse(is.na(evolucao), 0, 1),
                 recuperados = ifelse(evolucao == "RECUPERADO", 1, 0))
        negativos <- aux %>%
          group_by(data_evolucao) %>%
          filter(!is.na(data_evolucao)) %>%
          summarise(negativos = sum(obitos,na.rm = T)+sum(recuperados, na.rm =T)) %>%
          mutate(data_confirmacao = data_evolucao) %>%
          select(-data_evolucao)
        
        acomp <- aux %>%
          group_by(data_sintomas) %>%
          summarise(acompanhamento = sum(acompanhamento, na.rm = T)) %>%
          mutate(frequencia = acompanhamento) %>%
          mutate(data_confirmacao = data_sintomas) %>%
          select(-data_sintomas)
        
        
        n_days <- max(negativos$data_confirmacao,acomp$data_confirmacao)-min(negativos$data_confirmacao,acomp$data_confirmacao)
        dias <- min(negativos$data_confirmacao,acomp$data_confirmacao)+days(0:n_days)
      }
    
    if(input$var_covid != "acompanhamento") {
      aux2 <- tibble(data_confirmacao = dias[!(dias %in% aux$data_confirmacao)],
                     frequencia = 0)
      
      aux <- bind_rows(aux,aux2) %>%
        arrange(data_confirmacao)
      
      aux$acumulado <- c(aux$frequencia[1],rep(0,n_days))
      
      label_x <- "Dia de confirmação"
      caption_x <- "*Dados referentes à data de 'confirmação', e não 'notificação', portanto dados antigos são frequentemente adicionados"
      y_caption <- 0.99
      label_y <- "Acumulado"
      
      for (i in 2:nrow(aux)) {
        aux$acumulado[i] <- aux$acumulado[i-1]+aux$frequencia[i]
      }
    } else {
      negativos2 <- tibble(data_confirmacao = dias[!(dias %in% negativos$data_confirmacao)],
                           negativos = 0)
      
      negativos <- bind_rows(negativos,negativos2) %>%
        arrange(data_confirmacao)
      
      acomp2 <- tibble(data_confirmacao = dias[!(dias %in% acomp$data_confirmacao)],
                       frequencia = 0)
      
      acomp <- bind_rows(acomp,acomp2) %>%
        arrange(data_confirmacao) %>%
        left_join(negativos, by = "data_confirmacao") %>%
        mutate(negativos = ifelse(is.na(negativos),0,negativos)) %>%
        mutate(frequencia = frequencia-negativos)
      
      acomp$acumulado <- c(acomp$frequencia[1],rep(0,n_days))
      
      label_x <- "Dia de início dos sintomas"
      caption_x <- "*Dados referentes à data de início dos sintomas, portanto dados mais antigos são frequentemente adicionados"
      y_caption <- 0.99
      label_y <- "Em acompanhamento"
      
      for (i in 2:nrow(acomp)) {
        acomp$acumulado[i] <- acomp$acumulado[i-1]+acomp$frequencia[i]
      }
      
      aux <- acomp %>%
        mutate(acumulado = ifelse(acumulado < 0, 0, acumulado))
    }
    
    ordem <- as.character(format(aux$data_confirmacao, "%d-%b"))
    
    aux$data_confirmacao <- as.character(format(aux$data_confirmacao, "%d-%b"))
    
    if(input$var_covid != "acompanhamento") {
      
      p <- ggplotly(ggplot(aux) +
                      geom_line(aes(x = data_confirmacao, y = acumulado, group = 1, color = "Acumulado"), linetype = 'dotted') +
                      geom_point(aes(x = data_confirmacao, y = acumulado, color = "Acumulado")) + 
                      geom_col(aes(x = data_confirmacao, y = frequencia, fill = "Frequência")) +
                      scale_x_discrete(limits = ordem) +
                      scale_color_manual(values = list("Acumulado" = opcoes[[input$var_covid]][["cor"]])) +
                      scale_fill_manual(values = list("Frequência" = opcoes[[input$var_covid]][["cor"]])) +
                      labs(x = label_x, y = opcoes[[input$var_covid]][["texto"]], colour = NULL, fill = NULL) +
                      theme(axis.text.x = element_text(angle=90,size=8, vjust = 0.5)) +
                      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                            panel.grid.major = element_blank(),
                            legend.position=c(0.05, 0.95),
                            legend.key = element_blank())) %>%
        layout(legend = list(
          orientation = "v",
          x = 0.01,
          y = 0.95
        ),
        annotations = list(
          list(x = 0.001, y = y_caption, text = caption_x, 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='left', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10, color="gray"))
        )
        )
    } else {
      p <- ggplotly(ggplot(aux) +
                      geom_line(aes(x = data_confirmacao, y = acumulado, group = 1, color = "Em acompanhamento"), linetype = 'dotted') +
                      geom_point(aes(x = data_confirmacao, y = acumulado, color = "Em acompanhamento")) + 
                      geom_col(aes(x = data_confirmacao, y = frequencia, fill = "Frequência")) +
                      scale_x_discrete(limits = ordem) +
                      scale_color_manual(values = list("Em acompanhamento" = opcoes[[input$var_covid]][["cor"]])) +
                      scale_fill_manual(values = list("Frequência" = opcoes[[input$var_covid]][["cor"]])) +
                      labs(x = label_x, y = opcoes[[input$var_covid]][["texto"]], colour = NULL, fill = NULL) +
                      theme(axis.text.x = element_text(angle=90,size=8, vjust = 0.5)) +
                      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                            panel.grid.major = element_blank(),
                            legend.position=c(0.05, 0.95),
                            legend.key = element_blank())) %>%
        layout(legend = list(
          orientation = "v",
          x = 0.01,
          y = 0.95
        ),
        annotations = list(
          list(x = 0.001, y = y_caption, text = caption_x, 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='left', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10, color="gray"))
        )
        )
    }
    
    for (i in 1:length(p$x$data)){
      if (!is.null(p$x$data[[i]]$name)){
        p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    p
  })
  
  
  ############
  # serie_covid_semana
  
  output$serie_covid_sem <- renderPlotly({
    
    #input <- list(var_covid = "acompanhamento", agrup_covid = "municipio", filtro_covid = unique(dados_covid_poa$regiao_covid), filtro_serie_covid ="Todos selecionados")
    
    var <- rlang::sym(input$var_covid)
    
    
    if(input$var_covid %in% c("confirmados","incidencia")) {
      aux <- dados_covid_poa %>%
        group_by(semana_epidemiologica_confirmacao) %>%
        summarise(confirmados = n(), incidencia = n()*100000/first(populacao_estimada)) %>%
        mutate(frequencia = !!var) %>%
        arrange(semana_epidemiologica_confirmacao)
      
      n_weeks <- max(aux$semana_epidemiologica_confirmacao)-min(aux$semana_epidemiologica_confirmacao)
      weeks <- min(aux$semana_epidemiologica_confirmacao):(min(aux$semana_epidemiologica_confirmacao)+n_weeks)
    } else if(input$var_covid %in% c("obitos","mortalidade","recuperados")){
      aux <- dados_covid_poa %>%
        mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
               recuperados = ifelse(evolucao == "RECUPERADO", 1, 0)) %>% 
        group_by(semana_epidemiologica_evolucao) %>%
        summarise(obitos = sum(obitos, na.rm = T), mortalidade = sum(obitos, na.rm = T)*100000/first(populacao_estimada),
                  recuperados = sum(recuperados, na.rm = T)) %>%
        filter(!is.na(semana_epidemiologica_evolucao)) %>%
        mutate(frequencia = !!var) %>%
        ungroup() %>%
        mutate(semana_epidemiologica_confirmacao = semana_epidemiologica_evolucao) %>%
        select(-semana_epidemiologica_evolucao) %>%
        arrange(semana_epidemiologica_confirmacao)
      
      n_weeks <- max(aux$semana_epidemiologica_confirmacao)-min(aux$semana_epidemiologica_confirmacao)
      weeks <- min(aux$semana_epidemiologica_confirmacao):(min(aux$semana_epidemiologica_confirmacao)+n_weeks)
    } else {
      
      aux <- dados_covid_poa %>%
        mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
               acompanhamento = ifelse(is.na(evolucao), 0, 1),
               recuperados = ifelse(evolucao == "RECUPERADO", 1, 0))
      negativos <- aux %>%
        group_by(semana_epidemiologica_evolucao) %>%
        filter(!is.na(semana_epidemiologica_evolucao)) %>%
        summarise(negativos = sum(obitos,na.rm = T)+sum(recuperados, na.rm =T)) %>%
        mutate(semana_epidemiologica_confirmacao = semana_epidemiologica_evolucao) %>%
        select(-semana_epidemiologica_evolucao)
      
      acomp <- aux %>%
        group_by(semana_epidemiologica_sintomas) %>%
        summarise(acompanhamento = sum(acompanhamento, na.rm = T)) %>%
        mutate(frequencia = acompanhamento) %>%
        mutate(semana_epidemiologica_confirmacao = semana_epidemiologica_sintomas) %>%
        select(-semana_epidemiologica_sintomas)
      
      
      n_weeks <- max(negativos$semana_epidemiologica_confirmacao,acomp$semana_epidemiologica_confirmacao)-min(negativos$semana_epidemiologica_confirmacao,acomp$semana_epidemiologica_confirmacao)
      weeks <- min(negativos$semana_epidemiologica_confirmacao,acomp$semana_epidemiologica_confirmacao):(min(negativos$semana_epidemiologica_confirmacao,acomp$semana_epidemiologica_confirmacao)+n_weeks)
      
    }
    
    if(input$var_covid != "acompanhamento") {
      
      aux2 <- tibble(semana_epidemiologica_confirmacao = weeks[!(weeks %in% aux$semana_epidemiologica_confirmacao)],
                     frequencia = 0)
      
      aux <- bind_rows(aux,aux2) %>%
        arrange(semana_epidemiologica_confirmacao)
      
      aux$acumulado <- c(aux$frequencia[1],rep(0,n_weeks))
      
      for (i in 2:nrow(aux)) {
        aux$acumulado[i] <- aux$acumulado[i-1]+aux$frequencia[i]
      }
      
      label_x <- "Semana epidemiológica de confirmação"
      caption_x <- "*Dados referentes à data de 'confirmação', e não 'notificação', portanto dados antigos são frequentemente adicionados"
      y_caption <- 0.99
      
      ordem <- as.character(aux$semana_epidemiologica_confirmacao)
      
      aux$semana_epidemiologica_confirmacao <- as.character(aux$semana_epidemiologica_confirmacao)
      
      p <- ggplotly(ggplot(aux) +
                      geom_line(aes(x = semana_epidemiologica_confirmacao, y = acumulado, group = 1, color = "Acumulado"), linetype = 'dotted') +
                      geom_point(aes(x = semana_epidemiologica_confirmacao, y = acumulado, color = "Acumulado")) + 
                      geom_col(aes(x = semana_epidemiologica_confirmacao, y = frequencia, fill = "Frequência")) +
                      scale_x_discrete(limits = ordem) +
                      scale_color_manual(values = list("Acumulado" = opcoes[[input$var_covid]][["cor"]])) +
                      scale_fill_manual(values = list("Frequência" = opcoes[[input$var_covid]][["cor"]])) +
                      labs(x = label_x, y = opcoes[[input$var_covid]][["texto"]], colour = NULL, fill = NULL) +
                      theme(axis.text.x = element_text(angle=90,size=8, vjust = 0.5)) +
                      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                            panel.grid.major = element_blank(),
                            legend.position=c(0.05, 0.95),
                            legend.key = element_blank())) %>%
        layout(legend = list(
          orientation = "v",
          x = 0.01,
          y = 0.95
        ),
        annotations = list(
          list(x = 0.001, y = y_caption, text = caption_x, 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='left', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10, color="gray"))
        )
        )
      
    } else {
      
      negativos2 <- tibble(semana_epidemiologica_confirmacao = weeks[!(weeks %in% negativos$semana_epidemiologica_confirmacao)],
                           negativos = 0)
      
      negativos <- bind_rows(negativos,negativos2) %>%
        arrange(semana_epidemiologica_confirmacao)
      
      acomp2 <- tibble(semana_epidemiologica_confirmacao = weeks[!(weeks %in% acomp$semana_epidemiologica_confirmacao)],
                       frequencia = 0)
      
      acomp <- bind_rows(acomp,acomp2) %>%
        arrange(semana_epidemiologica_confirmacao) %>%
        left_join(negativos, by = "semana_epidemiologica_confirmacao") %>%
        mutate(negativos = ifelse(is.na(negativos),0,negativos)) %>%
        mutate(frequencia = frequencia-negativos)
      
      acomp$acumulado <- c(acomp$frequencia[1],rep(0,n_weeks))
      
      label_x <- "Semana epidemiológica de início dos sintomas"
      caption_x <- "*Dados referentes à data de início dos sintomas, portanto dados mais antigos são frequentemente adicionados"
      y_caption <- 0.99
      
      for (i in 2:nrow(acomp)) {
        acomp$acumulado[i] <- acomp$acumulado[i-1]+acomp$frequencia[i]
      }
      
      aux <- acomp %>%
        mutate(acumulado = ifelse(acumulado < 0, 0, acumulado))
      
      ordem <- as.character(aux$semana_epidemiologica_confirmacao)
      
      aux$semana_epidemiologica_confirmacao <- as.character(aux$semana_epidemiologica_confirmacao)
      
      p <- ggplotly(ggplot(aux) +
                      geom_line(aes(x = semana_epidemiologica_confirmacao, y = acumulado, group = 1, color = "Em acompanhamento"), linetype = 'dotted') +
                      geom_point(aes(x = semana_epidemiologica_confirmacao, y = acumulado, color = "Em acompanhamento")) + 
                      geom_col(aes(x = semana_epidemiologica_confirmacao, y = frequencia, fill = "Frequência")) +
                      scale_x_discrete(limits = ordem) +
                      scale_color_manual(values = list("Em acompanhamento" = opcoes[[input$var_covid]][["cor"]])) +
                      scale_fill_manual(values = list("Frequência" = opcoes[[input$var_covid]][["cor"]])) +
                      labs(x = label_x, y = opcoes[[input$var_covid]][["texto"]], colour = NULL, fill = NULL) +
                      theme(axis.text.x = element_text(angle=90,size=8, vjust = 0.5)) +
                      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                            panel.grid.major = element_blank(),
                            legend.position=c(0.05, 0.95),
                            legend.key = element_blank())) %>%
        layout(legend = list(
          orientation = "v",
          x = 0.01,
          y = 0.95
        ),
        annotations = list(
          list(x = 0.001, y = y_caption, text = caption_x, 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='left', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10, color="gray"))
        )
        )
      
    }
    
    for (i in 1:length(p$x$data)){
      if (!is.null(p$x$data[[i]]$name)){
        p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    p
  })
  
  
  #############################
  ####### ABA COVID PREF ######
  #############################
  
  # caixas com numeros gerais
  
  # caixa de confirmados
  # output$box_confi <- renderValueBox({
  #   aux <- dados %>%
  #     filter(data >= input$datas[1] & data <= input$datas[2]) %>%
  #     filter(fonte %in% input$fonte)
  #   
  #   sem_fonte <- nrow(filter(dados, is.na(fonte)))
  #   sem_data <- nrow(filter(dados, is.na(data)))
  #   
  #   total <- nrow(aux) + sem_fonte + sem_data
  #   
  #   valueBox(
  #     total,
  #     "Número de casos confirmados",
  #     icon = icon("notes-medical"),
  #     color = "red" 
  #   )
  # })
  # # caixa ativos
  # output$box_ativo <- renderValueBox({
  #   aux <- dados %>%
  #     filter(data >= input$datas[1] & data <= input$datas[2]) %>%
  #     filter(fonte %in% input$fonte)
  #   
  #   total <- nrow(aux)
  #   
  #   sem_fonte <- nrow(filter(dados, is.na(fonte)))
  #   
  #   ativos <- ifelse(total+sem_fonte-18-412 < 0,0,total+sem_fonte-18-412)
  #   
  #   valueBox(
  #     ativos,
  #     "Número de casos ativos",
  #     icon = icon("first-aid"),
  #     color = "red" 
  #   )
  # })
  # # caixa de mortes
  # output$box_morte <- renderValueBox({
  #   mortes <- 18
  #   valueBox(
  #     mortes,
  #     "Número de mortes",
  #     icon = icon("heartbeat"),
  #     color = "purple"
  #   )
  # })
  # # caixas de recuperados
  # output$box_recup <- renderValueBox({
  #   recuperados <- 412
  #   valueBox(
  #     recuperados,
  #     "Número de casos recuperados",
  #     icon = icon("heart"),
  #     color = "green"
  #   )
  #   
  # })
  
  
  
  ## gráfico de barras faixa etária e sexo
  #
  # output$barras_faixa_sexo <- renderPlotly({
  #   
  #   aux <- dados %>%
  #     filter(data >= input$datas[1] & data <= input$datas[2]) %>%
  #     filter(fonte %in% input$fonte) %>%
  #     filter(!(is.na(faixa_etaria) | is.na(sexo))) %>%
  #     group_by(faixa_etaria,sexo) %>%
  #     summarise(casos = n())
  #   
  #   p <- ggplot(aux, aes(x = faixa_etaria, fill = sexo, y = casos)) +
  #     geom_col() +
  #     geom_text(aes(label = casos), position = position_stack()) +
  #     labs(x = "Faixa Etária", y = "Casos", fill = "Sexo") +
  #     scale_fill_brewer(palette = "Dark2")
  #   
  #   ggplotly(p) %>%
  #     style(textposition = "top")
  #   
  # })
  # 
  # ## Gráfico de barras casos por sexo
  # #
  # 
  # output$barras_sexo <- renderPlotly({
  #   
  #   aux <- dados %>%
  #     filter(data >= input$datas[1] & data <= input$datas[2]) %>%
  #     filter(fonte %in% input$fonte) %>%
  #     filter(!is.na(sexo)) %>%
  #     group_by(sexo) %>%
  #     summarise(casos = n())
  #   
  #   p <- ggplot(aux, aes(x = sexo, fill = sexo, y = casos)) +
  #     geom_col() +
  #     geom_text(aes(label = casos)) +
  #     scale_fill_brewer(palette = "Dark2") +
  #     labs(x="Sexo", fill = "Sexo", y = "Casos")
  #   
  #   ggplotly(p) %>%
  #     style(textposition = "top")
  #   
  #   
  # })
  # 
  # ## Gráfico de barras casos por faixa etária
  # #
  # 
  # output$barras_faixa_etaria <- renderPlotly({
  #   
  #   aux <- dados %>%
  #     filter(data >= input$datas[1] & data <= input$datas[2]) %>%
  #     filter(fonte %in% input$fonte) %>%
  #     filter(!is.na(faixa_etaria)) %>%
  #     group_by(faixa_etaria) %>%
  #     summarise(casos = n())
  #   
  #   p <- ggplot(aux, aes(x = faixa_etaria, y = casos)) +
  #     geom_col(fill = "#1b9e77") +
  #     geom_text(aes(label = casos)) +
  #     coord_flip() +
  #     labs(x = "Faixa Etária", y = "Casos")
  #   
  #   ggplotly(p) %>%
  #     style(textposition = "middleright")
  #   
  #   
  # })
  # 
  # ## Gráfico de barras casos por unidade notificadora
  # 
  # output$barras_fonte <- renderPlotly({
  #   
  #   aux <- dados %>%
  #     filter(data >= input$datas[1] & data <= input$datas[2]) %>%
  #     filter(fonte %in% input$fonte) %>%
  #     group_by(fonte) %>%
  #     summarise(total = n()) %>%
  #     arrange(total)
  #   
  #   ordem <- aux$fonte
  #   
  #   p <- ggplot(aux, aes(x = fonte, y = total)) +
  #     geom_col(fill = "#1b9e77") +
  #     geom_text(aes(label = total)) +
  #     scale_y_continuous(limits = c(0, max(aux$total)+10))+
  #     labs(x = "Fonte Notificadora", y = "Número de casos") +
  #     scale_x_discrete(limits = ordem) +
  #     coord_flip()
  #   
  #   ggplotly(p) %>%
  #     style(textposition = "middleright")
  #   
  # })
  # 
  # ## Gráfico do número de casos por dia
  # #
  # 
  # output$barras_dia_pref <- renderPlotly({
  #   
  #   n_days <- input$datas[2]-input$datas[1]
  #   dias <- input$datas[1]+days(0:n_days)
  #   
  #   aux <- dados %>%
  #     filter(data >= input$datas[1] & data <= input$datas[2]) %>%
  #     filter(fonte %in% input$fonte) %>%
  #     group_by(data) %>%
  #     summarise(novos = n())
  #   
  #   aux2 <- tibble(data = dias[!(dias %in% aux$data)],
  #                  novos = 0)
  #   
  #   aux <- bind_rows(aux,aux2) %>%
  #     arrange(data)
  #   
  #   ordem <- as.character(format(aux$data, "%d-%m"))
  #   
  #   aux$acumulado <- c(aux$novos[1],rep(0,n_days))
  #   
  #   for (i in 2:nrow(aux)) {
  #     aux$acumulado[i] <- aux$acumulado[i-1]+aux$novos[i]
  #   }
  #   
  #   aux$novos <- as.numeric(aux$novos)
  #   
  #   aux$data <- as.character(format(aux$data, "%d-%m"))
  #   
  #   p <- ggplot(aux) +
  #     geom_line(aes(x = data, y = acumulado, group = 1)) +
  #     geom_point(aes(x = data, y = acumulado), size=2) +
  #     geom_col(aes(x = data, y = novos), fill = "#d95f02") +
  #     geom_text(aes(x = data, y = novos, label = novos)) +
  #     scale_x_discrete(limits = ordem) +
  #     labs(x = "Dia", y = "Número de casos em POA") +
  #     theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))
  #   
  #   ggplotly(p) %>%
  #     style(textposition = "top")
  #   
  # })
  # 
  # 
  # ## Gráfico do número de casos por semana epidemiológica
  # #
  # 
  # output$barras_sem_pref <- renderPlotly({
  #   
  #   n_days <- input$datas[2]-input$datas[1]
  #   dias <- input$datas[1]+days(0:n_days)
  #   
  #   aux <- dados %>%
  #     filter(data >= input$datas[1] & data <= input$datas[2]) %>%
  #     filter(fonte %in% input$fonte) %>%
  #     group_by(semana_epidemiologica) %>%
  #     summarise(novos = n())
  #   
  #   ordem <- as.character(aux$semana_epidemiologica)
  #   
  #   aux$acumulado <- c(aux$novos[1],rep(0,nrow(aux)-1))
  #   
  #   for (i in 2:nrow(aux)) {
  #     aux$acumulado[i] <- aux$acumulado[i-1]+aux$novos[i]
  #   }
  #   
  #   aux$novos <- as.numeric(aux$novos)
  #   
  #   aux$semana_epidemiologica <- as.character(aux$semana_epidemiologica)
  #   
  #   p <- ggplot(aux) +
  #     geom_line(aes(x = semana_epidemiologica, y = acumulado, group = 1)) +
  #     geom_point(aes(x = semana_epidemiologica, y = acumulado), size=2) +
  #     geom_col(aes(x = semana_epidemiologica, y = novos), fill = "#d95f02") +
  #     geom_text(aes(x = semana_epidemiologica, y = novos, label = novos)) +
  #     scale_x_discrete(limits = ordem) +
  #     labs(x = "Semana Epidemiológica", y = "Número de casos em POA") +
  #     theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))
  #   
  #   ggplotly(p) %>%
  #     style(textposition = "top")
  #   
  # })
  # 
  # ## mapa_poa
  # #
  # 
  # output$mapa_poa <- renderLeaflet({
  #   
  #   aux_mapa <- dados %>%
  #     filter(data >= input$datas[1] & data <= input$datas[2]) %>%
  #     filter(fonte %in% input$fonte) %>%
  #     group_by(fonte, lat, long) %>%
  #     summarise(casos = n())
  #   
  #   labs <- lapply(seq(nrow(aux_mapa)), function(i) {
  #     paste0("casos: ", aux_mapa[i, "casos"], '</p>', 
  #            " ",aux_mapa[i, "fonte"]) 
  #   })
  #   
  #   leaflet(aux_mapa) %>%
  #     addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
  #     addCircleMarkers(lng = aux_mapa$long, lat = aux_mapa$lat, radius = 3*sqrt(aux_mapa$casos),
  #                      color = "#1215a6", fillOpacity = 0.5, label = lapply(labs, htmltools::HTML), 
  #                      labelOptions = labelOptions(interactive = T, textsize = "15px"))
  #   
  # })
  
  ###############################
  ###### ABA LEITOS_ADULTO ######
  ###############################
  
  # caixa de operantes
  output$box_opera_adulto <- renderValueBox({
    aux <- adultos %>%
      filter(tipo %in% input$tipo_adulto) %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto)
    
    total <- sum(aux$leitos)
    
    valueBox(
      total,
      "Número de leitos operantes",
      icon = icon("bed"),
      color = "blue" 
    )
  })
  # caixa ocupados
  output$box_ocupa_adulto <- renderValueBox({
    aux <- adultos %>%
      filter(tipo %in% input$tipo_adulto) %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto)
    
    total <- sum(aux$internados)
    
    valueBox(
      total,
      "Número de leitos ocupados",
      icon = icon("procedures"),
      color = "red" 
    )
  })
  # caixa de covid confirmado
  output$box_covid_adulto <- renderValueBox({
    aux <- adultos %>%
      filter(tipo %in% input$tipo_adulto) %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto) %>%
      filter(!is.na(covid))
    
    covid <- sum(aux$covid)
    
    valueBox(
      covid,
      "Número de leitos com pacientes de covid-19",
      icon = icon("heartbeat"),
      color = "purple"
    )
  })
  # caixas de lotação
  output$box_porce_adulto <- renderValueBox({
    aux <- adultos %>%
      filter(tipo %in% input$tipo_adulto) %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto)
    
    porcentagem <- sum(aux$internados)/sum(aux$leitos)
    
    valueBox(
      paste0(round(porcentagem*100,2),"%"),
      "Porcentagem da lotação dos leitos",
      icon = icon("hospital"),
      color = "red"
    )
    
  })
  
  
  output$moinhos_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital Moinhos de Vento")
    
  })
  
  output$hcpa_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "HCPA")
    
  })
  
  output$conceicao_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital Conceição")
    
  })
  
  output$ic_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Instituto de Cardiologia")
    
  })
  
  output$santa_casa_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital Santa Casa")
    
  })
  
  output$sao_lucas_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital São Lucas")
    
  })
  
  output$restinga_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital Restinga")
    
  })
  
  output$vila_nova_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital Vila Nova")
    
  })
  
  output$pabj_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "	PA Bom Jesus")
    
  })
  
  output$pacs_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "PA Cruzeiro do Sul")
    
  })
  
  output$palp_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "PA Lomba do Pinheiro")
    
  })
  
  output$upa_zn_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "UPA Zona Norte")
    
  })
  
  output$cristo_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital Cristo Redentor")
    
  })
  
  output$ernesto_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital Ernesto Dornelles")
    
  })
  
  output$hps_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "HPS")
    
  })
  
  output$porto_alegre_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital Porto Alegre")
    
  })
  
  output$independencia_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospita Independência")
    
  })
  
  output$femina_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital Femina")
    
  })
  
  output$divina_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital Divina Providência")
    
  })
  
  output$ana_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital Santa Ana")
    
  })
  
  output$mae_adulto <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_adulto, var2 = "adulto", var3 = "Hospital Mãe de Deus")
    
  })
  
  # mapa_leitos_adulto
  
  output$mapa_leitos_adulto <- renderLeaflet({
    
    aux_mapa <- adultos %>%
      filter(tipo %in% input$tipo_adulto) %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto) %>%
      group_by(local,lat,long) %>%
      summarise(total = sum(leitos), leitos_disponiveis = sum(leitos)-sum(internados))
    
    
    labs <- lapply(seq(nrow(aux_mapa)), function(i) {
      paste0("leitos_disponiveis: ", aux_mapa[i, "leitos_disponiveis"], '</p>', 
             " ",aux_mapa[i, "local"]) 
    })
    
    leaflet(aux_mapa) %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
      addCircleMarkers(lng = aux_mapa$long, lat = aux_mapa$lat, radius = 4*sqrt(aux_mapa$leitos_disponiveis),
                       color = "deepskyblue", fillOpacity = 0.5, label = lapply(labs, htmltools::HTML), 
                       labelOptions = labelOptions(interactive = T, textsize = "15px"))
    
  })
  
  
  # barras_hosp_adulto
  
  ## Gráfico de barras leitos disponivveis por hospital
  
  output$barras_hosp_adulto <- renderPlotly({
    
    aux <- adultos %>%
      filter(tipo %in% input$tipo_adulto) %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto) %>%
      group_by(local) %>%
      summarise(leitos_disponiveis = sum(leitos)-sum(internados)) %>%
      arrange(leitos_disponiveis)
    
    ordem <- aux$local
    
    p <- ggplot(aux, aes(x = local, y = leitos_disponiveis)) +
      geom_col(fill = "#1b9e77") +
      geom_text(aes(label = leitos_disponiveis)) +
      scale_y_continuous(limits = c(0, max(aux$leitos_disponiveis)+5))+
      labs(x = "Local", y = "Número de leitos disponíveis") +
      scale_x_discrete(limits = ordem) +
      coord_flip()
    
    ggplotly(p) %>%
      style(textposition = "middleright")
    
  })
  
  ## Gráfico de barras leitos com covid-19 por hospital
  
  output$barras_hosp_covid <- renderPlotly({
    
    aux <- adultos %>%
      filter(tipo %in% input$tipo_adulto) %>%
      filter(tipo == "uti") %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto) %>%
      group_by(local) %>%
      summarise(leitos_covid = sum(covid)) %>%
      arrange(leitos_covid)
    
    ordem <- aux$local
    
    p <- ggplot(aux, aes(x = local, y = leitos_covid)) +
      geom_col(fill = "#D95F02") +
      geom_text(aes(label = leitos_covid)) +
      scale_y_continuous(limits = c(0, max(aux$leitos_covid)+5)) +
      labs(x = "Local", y = "Número de leitos com pacientes de covid-19") +
      scale_x_discrete(limits = ordem) +
      coord_flip()
    
    ggplotly(p) %>%
      style(textposition = "middleright")
    
  })
  #####################################################################################  
  # linhas_serie_adulto
  
  output$linhas_serie_adulto <- renderPlotly({
    
    aux <- adultos %>%
      filter(tipo %in% input$tipo_adulto) %>%
      filter(local %in% input$local_adulto) %>%
      group_by(data_atualizacao, tipo) %>%
      summarise(porcentagem_media = 100*(sum(internados)/sum(leitos)), total_leitos = sum(leitos),
                total_ocupados = sum(internados), total_disponiveis = sum(leitos)-sum(internados)) %>%
      arrange(data_atualizacao)
    
    ordem <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    aux$data_atualizacao <- as.character(format(aux$data_atualizacao, "%d-%m"))
    cores_uti_emergencia = c("emergencia" = "#1B9E77", "uti" = "#D95F02")
    
    
    p <- ggplot(aux) + 
      geom_point(aes(x = data_atualizacao,y = total_leitos , color = tipo),size = 2) +
      geom_line(aes(x = data_atualizacao, y = total_leitos, group = tipo, color = tipo)) +
      geom_point(aes(x = data_atualizacao,y = total_disponiveis , color = tipo),size = 2) +
      geom_line(aes(x = data_atualizacao, y = total_disponiveis, group = tipo, color = tipo)) +
      scale_x_discrete(limits = ordem) +
      labs(x = "Dia", y = "Número de leitos") +
      scale_fill_manual(values = cores_uti_emergencia) +
      scale_color_manual(values = cores_uti_emergencia) +
      theme(axis.text.x = element_text(angle=90,size=8, vjust = 0.5))
    
    my_plot <- ggplotly(p)
    
    for (i in 1:length(my_plot$x$data)){
      if (!is.null(my_plot$x$data[[i]]$name)){
        my_plot$x$data[[i]]$name =  gsub("\\(","",str_split(my_plot$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    my_plot
    
  })
  ###############
  output$linhas_serie_adulto2 <- renderPlotly({
    
    aux <- adultos %>%
      filter(tipo %in% input$tipo_adulto) %>%
      filter(local %in% input$local_adulto) %>%
      group_by(data_atualizacao, tipo) %>%
      summarise(porcentagem_media = 100*(sum(internados)/sum(leitos)), total_leitos = sum(leitos), total_ocupados = sum(internados), 
                total_disponiveis = sum(leitos)-sum(internados)) %>%
      arrange(data_atualizacao)
    
    ordem <- as.character(format(aux$data_atualizacao, "%d-%m"))
    aux$data_atualizacao <- as.character(format(aux$data_atualizacao, "%d-%m"))
    cores_uti_emergencia = c("emergencia" = "#1B9E77", "uti" = "#D95F02")
    
    p <- ggplot(aux, aes(x = data_atualizacao)) +
      geom_point(aes(y = total_leitos, color = tipo), size = 2) +
      geom_line(aes(y = total_leitos, color = tipo, group = tipo)) +
      geom_point(aes(y = total_ocupados, color = tipo), size = 2) +
      geom_line(aes(y = total_ocupados, color = tipo, group = tipo)) +
      scale_fill_manual(values = cores_uti_emergencia) +
      scale_color_manual(values = cores_uti_emergencia) +
      scale_x_discrete(limits = ordem) +
      #scale_fill_brewer(palette = "Dark2") +
      #scale_color_brewer(palette = "Dark2") +
      labs(x = "Dia", y = "Número de leitos") +
      theme(axis.text.x = element_text(angle=90,size=8, vjust = 0.5))
    
    my_plot <- ggplotly(p)
    
    for (i in 1:length(my_plot$x$data)){
      if (!is.null(my_plot$x$data[[i]]$name)){
        my_plot$x$data[[i]]$name =  gsub("\\(","",str_split(my_plot$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    my_plot
    
  })
  
  #####################################
  # barras covid adulto
  
  output$barras_covid_adulto <- renderPlotly({
    
    aux <- adultos %>%
      filter(tipo %in% input$tipo_adulto) %>%
      filter(local %in% input$local_adulto) %>%
      group_by(data_atualizacao) %>%
      summarise(leitos_covid = sum(na.rm=T, covid)) %>%
      arrange(data_atualizacao)
    
    ordem <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    aux$data_atualizacao <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    p <- ggplot(aux, aes(x = data_atualizacao)) +
      geom_col(aes(y = leitos_covid), fill = "#D95F02") +
      scale_x_discrete(limits = ordem) +
      scale_y_continuous(limits = c(0, max(aux$leitos_covid)+5))+
      labs(x = "Dia", y = "Número de leitos com pacientes de covid-19") +
      theme(axis.text.x = element_text(angle=90,size=8, vjust = 0.5))
    
    ggplotly(p)
    
    
  })
  
  ############################################################################# 
  
  
  
  ####################
  
  ###############################
  ###### ABA LEITOS_PEDIA ######
  ###############################
  
  # caixa de operantes
  output$box_opera_pedia <- renderValueBox({
    aux <- pediatricos %>%
      filter(tipo %in% input$tipo_pedia) %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto)
    
    total <- sum(aux$leitos)
    
    valueBox(
      total,
      "Número de leitos operantes",
      icon = icon("bed"),
      color = "blue" 
    )
  })
  # caixa ocupados
  output$box_ocupa_pedia <- renderValueBox({
    aux <- pediatricos %>%
      filter(tipo %in% input$tipo_pedia) %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto)
    
    total <- sum(aux$internados)
    
    valueBox(
      total,
      "Número de leitos ocupados",
      icon = icon("procedures"),
      color = "red" 
    )
  })
  # caixa de covid confirmado
  output$box_covid_pedia <- renderValueBox({
    aux <- pediatricos %>%
      filter(tipo %in% input$tipo_pedia) %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto) %>%
      filter(!is.na(covid))
    
    covid <- sum(aux$covid)
    
    valueBox(
      covid,
      "Número de leitos com pacientes de covid-19",
      icon = icon("heartbeat"),
      color = "purple"
    )
  })
  # caixas de lotação
  output$box_porce_pedia <- renderValueBox({
    aux <- pediatricos %>%
      filter(tipo %in% input$tipo_pedia) %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto)
    
    porcentagem <- sum(aux$internados)/sum(aux$leitos)
    
    valueBox(
      paste0(round(porcentagem*100,2),"%"),
      "Porcentagem da lotação dos leitos",
      icon = icon("hospital"),
      color = "red"
    )
    
  })
  
  
  output$moinhos_pedia <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_pedia, var2 = "pediatrico", var3 = "Hospital Moinhos de Vento")
    
  })
  
  output$hcpa_pedia <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_pedia, var2 = "pediatrico", var3 = "HCPA")
    
  })
  
  output$conceicao_pedia <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_pedia, var2 = "pediatrico", var3 = "Hospital Conceição")
    
  })
  
  output$santa_casa_pedia <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_pedia, var2 = "pediatrico", var3 = "Hospital Santa Casa")
    
  })
  
  #output$sao_lucas_pedia <- renderUI({
  #  
  #  caixinha_hospital(var1 = input$tipo_pedia, var2 = "pediatrico", var3 = "Hospital São Lucas")
  #  
  #})
  
  output$restinga_pedia <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_pedia, var2 = "pediatrico", var3 = "Hospital Restinga")
    
  })
  
  output$pabj_pedia <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_pedia, var2 = "pediatrico", var3 = "PA Bom Jesus")
    
  })
  
  output$pacs_pedia <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_pedia, var2 = "pediatrico", var3 = "PA Cruzeiro do Sul")
    
  })
  
  output$palp_pedia <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_pedia, var2 = "pediatrico", var3 = "PA Lomba do Pinheiro")
    
  })
  
  output$hps_pedia <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_pedia, var2 = "pediatrico", var3 = "HPS")
    
  })
  
  output$mipv_pedia <- renderUI({
    
    caixinha_hospital(var1 = input$tipo_pedia, var2 = "pediatrico", var3 = "HMIPV")
    
  })
  
  
  # mapa_leitos_pedia
  
  output$mapa_leitos_pedia <- renderLeaflet({
    
    aux_mapa <- pediatricos %>%
      filter(tipo %in% input$tipo_pedia) %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto) %>%
      group_by(local,lat,long) %>%
      summarise(total = sum(leitos), leitos_disponiveis = sum(leitos)-sum(internados))
    
    
    labs <- lapply(seq(nrow(aux_mapa)), function(i) {
      paste0("leitos_disponiveis: ", aux_mapa[i, "leitos_disponiveis"], '</p>', 
             " ",aux_mapa[i, "local"]) 
    })
    
    leaflet(aux_mapa) %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
      addCircleMarkers(lng = aux_mapa$long, lat = aux_mapa$lat, radius = 4*sqrt(aux_mapa$leitos_disponiveis),
                       color = "deepskyblue", fillOpacity = 0.5, label = lapply(labs, htmltools::HTML), 
                       labelOptions = labelOptions(interactive = T, textsize = "15px"))
    
  })
  
  
  # barras_hosp_pedia
  
  ## Gráfico de barras casos por unidade notificadora
  
  output$barras_hosp_pedia <- renderPlotly({
    
    aux <- pediatricos %>%
      filter(tipo %in% input$tipo_pedia) %>%
      filter(data_atualizacao %in% input$data_adulto) %>%
      filter(local %in% input$local_adulto) %>%
      group_by(local) %>%
      summarise(leitos_disponiveis = sum(leitos)-sum(internados)) %>%
      arrange(leitos_disponiveis)
    
    ordem <- aux$local
    
    p <- ggplot(aux, aes(x = local, y = leitos_disponiveis)) +
      geom_col(fill = "#1b9e77") +
      geom_text(aes(label = leitos_disponiveis)) +
      scale_y_continuous(limits = c(0, max(aux$leitos_disponiveis)+5))+
      labs(x = "Local", y = "Número de leitos disponíveis") +
      scale_x_discrete(limits = ordem) +
      coord_flip()
    
    ggplotly(p) %>%
      style(textposition = "middleright")
    
  })
  
  # linhas_serie_pedia
  
  output$linhas_serie_pedia <- renderPlotly({
    
    aux <- pediatricos %>%
      filter(tipo %in% input$tipo_pedia) %>%
      filter(local %in% input$local_adulto) %>%
      group_by(data_atualizacao, tipo) %>%
      summarise(porcentagem_media = 100*(sum(internados)/sum(leitos)), total_leitos = sum(leitos),
                total_ocupados = sum(internados), total_disponiveis = sum(leitos)-sum(internados)) %>%
      arrange(data_atualizacao)
    
    ordem <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    aux$data_atualizacao <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    p <- ggplot(aux) + 
      geom_point(aes(x = data_atualizacao,y = total_leitos , color = tipo),size = 2) +
      geom_line(aes(x = data_atualizacao, y = total_leitos, group = tipo, color = tipo)) +
      geom_point(aes(x = data_atualizacao,y = total_disponiveis , color = tipo),size = 2) +
      geom_line(aes(x = data_atualizacao, y = total_disponiveis, group = tipo, color = tipo)) +
      scale_x_discrete(limits = ordem) +
      labs(x = "Dia", y = "Número de leitos") +
      scale_fill_brewer(palette = "Dark2") +
      scale_color_brewer(palette = "Dark2") +
      theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))
    
    my_plot <- ggplotly(p)
    
    for (i in 1:length(my_plot$x$data)){
      if (!is.null(my_plot$x$data[[i]]$name)){
        my_plot$x$data[[i]]$name =  gsub("\\(","",str_split(my_plot$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    my_plot
  })
  ###############
  output$linhas_serie_pedia2 <- renderPlotly({
    
    aux <- pediatricos %>%
      filter(tipo %in% input$tipo_pedia) %>%
      filter(local %in% input$local_adulto) %>%
      group_by(data_atualizacao, tipo) %>%
      summarise(porcentagem_media = 100*(sum(internados)/sum(leitos)), total_leitos = sum(leitos), total_ocupados = sum(internados), 
                total_disponiveis = sum(leitos)-sum(internados)) %>%
      arrange(data_atualizacao)
    
    ordem <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    aux$data_atualizacao <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    p <- ggplot(aux, aes(x = data_atualizacao)) +
      geom_point(aes(y = total_leitos, color = tipo), size = 2) +
      geom_line(aes(y = total_leitos, color = tipo, group = tipo)) +
      geom_point(aes(y = total_ocupados, color = tipo), size = 2) +
      geom_line(aes(y = total_ocupados, color = tipo, group = tipo)) +
      scale_x_discrete(limits = ordem) +
      scale_fill_brewer(palette = "Dark2") +
      scale_color_brewer(palette = "Dark2") +
      labs(x = "Dia", y = "Número de leitos") +
      theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))
    
    my_plot <- ggplotly(p)
    
    for (i in 1:length(my_plot$x$data)){
      if (!is.null(my_plot$x$data[[i]]$name)){
        my_plot$x$data[[i]]$name =  gsub("\\(","",str_split(my_plot$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    my_plot
  })
  
  ###############################
  ###### ABA CovidMetrika  ######
  ###############################
  
  output$dados_covid <- renderValueBox({
    
    valueBox(
      value = "COVID-19",
      subtitle = "Secretaria Estadual de Saúde do RS",
      icon = icon("viruses"),
      color = "aqua",
      href = "http://ti.saude.rs.gov.br/covid19/",
      width = 12
    )
    
  })
  
  output$dados_uti <- renderValueBox({
    
    valueBox(
      value = "UTI",
      subtitle = "Prefeitura de Porto Alegre",
      icon = icon("hospital"),
      color = "aqua",
      href = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTwlPdNvBfVAYGVXKzmWxWLpQLuPwf28zVY3PhqBw5qZ6D4sppyXj5IIslEOUfBBipAyqnGTUHX-IRV/pubhtml",
      width = 12
    )
    
  })
  
  output$dados_emergencias <- renderValueBox({
    
    valueBox(
      value = "Emergências",
      subtitle = "Prefeitura de Porto Alegre",
      icon = icon("hospital"),
      color = "aqua",
      href = "https://docs.google.com/spreadsheets/d/1-Zea1tEJd-rJJp77Veptkrone0_hddVKgy-pW58E5iM/pubhtml#",
      width = 12
    )
    
  })
  
  output$licenca <- renderValueBox({
    
    valueBox("Licença:", 
             subtitle = "(CC BY-SA 4.0)", 
             icon = icon("creative-commons"), 
             color = "aqua", 
             width = 12,
             href = "https://creativecommons.org/licenses/by-sa/4.0/deed.en"
    )
    
  })
  
  output$covidMetrika <- renderValueBox({
    
    valueBox("Site covidMetrika", 
             subtitle = div("Aplicativo desenvolvido pelo grupo covidMetrika",br(),"Confira aqui nosso site para ver nossos outros projetos!"), 
             icon = icon("external-link-alt"), 
             color = "yellow", 
             width = 12,
             href = "https://www.ufrgs.br/covidmetrika/"
    )
    
  })
  
  output$git_covidMetrika <- renderValueBox({
    
    valueBox("Repositório covidMetrika", 
             subtitle = div("Confira aqui nosso repositório no GitHub!",br(),"Contato: covidmetrika@gmail.com"), 
             icon = icon("github"), 
             color = "yellow", 
             width = 12,
             href = "https://github.com/CovidMetrika/dashboard_poa"
    )
    
  })
  
  output$dashboard_poa <- renderValueBox({
    
    valueBox("Dashboard RS", 
             subtitle = div("Confira aqui o nosso outro painel de dados com foco no Rio Grande do Sul, informando os casos de COVID-19 e a situação dos leitos de UTI"), 
             icon = icon("columns"), 
             color = "yellow", 
             width = 12,
             href = "https://mhbarbian.shinyapps.io/covid19_rs/"
    )
    
  })
  
  output$dashboard_br <- renderValueBox({
    
    valueBox("Dashboard Brasil", 
             subtitle = div("Confira aqui o nosso outro painel de dados com foco no Brasil e seus estados"), 
             icon = icon("columns"), 
             color = "yellow", 
             width = 12,
             href = "https://mhbarbian.shinyapps.io/CovidMetrika/"
    )
    
  })
  
}