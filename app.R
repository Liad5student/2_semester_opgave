#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Loader pakker til Shiny appen
pacman::p_load(
  shiny, leaflet, dplyr, readr, shinyWidgets, DT, ggplot2
)

# Indlæser datasæt
full_results <- readRDS("data/full_results.rds")

# Postnummer koordinater
postal_coords <- data.frame(
  PostalCode = c(8800, 8850, 8830, 7470, 8840, 7800, 8831, 8832, 9632, 7850, 9620, 9500, 8860),
  lat = c(56.451, 56.532, 56.447, 56.489, 56.472, 56.475, 56.448, 56.449, 56.907, 56.573, 56.824, 57.226, 56.611),
  lng = c(9.404, 8.486, 9.186, 9.000, 8.662, 9.156, 9.163, 9.171, 9.287, 9.156, 9.388, 9.388, 8.954)
)

# Klargør data
full_results$PostalCode <- as.character(full_results$PostalCode)
postal_coords$PostalCode <- as.character(postal_coords$PostalCode)

data_map <- full_results %>%
  left_join(postal_coords, by = "PostalCode") %>%
  mutate(
    risk_category = case_when(
      churn_prob > 0.75 ~ "High",
      churn_prob > 0.5 ~ "Medium",
      TRUE ~ "Low"
    ),
    risk_category = factor(risk_category, levels = c("High", "Medium", "Low"))
  )

# UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400;600;700&display=swap"),
    tags$style(HTML("
      body {
        background: linear-gradient(to bottom, #0b2e2a 0%, #12483e 40%, #1a5d4b 70%, #2f7561 100%);
        font-family: 'Open Sans', sans-serif;
        color: #ffffff;
      }
      h1, h2, h3, h4, h5 {
        font-weight: 600;
        color: #90c73e;
      }
      .container-fluid, .main-panel, .well, .panel, .tab-pane {
        background-color: transparent;
        padding: 20px;
      }
      .sidebarPanel {
        background-color: #103a33;
        padding: 20px;
        border-radius: 10px;
        border-left: 5px solid #90c73e;
      }
      .info-box { 
        background-color: #103a33; 
        border-left: 5px solid #90c73e; 
        padding: 15px;
        margin-bottom: 15px;
        color: #ffffff;
      }
      .btn, .btn-primary, .btn-warning, .btn-danger {
        background-color: #90c73e !important;
        color: #0b2e2a !important;
        font-weight: 600;
        border: none;
        border-radius: 4px;
      }
      .btn:hover {
        background-color: #a7db50 !important;
      }
      .radioButtons .btn, .checkboxGroupButtons .btn {
        background-color: #18453e !important;
        color: #ffffff !important;
        border: 1px solid #90c73e;
      }
      .radioButtons .active, .checkboxGroupButtons .active {
        background-color: #90c73e !important;
        color: #0b2e2a !important;
      }
      .tabbable > .nav > li[class=active] > a {
        background-color: #90c73e !important;
        color: #0b2e2a !important;
      }
      .dataTables_wrapper .dataTables_length, 
      .dataTables_wrapper .dataTables_filter, 
      .dataTables_wrapper .dataTables_info, 
      .dataTables_wrapper .dataTables_paginate {
        font-size: 90%;
      }
      table.dataTable th, table.dataTable td {
        white-space: nowrap;
        padding: 4px 10px;
        font-size: 90%;
      }
      .risk-high { color: #e74c3c; font-weight: bold; }
      .risk-medium { color: #f39c12; }
      .risk-low { color: #2ecc71; }
    "))
  ),
  
  div(style = "display: flex; align-items: center; justify-content: space-between; padding: 10px 30px;",
      img(src = "businessviborgny.jpeg", height = "50px"),
      h2("Medlemsanalyse – Business Viborg")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      class = "sidebarPanel",
      
      div(class = "info-box",
          h4("Guide"),
          p("Udforsk medlemmernes churn-risiko efter lokation. Filtrer og analyser data med kontrollerne.")
      ),
      
      radioGroupButtons(
        inputId = "view_by",
        label = "Visningstilstand:",
        choices = c("Postnummer" = "PostalCode", "Medlemsnummer" = "PNumber", "Alle medlemmer" = "all"),
        selected = "all",
        status = "primary"
      ),
      
      conditionalPanel(
        condition = "input.view_by == 'PostalCode'",
        pickerInput(
          inputId = "postal_code",
          label = "Vælg postnummer:",
          choices = unique(data_map$PostalCode),
          multiple = TRUE,
          options = list(`actionsBox` = TRUE)
        )
      ),
      
      conditionalPanel(
        condition = "input.view_by == 'PNumber'",
        selectizeInput(
          inputId = "member_id",
          label = "Vælg medlemsnummer:",
          choices = NULL,
          multiple = FALSE,
          options = list(
            placeholder = 'Skriv for at søge',
            onInitialize = I('function() { this.setValue(\"\"); }')
          )
        )
      ),
      
      sliderInput(
        inputId = "churn_range",
        label = "Churn sandsynlighed:",
        min = 0,
        max = 1,
        value = c(0, 1),
        step = 0.01
      ),
      
      checkboxGroupButtons(
        inputId = "risk_categories",
        label = "Risiko-kategorier:",
        choices = c("High", "Medium", "Low"),
        selected = c("High", "Medium", "Low"),
        status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      ),
      
      actionBttn(
        inputId = "reset_filters",
        label = "Nulstil filtre",
        style = "material-flat",
        color = "warning"
      ),
      
      actionBttn(
        inputId = "show_top10",
        label = "Vis Top 10 Churn",
        style = "material-flat",
        color = "danger"
      ),
      
      br(), br(),
      div(class = "info-box",
          h5("Business Viborg"),
          p("Erik Ejegods Vej 16, 2. sal\n8800 Viborg\n+45 87 25 51 51\ninfo@buvi.dk")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Kortvisning", leafletOutput("map", height = "700px")),
        tabPanel("Data", DTOutput("data_table"), downloadButton("download_data", "Download data")),
        tabPanel("Statistik",
                 h4("Fordeling af churn-risiko"), plotOutput("risk_distribution", height = "300px"),
                 h4("Risiko efter postnummer"), plotOutput("postal_code_summary", height = "300px")
        ),
        tabPanel("Brancher",
                 h4("Top 5 brancher med højest churn"),
                 plotOutput("top_branches", height = "400px")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  updateSelectizeInput(session, "member_id", choices = unique(data_map$PNumber), server = TRUE)
  filtered_data <- reactive({
    data <- data_map %>%
      filter(churn_prob >= input$churn_range[1], churn_prob <= input$churn_range[2]) %>%
      filter(risk_category %in% input$risk_categories)
    if (input$view_by == "PostalCode" && !is.null(input$postal_code)) {
      data <- data %>% filter(PostalCode %in% input$postal_code)
    } else if (input$view_by == "PNumber" && !is.null(input$member_id)) {
      data <- data %>% filter(PNumber == input$member_id)
    }
    data
  })
  observeEvent(input$reset_filters, {
    updateSliderInput(session, "churn_range", value = c(0, 1))
    updateCheckboxGroupButtons(session, "risk_categories", selected = c("High", "Medium", "Low"))
    updateRadioGroupButtons(session, "view_by", selected = "all")
    updatePickerInput(session, "postal_code", selected = character(0))
    updateSelectizeInput(session, "member_id", selected = "")
  })
  observeEvent(input$show_top10, {
    showModal(modalDialog(
      title = "Top 10 medlemmer med højest churn-risiko",
      p("Denne tabel viser de 10 medlemmer med størst risiko for at forlade Business Viborg."),
      DTOutput("top_members_modal"),
      easyClose = TRUE,
      footer = modalButton("Luk")
    ))
  })
  output$top_members_modal <- renderDT({
    filtered_data() %>%
      arrange(desc(churn_prob)) %>%
      slice_head(n = 10) %>%
      dplyr::mutate(churn_prob = churn_prob * 100) %>%
      dplyr::select(PNumber, PostalCode, churn_prob, risk_category) %>%
      datatable(
        rownames = FALSE,
        colnames = c('Member ID', 'Postal Code', 'Churn Probability (%)', 'Risk Category'),
        options = list(pageLength = 10, scrollX = FALSE, autoWidth = TRUE),
        class = 'compact'
      ) %>%
      formatRound('churn_prob', 1) %>%
      formatStyle(
        'risk_category',
        backgroundColor = styleEqual(
          c("High", "Medium", "Low"),
          c("#e74c3c", "#f39c12", "#2ecc71")
        )
      )
  })
  # Kort og data/plots - beholdt uændret
  risk_pal <- colorFactor(palette = c("#e74c3c", "#f39c12", "#2ecc71"), levels = c("High", "Medium", "Low"))
  output$map <- renderLeaflet({
    df <- filtered_data()
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = ~churn_prob * input$marker_size,
        color = ~risk_pal(risk_category),
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        label = ~paste0(
          "Member ID: ", PNumber, "<br>",
          "Postal Code: ", PostalCode, "<br>",
          "Churn Risk: ", round(churn_prob * 100, 1), "%<br>",
          "Category: ", risk_category
        ),
        clusterOptions = if(nrow(df) > 100) markerClusterOptions() else NULL
      ) %>%
      addLegend(position = "bottomright", pal = risk_pal, values = ~risk_category, title = "Churn Risk", opacity = 1)
  })
  output$data_table <- renderDT({
    datatable(
      filtered_data() %>%
        dplyr::mutate(churn_prob = churn_prob * 100) %>%
        dplyr::select(PNumber, PostalCode, churn_prob, risk_category, lat, lng),
      rownames = FALSE,
      colnames = c('Member ID', 'Postal Code', 'Churn Probability', 'Risk Category', 'Latitude', 'Longitude'),
      filter = 'top',
      options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
      extensions = 'Buttons'
    ) %>%
      formatRound('churn_prob', 1) %>%
      formatStyle('churn_prob', textAlign = 'right', color = 'black') %>%
      formatStyle('risk_category', backgroundColor = styleEqual(c("High", "Medium", "Low"), c("#e74c3c", "#f39c12", "#2ecc71")))
  })
  output$download_data <- downloadHandler(
    filename = function() { paste("churn_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )
  output$risk_distribution <- renderPlot({
    df <- filtered_data() %>% mutate(churn_prob = churn_prob * 100)
    ggplot(df, aes(x = churn_prob, fill = risk_category)) +
      geom_histogram(binwidth = 5, color = "white") +
      scale_fill_manual(values = c("#e74c3c", "#f39c12", "#2ecc71")) +
      labs(title = "Distribution of Churn Probabilities", x = "Churn Probability (%)", y = "Count") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  output$postal_code_summary <- renderPlot({
    df <- filtered_data()
    if(nrow(df) > 0) {
      df %>%
        group_by(PostalCode) %>%
        summarise(avg_churn = mean(churn_prob) * 100, count = n()) %>%
        ggplot(aes(x = reorder(PostalCode, avg_churn), y = avg_churn, fill = count)) +
        geom_col() +
        coord_flip() +
        scale_fill_gradient(low = "#d6eaf8", high = "#3498db") +
        labs(title = "Average Churn Risk by Postal Code", x = "Postal Code", y = "Average Churn Probability (%)") +
        theme_minimal()
    }
  })
  output$top_branches <- renderPlot({
    full_results %>%
      group_by(Branche_navn) %>%
      summarise(gennemsnitlig_churn = mean(churn_prob) * 100, n = n()) %>%
      arrange(desc(gennemsnitlig_churn)) %>%
      slice_head(n = 5) %>%
      ggplot(aes(x = reorder(Branche_navn, gennemsnitlig_churn), y = gennemsnitlig_churn)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 5 brancher med højest churn-risiko", x = "Branche", y = "Gns. churn sandsynlighed (%)") +
      theme_minimal()
  })
}

# Server og app start
shinyApp(ui, server)