library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(shinyWidgets)
library(DT)
library(ggplot2)

# Data
full_results <- readRDS("data/full_results.rds")

postal_coords <- data.frame(
  PostalCode = c(8800, 8850, 8830, 7470, 8840, 7800, 8831, 8832, 9632, 7850, 9620, 9500, 8860),
  lat = c(56.451, 56.532, 56.447, 56.489, 56.472, 56.475, 56.448, 56.449, 56.907, 56.573, 56.824, 57.226, 56.611),
  lng = c(9.404, 8.486, 9.186, 9.000, 8.662, 9.156, 9.163, 9.171, 9.287, 9.156, 9.388, 9.388, 8.954)
)

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
    tags$style(HTML("
      body {
        background: #ffffff;
        font-family: 'Open Sans', sans-serif;
      }
      .top-bar {
        background: #ffffff;
        padding: 15px 30px;
        display: flex;
        align-items: center;
        justify-content: space-between;
        border-bottom: 2px solid #eeeeee;
      }
      .header-banner {
        background: linear-gradient(to bottom, #0e2f33 0%, #2a6c73 100%);
        padding: 30px;
        color: #ffffff;
        text-align: center;
        margin-bottom: 20px;
      }
      .sidebarPanel {
        background-color: rgba(16, 58, 51, 0.1);
        padding: 20px;
        border-radius: 10px;
        border-left: 5px solid #d0e647;
      }
      .btn, .btn-primary, .btn-warning, .btn-danger {
        background-color: #d0e647 !important;
        color: #0b2e2a !important;
        font-weight: 600;
        border: none;
        border-radius: 4px;
      }
      .btn:hover {
        background-color: #e1f96e !important;
      }
      .leaflet-container {
        width: 70% !important;
        margin-left: 0 !important;
      }
      .tab-content {
        display: flex;
        justify-content: flex-start;
      }
    "))
  ),
  
  div(class = "top-bar",
      img(src = "businessviborgny.jpeg", height = "50px"),
      h2("Churnanalyse – Business Viborg")
  ),
  
  div(class = "header-banner",
      h3("Velkommen til churn-analysen af medlemmer")
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
          choices = unique(data_map$PNumber),
          multiple = FALSE,
          options = list(placeholder = 'Skriv for at søge')
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
        status = "primary"
      ),
      
      actionBttn("reset_filters", "Nulstil filtre", style = "material-flat", color = "warning"),
      actionBttn("show_top10", "Vis Top 10 Churn", style = "material-flat", color = "danger")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Kortvisning", leafletOutput("map", height = "400px")),
        tabPanel("Data", DTOutput("data_table"), downloadButton("download_data", "Download data")),
        tabPanel("Statistik",
                 plotOutput("risk_distribution", height = "300px"),
                 plotOutput("postal_code_summary", height = "300px")
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
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
  
  output$map <- renderLeaflet({
    df <- filtered_data()
    risk_pal <- colorFactor(palette = c("#e74c3c", "#f39c12", "#2ecc71"), levels = c("High", "Medium", "Low"))
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = ~churn_prob * 10,
        color = ~risk_pal(risk_category),
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        label = ~paste0(
          "Member ID: ", PNumber, "<br>",
          "Postal Code: ", PostalCode, "<br>",
          "Churn Risk: ", round(churn_prob * 100, 1), "%<br>",
          "Category: ", risk_category
        )
      ) %>%
      addLegend(position = "bottomright", pal = risk_pal, values = ~risk_category, title = "Churn Risk", opacity = 1)
  })
  
  output$data_table <- renderDT({
    datatable(filtered_data())
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("churn_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )
  
  output$risk_distribution <- renderPlot({
    df <- filtered_data()
    ggplot(df, aes(x = churn_prob, fill = risk_category)) +
      geom_histogram(binwidth = 0.1) +
      theme_minimal()
  })
  
  output$postal_code_summary <- renderPlot({
    df <- filtered_data()
    df %>%
      group_by(PostalCode) %>%
      summarise(avg_churn = mean(churn_prob), n = n()) %>%
      ggplot(aes(x = reorder(PostalCode, avg_churn), y = avg_churn)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal()
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
      DTOutput("top_members_modal"),
      easyClose = TRUE,
      footer = modalButton("Luk")
    ))
  })
  
  output$top_members_modal <- renderDT({
    filtered_data() %>%
      arrange(desc(churn_prob)) %>%
      slice_head(n = 10) %>%
      datatable()
  })
}

# KØR APP
shinyApp(ui, server)
