

# 1. FORBEREDELSE
#   1.1 Indlæsning af pakker
#   1.2 Indlæsning og behandling af data

# 2. BRUGERFLADE (UI)
#   2.1 UI layout og styling
#   2.2 sidebarPanel – kontrolpanel (filtre, inputs)
#   2.3 mainPanel – kort, data og statistik

# 3. SERVER-LOGIK
#   3.1 Reaktiv filtrering af data
#   3.2 Kortvisning med Leaflet
#   3.3 Datatabel og download-funktion
#   3.4 Plot: Risiko-fordeling (histogram)
#   3.5 Plot: Opsummering per postnummer
#   3.6 Brugerinteraktion: Nulstil filtre og vis Top 10

# 4. KØRSEL AF APP
#   4.1 Initialisering med shinyApp(ui, server)

#-------------------------------------------------------------------------------
# 1.1 Indlæsning af pakker
#-------------------------------------------------------------------------------
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(shinyWidgets)
library(DT)
library(ggplot2)

#-------------------------------------------------------------------------------
# 1.2 Indlæsning og behandling af data
#-------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------
# 2.1 UI layout og styling
#-------------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: #ffffff;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
        zoom: 0.8;

      }
      .top-bar {
        background: #ffffff;
        font-family: 'Open Sans', sans-serif;
        padding: 15px 30px;
        display: flex;
        align-items: center;
        justify-content: space-between;
        border-bottom: 2px solid #eeeeee;
      }
      .header-banner {
        background: linear-gradient(to bottom, #0e2f33 0%, #2a6c73 100%);
        padding: 10px 2px;
        color: #ffffff;
        text-align: center;
        margin-bottom: 2px;

      }
      
      .small-paragraph {
        font-size: 16px;  /* eller fx 14px */
        margin-top: 10px;
      }
      .sidebarPanel {
        background-color: #f5f5f5;
        padding: 30px;
        border-radius: 10px;
        border-left: 10px solid #2a6c73;
      }
      .btn, .btn-primary, .btn-warning, .btn-danger {
      background-color: #0e2f33!important;
      color: #ffffff !important;
      font-weight: 500;
      border: none;
      border-radius: 5px;
      padding: 6px 12px;
      }
     .btn:hover {
      background-color: #0e2f33 !important;
      }

   
      .btn-group .btn {
      background-color: #e0e0e0 !important;
      color: #333 !important;
      border-radius: 5px !important;
      }
     .btn-group .btn.active, 
     .btn-group .btn:active, 
     .btn-group .btn:focus {
      background-color: #0e2f33  !important;
      color: #fff !important;
      font-weight: 600;
      }

      }
      .leaflet-container {
        width: 70% !important;
        margin-left: 0 !important;
        
      }
      .tab-content {
        display: flex;
        justify-content: flex-start;
      }
      
     .nav-tabs {
      background-color: transparent !important;
      border: none !important;
      margin: 0;
      padding-left: 30px;
      }


     .nav-tabs > li > a {
     color: white !important;
     font-weight: 500;
     border: none !important;
      }

    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
     background-color: transparent !important;
     border: none !important;
     border-bottom: 3px solid #7FC8A3 !important;
     color: #ffffff !important;
     font-weight: bold;
      }
     
     .bootstrap-select .dropdown-toggle {
      background-color: #0e2f33 !important;     /* mørkeblå baggrund */
      color: #ffffff !important;                /* hvid tekst */
      border-color: #0e2f33 !important;         /* kantfarve */
      font-weight: 500;
      border-radius: 5px;
      padding: 6px 12px;
      }

     .bootstrap-select .dropdown-toggle:focus {
      border-color: #0e2f33 !important;
      box-shadow: 0 0 0 0.15rem rgba(26, 78, 99, 0.5) !important;
      outline: none !important;
      }


    .bootstrap-select .dropdown-toggle:hover {
     background-color: #0e2f33 !important;
      }
     
     .irs-bar,
     .irs-bar-edge,
     .irs-single,
     .irs-from,
     .irs-to {
     background-color: #0e2f33 !important;
     border-color: #1f3e47 !important;
     }
     
   .custom-btn {
    background-color: #0e2f33  !important;
    color: #7FC8A3 !important;
    font-weight: bold;
    border: none;
    border-radius: 5px;
    padding: 10px 16px;
    margin-top: 10px;
    display: block;
    width: 100%;
    text-align: center;

   }

    .info-box-container {
    display: flex;
    flex-wrap: wrap;
    gap: 15px;
    margin-bottom: 20px;
   }

    .info-box {
    flex: 1 1 200px;
    background-color: #0e2f33;
    color: #00bfff;
    border-radius: 10px;
    padding: 20px;
    text-align: center;
    box-shadow: 0 2px 5px rgba(0,0,0,0.2);
   }

   .info-box h4 {
    font-size: 13px;
    color: #ffffff;
    font-weight: normal;
    margin-bottom: 6px;
   }

   .info-box p {
   font-size: 18px;
   font-weight: normal;
   margin: 0;
   color: #7FC8A3;
   }





    "))
  ),
  
  div(class =  "top-bar",
      style = "display: flex; align-items: center; justify-content: space-between; padding: 20px 30px;",
      
      # Venstre: logo
      div(style = "display: flex; align-items: center; gap: 15px;",
          img(src = "businessviborgny.jpeg", height = "60px")
      ),
      
      # Center: overskrift
      div(style = "text-align: center; flex-grow: 1;",
          h2("Velkommen til medlemsindsigt", style = "font-size: 30px; margin: 0; color: #0e2f33;"),
          p("Overvåg og analyser churn risiko blandt medlemmer",
            style = "color: #88c5aa; font-size: 18px; margin: 0;")
      ),
      
      # Højre: søg og profil
      div(style = "display: flex; align-items: center; gap: 10px;",
          textInput("search", NULL, placeholder = "Søg...", width = "200px"),
          tags$div(style = "
            width: 40px; height: 40px;
            background-color: #0e2f33;
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            color: white;
            font-size: 18px;
        ",
                   icon("user"))
      )
  ),
  
  
  div(class = "header-banner", 
      
      div(class = "header-banner", 
          tabsetPanel(
            type = "tabs",
            id = "main_tabs",
            
            tabPanel(title = tagList(icon("tachometer-alt"), strong("Dashboard")), value = "dashboard"),
            tabPanel(title = tagList(icon("map"), "Map"), value = "map"),
            tabPanel(title = tagList(icon("lightbulb"), "Indsigt & Analyse"), value = "analyse"),
            tabPanel(title = tagList(icon("sliders-h"), "Simulation"), value = "simulation"),
            tabPanel(title = tagList(icon("users"), "Leads"), value = "leads")
          )
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      class = "sidebarPanel",
      
      
      div(
        h4(strong("Vælg dine indsigter")),
        p("Brug værktøjerne nedenfor til at filtrere og analysere churn-risiko på tværs af medlemsdata.",
          style = "font-size: 14px; color: #555555;")
      ),
      
      checkboxGroupButtons(
        inputId = "risk_categories",
        label = "Risikoniveau:",
        choices = c("High", "Medium", "Low"),
        selected = c("High", "Medium", "Low"),
        status = "primary"
      ),
      
      pickerInput(
        inputId = "postal_code",
        label = "Vælg postnummer:",
        choices = c("Vælg alle" = "ALL", unique(data_map$PostalCode)),
        multiple = TRUE,
        options = list(`actionsBox` = TRUE, `liveSearch` = TRUE, `title` = "Ingen valgt"), 
        
      ),
      
      pickerInput(
        inputId = "CompanyTypeName",
        label = "Vælg virksomhedstype:",
        choices = c("Vælg alle" = "ALL", unique(as.character(data_map$CompanyTypeName))),
        multiple = TRUE,
        options = list(`actionsBox` = TRUE, `liveSearch` = TRUE, `title` = "Ingen valgt")
        
      ),
      
      pickerInput(
        inputId = "Branche_navn",
        label = "Vælg branche:",
        choices = c("Vælg alle" = "ALL", unique(as.character(data_map$Branche_navn))),
        multiple = TRUE,
        options = list(`actionsBox` = TRUE, `liveSearch` = TRUE, `title` = "Ingen valgt")
      ),
      
      
      sliderInput(
        inputId = "churn_range",
        label = "Churn sandsynlighed:",
        min = 0,
        max = 1,
        value = c(0, 1),
        step = 0.01
      ),
      
      
      actionButton("reset_filters", "Nulstil filtre", class = "custom-btn"),
      actionButton("show_top10", "Vis Top 10 Churn", class = "custom-btn"),
      
      
    ),
    
    mainPanel(
      width = 9,
      
      conditionalPanel(
        condition = "input.main_tabs == 'dashboard'",
        div(class = "info-box-container",
            div(class = "info-box",
                icon("users", class = "fa-2x", style = "color: white;"),
                h4("Antal medlemmer"),
                p("1.870")
            ),
            div(class = "info-box",
                icon("door-open", class = "fa-2x", style = "color: white;"),
                h4("Churned medlemmer"),
                p("1.325")
            ),
            div(class = "info-box",
                h4("Potentiel churn"),
                p("1.052")
            ),
            div(class = "info-box",
                h4("GNS. risiko score"),
                p("70.8%")
            ),
            div(class = "info-box",
                h4("GNS. loyalitet (måneder)"),
                p("93.6 mdr")
            ),
            div(class = "info-box",
                h4("Mangler at deltage i event"),
                p("2059 dage")
            ),
            div(class = "info-box",
                h4("Mangler at kontakte"),
                p("257 dage")
            )
        ),
        leafletOutput("map", height = "100px", width = "50%")
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'map'",
        fluidRow(
          column(
            width = 7,
            leafletOutput("map", height = "700px")
          ),
          column(
            width = 5,
            h4("Filtrerede virksomheder"),
            DTOutput("top_members_list")
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'analyse'",
        plotOutput("risk_distribution", height = "300px"),
        plotOutput("postal_code_summary", height = "300px")
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'simulation'",
        p("Simulation indhold her")
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'leads'",
        DTOutput("data_table"),
        downloadButton("download_data", "Download data")
      )
    )
  )
)


#-------------------------------------------------------------------------------
# 3.1 Serverfunktion – Reaktiv filtrering
#-------------------------------------------------------------------------------
server <- function(input, output, session) {
  filtered_data <- reactive({
    data <- data_map %>%
      filter(churn_prob >= input$churn_range[1], churn_prob <= input$churn_range[2]) %>%
      filter(risk_category %in% input$risk_categories)
    
    # Filtrér på postnumre hvis ikke "Vælg alle"
    if (!is.null(input$postal_code) && !("ALL" %in% input$postal_code)) {
      data <- data %>% filter(PostalCode %in% input$postal_code)
    }
    
    # Filtrér på virksomhedstype
    if (!is.null(input$CompanyTypeName) && !("ALL" %in% input$CompanyTypeName)) {
      data <- data %>% filter(CompanyTypeName %in% input$CompanyTypeName)
    }
    
    # Filtrér på branche
    if (!is.null(input$Branche_navn) && !("ALL" %in% input$Branche_navn)) {
      data <- data %>% filter(Branche_navn %in% input$Branche_navn)
    }
    
    # Debug-print i konsollen
    print(paste("Antal rækker i filtered_data:", nrow(data)))
    
    data
    
  })
  output$map <- renderLeaflet({
    df <- filtered_data()
    risk_pal <- colorFactor(
      palette = c("darkred", "yellow", "darkgreen"),
      levels = c("High", "Medium", "Low")
    )
    
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%  # Du kan også bruge: CartoDB.Positron
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
      setView(lng = 10.0, lat = 56.0, zoom = 7,5) %>%  # ← centrér og zoom på Danmark
      addLegend(
        position = "bottomright",
        pal = risk_pal,
        values = ~risk_category,
        title = "Churn Risiko",
        opacity = 1
      )
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
  
  observeEvent(input$select_all_postal, {
    updatePickerInput(
      session,
      inputId = "postal_code",
      selected = unique(data_map$PostalCode)
    )
  })
  
  output$top_members_list <- renderDT({
    filtered_data() %>%
      arrange(desc(churn_prob)) %>%
      slice_head(n = 10) %>%
      select(PNumber, CompanyName, CompanyTypeName, Branche_navn, PostalCode, churn_prob) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
    
  })
  
  output$filtered_list <- renderDT({
    filtered_data() %>%
      select(PNumber, CompanyName, Branche_navn, CompanyTypeName, PostalCode, churn_prob) %>%
      arrange(desc(churn_prob)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
}

#-------------------------------------------------------------------------------
# 4. Kørsel af Shiny-app
#-------------------------------------------------------------------------------
shinyApp(ui, server)
