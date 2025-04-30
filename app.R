

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
library(tibble)# Tilføjet for simulation
library(tidymodels)
#install.packages("ggforce")
library(ggforce)


#-------------------------------------------------------------------------------
# 1.2 Indlæsning og behandling af data
#-------------------------------------------------------------------------------
full_results <- readRDS("data/full_results.rds")
final_model <- readRDS("models/final_churn_model.rds")

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
        options = list(`actionsBox` = TRUE, `liveSearch` = TRUE, `title` = "Ingen valgt") 
        
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
        max = 100,
        value = c(0, 100),
        step = 10
      ),
      
      
      actionButton("reset_filters", "Nulstil filtre", class = "custom-btn")
      
      
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
                
            ),
            div(class = "info-box",
                actionButton("show_top10", "Vis Top 10 Churn", class = "custom-btn")
            )
            
        ),
        br(),  # ← tilføjer lidt luft
        DT::dataTableOutput("dashboard_table"),  # ← NY TABEL HER
        br(),
        
        
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
        plotOutput("postal_code_summary", height = "300px"),
        plotOutput("company_type_churn", height = "300px"),
        plotOutput("risk_category_distribution", height = "300px"),
        plotOutput("branche_churn_boxplot", height = "300px")
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'simulation'",
        fluidRow(
          column(
            width = 6,
            wellPanel(
              style = "background: #f8f9fa; border-radius: 8px; padding: 20px;",
              h4("Simuler Churn for Ny Virksomhed", style = "color: #0e2f33;"),
              
              numericInput("sim_employees", "Antal ansatte:", value = 10, min = 1, max = 500),
              pickerInput("sim_postal", "Postnummer:", choices = unique(data_map$PostalCode)),
              pickerInput("sim_company_type", "Virksomhedstype:", choices = levels(data_map$CompanyTypeName)),
              radioGroupButtons("sim_contact", "Har haft kontakt:", 
                                choices = c("Ja" = "Ja", "Nej" = "Nej"), selected = "Ja"),
              radioGroupButtons("sim_event", "Deltaget i event:", 
                                choices = c("Ja" = "Ja", "Nej" = "Nej"), selected = "Nej"),
              pickerInput("sim_help_category", "Hjælpekategori:", 
                          choices = levels(data_map$hjælp_kategori)),
              sliderInput("sim_member_years", "Medlemsantal år:", 
                          min = 0, max = 20, value = 2),
              pickerInput("sim_branche", "Branche:", 
                          choices = levels(data_map$Branche_navn)),
              numericInput("sim_meeting_length", "Mødelængde (minutter):", 
                           value = 60, min = 0, max = 300)
            ),
            actionButton("run_simulation", "Kør Simulation", 
                         class = "btn-primary", icon = icon("play"))
          ),
          column(
            width = 6,
            wellPanel(
              style = "background: #ffffff; border-radius: 8px; min-height: 400px; padding: 20px;",
              h4("Simuleringsresultat", style = "color: #0e2f33;"),
              div(
                style = "text-align: center; margin-top: 30px;",
                htmlOutput("simulation_result"),
                plotOutput("simulation_gauge", height = "200px")
              ),
              hr(),
              h5("Faktorers indflydelse:"),
              plotOutput("simulation_factors", height = "200px")
            )
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'leads'",
        DTOutput("data_table"),
        downloadButton("download_data", "Download data")
      )
    )
  )
)

tabPanel("Om Business Viborg",
         fluidRow(
           column(12,
                  h3("Om Business Viborg"),
                  p("Business Viborg understøtter virksomheder i Viborg Kommune gennem netværk, rådgivning og vækstinitiativer."),
                  p("Læs mere på: ",
                    a("www.businessviborg.dk", href = "https://www.businessviborg.dk", target = "_blank"))
           )
         )
)



#-------------------------------------------------------------------------------
# 3.1 Serverfunktion – Reaktiv filtrering
#-------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  
  filtered_data <- reactive({
    data <- data_map %>%
      filter(churn_prob * 100 >= input$churn_range[1], churn_prob * 100 <= input$churn_range[2]) %>%
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
  
  output$dashboard_table <- renderDT({
    filtered_data() %>%
      select(PNumber, CompanyTypeName, Branche_navn, PostalCode, churn_prob) %>%
      mutate(churn_prob = scales::percent(churn_prob, accuracy = 0.1)) %>%
      arrange(desc(churn_prob)) %>%
      datatable(
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          searching = FALSE,
          lengthChange = FALSE
        ),
        rownames = TRUE
      )
  })
  
  
  output$download_data <- downloadHandler(
    filename = function() { paste("churn_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )
  
  risk_pal <- c("Low" = "#2ca02c",      # grøn
                "Medium" = "#ffcc00",   # gul
                "High" = "#d62728")     # rød
  
  
  output$risk_distribution <- renderPlot({
    df <- filtered_data()
    ggplot(df, aes(x = churn_prob, fill = risk_category)) +
      geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.6, color = "black") +
      scale_fill_manual(values = risk_pal) +
      labs(
        title = "Fordeling af churn-sandsynlighed efter risikokategori",
        x = "Churn-sandsynlighed",
        y = "Antal virksomheder",
        fill = "Risikokategori"
      ) +
      theme_minimal(base_size = 13)
  })
  
  
  output$postal_code_summary <- renderPlot({
    df <- filtered_data()
    df %>%
      group_by(PostalCode) %>%
      summarise(avg_churn = mean(churn_prob), n = n()) %>%
      mutate(
        risk_category = case_when(
          avg_churn < 0.33 ~ "Low",
          avg_churn < 0.66 ~ "Medium",
          TRUE ~ "High"
        )
      ) %>%
      ggplot(aes(x = reorder(PostalCode, avg_churn), y = avg_churn, fill = risk_category)) +
      geom_col() +
      geom_text(aes(label = paste0(round(avg_churn * 100, 1), "% (n=", n, ")")),
                hjust = -0.1, size = 3.5) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_manual(values = risk_pal, name = "Risikokategori") +
      coord_flip() +
      labs(
        title = "Gennemsnitlig churn-sandsynlighed pr. postnummer",
        x = "Postnummer",
        y = "Gennemsnitlig churn (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.margin = margin(10, 30, 10, 10))
  })
  
  
  output$company_type_churn <- renderPlot({
    df <- filtered_data()
    df %>%
      group_by(CompanyTypeName) %>%
      summarise(avg_churn = mean(churn_prob), n = n()) %>%
      mutate(
        risk_category = case_when(
          avg_churn < 0.33 ~ "Low",
          avg_churn < 0.66 ~ "Medium",
          TRUE ~ "High"
        )
      ) %>%
      ggplot(aes(x = reorder(CompanyTypeName, avg_churn), y = avg_churn, fill = risk_category)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = risk_pal, name = "Risikokategori") +
      coord_flip() +
      labs(
        title = "Gennemsnitlig churn pr. virksomhedstype",
        x = "Virksomhedstype",
        y = "Gennemsnitlig churn (%)"
      ) +
      theme_minimal(base_size = 13)
  })
  
  
  output$risk_category_distribution <- renderPlot({
    df <- filtered_data()
    df %>%
      count(risk_category) %>%
      ggplot(aes(x = reorder(risk_category, n), y = n, fill = risk_category)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = n), vjust = -0.3) +
      labs(
        title = "Fordeling af virksomheder efter risikokategori",
        x = "Risikokategori",
        y = "Antal virksomheder"
      ) +
      scale_fill_manual(values = risk_pal) +
      theme_minimal(base_size = 13)
  })
  
  output$branche_churn_boxplot <- renderPlot({
    df <- filtered_data()
    ggplot(df, aes(x = reorder(Branche_navn, churn_prob, FUN = median), y = churn_prob)) +
      geom_boxplot(fill = "darkgreen") +
      scale_y_continuous(labels = scales::percent) +
      coord_flip() +
      labs(
        title = "Churn-sandsynlighed fordelt på brancher",
        x = "Branche",
        y = "Churn (%)"
      ) +
      theme_minimal(base_size = 13)
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
      selected = unique(data_map$PostalCode))
  })
  
  output$top_members_list <- renderDT({
    filtered_data() %>%
      mutate(churn_prob = scales::percent(churn_prob, accuracy = 0.1)) %>%
      arrange(desc(churn_prob)) %>%
      select(PNumber, CompanyTypeName, PostalCode, churn_prob) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$filtered_list <- renderDT({
    filtered_data() %>%
      select(PNumber, CompanyTypeName, PostalCode, churn_prob) %>%
      mutate(churn_prob = scales::percent(churn_prob, accuracy = 0.1)) %>%
      arrange(desc(churn_prob)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Simulation section 
  output$simulation_result <- renderUI({
    if (input$run_simulation > 0) {
      isolate({
        new_company <- tibble(
          Employees = input$sim_employees,
          PostalCode = factor(input$sim_postal, levels = levels(data_map$PostalCode)),
          CompanyTypeName = factor(input$sim_company_type, levels = levels(data_map$CompanyTypeName)),
          har_haft_kontakt = factor(input$sim_contact, levels = c("Ja", "Nej")),
          deltaget_i_event = factor(input$sim_event, levels = c("Ja", "Nej")),
          hjælp_kategori = factor(input$sim_help_category, levels = levels(data_map$hjælp_kategori)),
          medlem_antal_år = input$sim_member_years,
          Branche_navn = factor(input$sim_branche, levels = levels(data_map$Branche_navn)),
          MeetingLength = input$sim_meeting_length,
          PNumber = 99999999
        )
        
        # Predict churn probability
        prediction <- predict(final_model, new_data = new_company(), type = "prob")
        churn_prob <- prediction$.pred_1
        risk_category <- ifelse(churn_prob > 0.75, "High",
                                ifelse(churn_prob > 0.5, "Medium", "Low"))
        
        color <- ifelse(risk_category == "High", "#d9534f",
                        ifelse(risk_category == "Medium", "#f0ad4e", "#5cb85c"))
        
        div(
          style = "margin-bottom: 30px;",
          h3(paste0("Churn-sandsynlighed: ", round(churn_prob * 100, 1), "%"), 
             style = paste0("color: ", color, "; text-align: center;")),
          h4(paste0("Risikokategori: ", risk_category), 
             style = "text-align: center; font-weight: bold;"),
          p("Baseret på indtastede parametre", style = "text-align: center; font-style: italic;")
        )
        
      })
    } else {
      div(
        style = "text-align: center; padding-top: 50px; color: #666;",
        icon("sliders-h", style = "font-size: 50px; margin-bottom: 20px;"),
        h4("Indtast parametre og klik på 'Kør Simulation'")
      )
    }
  })
  
  output$simulation_gauge <- renderPlot({
    if (input$run_simulation > 0) {
      isolate({
        new_company_data <- new_company()
        prediction <- predict(final_model, new_data = new_company(), type = "prob")
        churn_prob <- prediction$.pred_1
        
        ggplot() +
          geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1,
                           start = 0, end = 2*pi, fill = "lightgrey")) +
          geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1,
                           start = 0, end = 2*pi * churn_prob, 
                           fill = ifelse(churn_prob > 0.75, "#d9534f",
                                         ifelse(churn_prob > 0.5, "#f0ad4e", "#5cb85c")))) +
          geom_text(aes(x = 0, y = 0, 
                        label = paste0(round(churn_prob * 100, 1), "%")),
                    size = 8, fontface = "bold") +
          coord_fixed() +
          theme_void() +
          theme(legend.position = "none") +
          scale_fill_identity()
      })
    }
  })
  
  output$simulation_factors <- renderPlot({
    if (input$run_simulation > 0) {
      isolate({
        var_imp <- tibble(
          factor = c("Medlemsvarighed", "Event-deltagelse", "Kontakt", "Branche", "Ansatte"),
          importance = c(0.3, 0.25, 0.2, 0.15, 0.1)
        )
        
        ggplot(var_imp, aes(x = reorder(factor, importance), y = importance)) +
          geom_col(fill = "#0e2f33", width = 0.7) +
          coord_flip() +
          labs(x = "", y = "Relativ betydning") +
          theme_minimal() +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1))
      })
    }
  })
  
  new_company <- reactive({
    tibble(
      Employees = input$sim_employees,
      PostalCode = factor(input$sim_postal, levels = levels(data_map$PostalCode)),
      CompanyTypeName = factor(input$sim_company_type, levels = levels(data_map$CompanyTypeName)),
      har_haft_kontakt = factor(input$sim_contact, levels = c("Ja", "Nej")),
      deltaget_i_event = factor(input$sim_event, levels = c("Ja", "Nej")),
      hjælp_kategori = factor(input$sim_help_category, levels = levels(data_map$hjælp_kategori)),
      medlem_antal_år = input$sim_member_years,
      Branche_navn = factor(input$sim_branche, levels = levels(data_map$Branche_navn)),
      MeetingLength = input$sim_meeting_length,
      PNumber = 99999999
    )
  })
}

#-------------------------------------------------------------------------------
# 4. Kørsel af Shiny-app
#-------------------------------------------------------------------------------
shinyApp(ui, server)
