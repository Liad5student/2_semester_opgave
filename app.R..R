

#-------------------------------------------------------------------------------
# 1.1 Indlæsning af pakker
#-------------------------------------------------------------------------------
library(shiny)           # Brugergrænseflade (UI) og serverlogik
library(leaflet)         # Kortvisualisering
library(dplyr)           # Data-manipulation (filter, mutate, group_by, etc.)
library(readr)           # Hurtig og enkel import af data
library(shinyWidgets)    # Ekstra UI-komponenter (fx pickerInput, radioGroupButtons)
library(DT)              # Dynamiske datatabeller
library(ggplot2)         # Visualisering med grafer
library(tibble)          # Bruges til bl.a. simulation (kolonneorienterede datastrukturer)
library(tidymodels)      # Modellering og machine learning framework

#-------------------------------------------------------------------------------
# 1.2 Indlæsning af datasæt og churn-model
#-------------------------------------------------------------------------------
full_results <- readRDS("data/full_results.rds")           # RDS-fil med medlems- og churndata
final_model <- readRDS("models/final_churn_model.rds")     # RDS-fil med trænet model

#-------------------------------------------------------------------------------
# 1.3 Tilføj geografiske koordinater til postnumre
#-------------------------------------------------------------------------------
postal_coords <- data.frame(
  PostalCode = c(8800, 8850, 8830, 7470, 8840, 7800, 8831, 8832, 9632, 7850, 9620, 9500, 8860),
  lat = c(56.451, 56.532, 56.447, 56.489, 56.472, 56.475, 56.448, 56.449, 56.907, 56.573, 56.824, 57.226, 56.611),
  lng = c(9.404, 8.486, 9.186, 9.000, 8.662, 9.156, 9.163, 9.171, 9.287, 9.156, 9.388, 9.388, 8.954)
)

#-------------------------------------------------------------------------------
# 1.4 Sørg for at postnumre er i karakterformat (nødvendigt for korrekt join)
#-------------------------------------------------------------------------------
full_results$PostalCode <- as.character(full_results$PostalCode)
postal_coords$PostalCode <- as.character(postal_coords$PostalCode)

#-------------------------------------------------------------------------------
# 1.5 Join datasæt og tildel risikokategorier
#-------------------------------------------------------------------------------
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
# 1.6 BEREGNINGSFUNKTION: Medlemskabspris - Antal ansatte
#-------------------------------------------------------------------------------

calculate_membership_fee <- function(n) {
  case_when(
    is.na(n)     ~ 0,
    n <= 1       ~ 1550,
    n <= 4       ~ 2770,
    n <= 9       ~ 3880,
    n <= 24      ~ 6760,
    n <= 49      ~ 9750,
    n <= 99      ~ 11960,
    TRUE         ~ 15510
  )
}


#-------------------------------------------------------------------------------
# 2.1 Begyndelsen af UI - layout og styling
#-------------------------------------------------------------------------------

ui <- fluidPage(

#-------------------------------------------------------------------------------
# 2.2 CSS og Styling
#-------------------------------------------------------------------------------
  # =====================================================
# 1.4 BRUGERDEFINERET CSS – TEMA, STIL & UI-KOMPONENTER
# =====================================================
tags$head(
  tags$style(HTML("

    /* =====================================================
       BASISLAYOUT & TYPOGRAFI
    ===================================================== */
    body {
      background: #ffffff;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      zoom: 0.8;
    }

    .small-paragraph {
      font-size: 16px;
      margin-top: 10px;
    }

    /* =====================================================
       OVERORDNET UI: TOPBAR, FANER & INDHOLD
    ===================================================== */
    .top-bar {
      background: #ffffff;
      font-family: 'Open Sans', sans-serif;
      padding: 15px 30px;
      display: flex;
      align-items: center;
      justify-content: space-between;
      border-bottom: 2px solid #eeeeee;
    }

    .main_tabs {
      background: linear-gradient(to bottom, #0e2f33 0%, #2a6c73 100%);
      padding: 10px 2px;
      color: #ffffff;
      text-align: center;
      margin-bottom: 2px;
    }

    .main_tabs .tab-content {
      background-color: #0e2f33;
    }

    .tab-content {
      display: flex;
      justify-content: flex-start;
      margin-top: 25px;
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

    /* =====================================================
       KNAPPER (generelle + gruppeknapper + brugerdefinerede)
    ===================================================== */
    .btn, .btn-primary, .btn-warning, .btn-danger {
      background-color: #0e2f33 !important;
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
      background-color: #0e2f33 !important;
      color: #fff !important;
      font-weight: 600;
    }

    .custom-btn {
      background-color: #325e63 !important;
      color: #ffffff !important;
      font-weight: bold;
      border: none;
      border-radius: 10px;
      padding: 20px 20px;
      margin-top: 5px;
      display: block;
      width: 100%;
      text-align: center;
    }

    /* =====================================================
       INFOBOKSE OG DASHBOARD-ELEMENTER
    ===================================================== */
    .info-box-container {
      display: flex;
      flex-wrap: wrap;
      gap: 15px;
      margin-bottom: 20px;
      justify-content: flex-start;
    }

    .info-box {
      flex: 1 1 calc(14.28% - 20px);
      background-color: #325e63;
      border-radius: 10px;
      min-width: 180px;
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
      color: #325e63;
    }

    .info-box2 {
      background-color: #173234;
      color: #ffffff;
      padding: 12px;
      border-radius: 8px;
      margin-bottom: 10px;
      text-align: center;
    }

    .info-box3 {
      background-color: #F5F5F5;
      color: #173234;
      padding: 10px 20px;
      border-radius: 6px;
      text-align: center;
      font-weight: bold;
      margin-bottom: 10px;
      margin-top: 0px;
      font-size: 26px;
    }

    .dashboard-wrapper {
      padding-left: 40px;
      padding-right: 40px;
    }

    /* =====================================================
       SIDEPANELER & FILTRERING
    ===================================================== */
    .sidebarPanel {
      background-color: #ffffff;
      padding: 30px;
      border-radius: 10px;
      border-left: 10px solid #2a6c73;
    }

    .filter-box {
      background-color: #ffffff;
      color: #000000;
      padding: 25px;
      border-radius: 10px;
      border: 1px solid #dddddd;
      box-shadow: 0 2px 5px rgba(0,0,0,0.05);
      margin-top: 20px;
      text-align: left;
    }

    /* =====================================================
       DROPDOWNS (bootstrap) & SLIDERS
    ===================================================== */
    .bootstrap-select .dropdown-toggle {
      background-color: #0e2f33 !important;
      color: #ffffff !important;
      border-color: #0e2f33 !important;
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

    /* =====================================================
       TABELVISNINGER (DT)
    ===================================================== */
    #dashboard_table,
    #dashboard_table * {
      color: white !important;
    }

    #dashboard_table,
    .dataTables_wrapper {
      height: 100%;
      min-height: 100%;
      width: 100% !important;
    }

    .dataTables_wrapper .dataTables_scrollBody {
      max-height: none !important;
    }

    /* =====================================================
       LEAFLET MAPS
    ===================================================== */
    .leaflet-container {
      width: 70% !important;
      margin-left: 0 !important;
      color: white !important;
    }

    /* =====================================================
       VIRKSOMHEDSDETALJER – KORTVISNING & RISIKO
    ===================================================== */
    .company-detail-card {
      background: #ffffff;
      padding: 25px;
      border-radius: 12px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      height: 100%;
    }

    .company-risk {
      font-size: 40px;
      font-weight: bold;
      margin-top: 10px;
      margin-bottom: 5px;
    }

    .company-risk-label {
      font-size: 18px;
      font-weight: bold;
    }

    .detail-button {
      width: 100%;
      margin-bottom: 10px;
    }

    .company-detail-title {
      font-size: 20px;
      font-weight: bold;
      margin-bottom: 10px;
      color: #0e2f33;
    }
  "))
),

  
#-------------------------------------------------------------------------------
# 2.3 Top-bar
#-------------------------------------------------------------------------------
 
div(class = "top-bar",
    
    # ---------------------------------------------
    # VENSTRE SIDE: LOGO
    # ---------------------------------------------
    div(style = "display: flex; align-items: center; gap: 15px;",
        img(src = "businessviborgny.jpeg", height = "80px")
    ),
    
    # ---------------------------------------------
    # MIDTERSEKTION: OVERSKRIFT OG UNDERTEKST
    # ---------------------------------------------
    div(style = "text-align: center; flex-grow: 1;",
        h2(strong("Velkommen til medlemsindsigt", 
                  style = "font-size: 30px; margin: 0; color: #0e2f33;")),
        p("Overvåg og analyser churn risiko blandt medlemmer",
          style = "color: #88c5aa; font-size: 18px; margin: 0;")
    ),
    
    # ---------------------------------------------
    # HØJRE SIDE: SØGEFELT OG IKON (BRUGER)
    # ---------------------------------------------
    div(style = "display: flex; align-items: center; gap: 10px;",
        textInput("search", NULL, placeholder = "Søg...", width = "200px"),
        tags$div(style = "width: 40px; height: 40px; background-color: #0e2f33;
                          border-radius: 50%; display: flex; align-items: center;
                          justify-content: center; color: white; font-size: 18px;",
                 icon("user"))
    )
),
 
#-------------------------------------------------------------------------------
# 2.5 Tabs og Indhold
#-------------------------------------------------------------------------------
div(class = "main_tabs",
    
    # ---------------------------------------------
    # FANEPANEL: Styrer navigationen mellem visninger
    # ---------------------------------------------
    tabsetPanel(
      id = "main_tabs",  # Unik ID bruges til at spore valgt fane
      
      
      # ---------------------------------------------
      # FANEPANEL: Dashboard 
      # ---------------------------------------------
      tabPanel("Dashboard",
                   fluidRow(
                     
                     # Venstre kolonne: Filtre
                     column(2,
                            div(class = "filter-box",
                                h5("Strategisk ledelsesoverblik"),
                                h4(strong("Vigtige nøgletal")),
                                p("Dashboardet giver et solidt, datadrevet fundament for strategiske beslutninger om medlemsudvikling og fastholdelse.
                                  Få et overblik over churn-risiko, medlemsloyalitet og økonomiske konsekvenser. 
                                  Anvend indsigt fra data til at optimere både relationer og ressourcer – og styrk organisationens samlede medlemsværdi.",
                                  style = "font-size: 14px;"),
                                
                                
                            )
                     ),
                     
                     column(10,
                     
                     div(style = "padding: 3px 2px;",  
                        
                        div(class = "info-box-container",
                         
                         div(class = "info-box2", style = "background-color: #B43C37;", 
                             icon("sign-out-alt", style = "font-size: 24px; color:#88c5aa margin-bottom: 10px;"),
                             h4("Churned"), 
                             p("I år"),
                             h2(textOutput("info_churned_members"))
                         ),
                         
                         
                         div(class = "info-box2", style = "background-color: #B43C37;", 
                             icon("minus-circle", style = "font-size: 24px; color: #88c5aa margin-bottom: 10px;"),
                             h4("Tab pga. churn"),
                             p("I måneden"),
                             h2(textOutput("churn_loss_box_churned"))
                         ),
                         
                         div(class = "info-box2", style = "background-color: #B43C37;", 
                             icon("exclamation-triangle", style = "font-size: 24px; color: #88c5aa margin-bottom: 10px;"),
                             h4("Potentiel churn"),
                             p("Over 75% risiko"),
                             h2(textOutput("info_high_risk_members"))
                             
                         ),
                         
                         div(class = "info-box2", style = "background-color: #B43C37;", 
                             icon("exclamation-triangle", style = "font-size: 24px; color: #88c5aa margin-bottom: 10px;"),
                             h4("Potentielt tab"),
                             p("I måneden"),
                             h2(textOutput("churn_loss_box"))
                         ),
                         
                         div(class = "info-box2", style = "background-color: #B43C37;", 
                             icon("percent", style = "font-size: 24px; color: #88c5aa margin-bottom: 10px;"),
                             h4("Risiko score"),
                             p("Gennemsnitlig"),
                             h2(textOutput("info_avg_risk_score"))
                         ),
                         
                         div(class = "info-box2",
                             icon("clock", style = "font-size: 24px; color: #88c5aa; margin-bottom: 10px;"),
                             h4("Loyalitet"),
                             p("Gennemsnitlig"),
                             h2(textOutput("info_avg_loyalty"))
                         
                         ),
                         
                        
                         
                         div(class = "info-box2",
                             icon("users", style = "font-size: 24px; color: #88c5aa; margin-bottom: 10px;"),
                             h4("Medlemmer"),
                             p("Antal Aktive"),
                             h2(textOutput("info_total_members"))
                         ),
                         
                         
                         
                         div(class = "info-box2",
                             icon("money-bill-wave", style = "font-size: 24px; color: #88c5aa; margin-bottom: 10px;"),
                             h4("Indtjening"),
                             p("Om måneden"),
                             h2(textOutput("budget_box"))
                         ),
                         
                         
                         div(class = "info-box2",
                             icon("money-bill-wave", style = "font-size: 24px; color: #88c5aa; margin-bottom: 10px;"),
                             h4("Indtjening"),
                             p("Om året"),
                             h2(textOutput("budget_box1"))
                         ),
                     )
                     )
                   ),
                   
                   br(),
                   
                   fluidRow(
                     column(4, div(style = "padding-left: 40px;", plotOutput("plot_churn_by_size", height = "450px"))),
                     column(4, plotOutput("plot_risk_category", height = "450px")),
                     column(4, plotOutput("plot_membership_years", height = "450px"))
                   )
               )
      ),
      
      # ---------------------------------------------
      # FANEPANEL: Simulation
      # ---------------------------------------------
        tabPanel("Simulation",
                 div(style = "padding: 30px 40px;",
                     fluidRow(
                       column(
                         width = 6,
                         wellPanel(
                           style = "background: #f8f9fa; border-radius: 8px; padding: 20px; color: black;",
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
                                        value = 60, min = 0, max = 300),
                           
                           actionButton("run_simulation", "Kør Simulation", 
                                        class = "btn-primary", icon = icon("play"))
                         )
                       ),
                       
                       column(
                         width = 6,
                         wellPanel(
                           style = "background: #ffffff; border-radius: 8px; min-height: 400px; padding: 20px; color: black;",
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
                 )
        ),
        
                          
                          
      # ---------------------------------------------
      # FANEPANEL: Indsigt – Forbedret layout
      # --------------------------------------------- 
      tabPanel("Indsigt", 
               div(style = "padding: 30px 40px;", 
                   fluidRow(
                     
                     
                     
                     # Venstre kolonne: Filtre
                     column(3,
                            div(class = "filter-box",
                                h5("kære konsulent"),
                                h4(strong("Vælg dine indsigter")),
                                p("Brug værktøjerne nedenfor til at filtrere og analysere churn-risiko blandt virksomheder.",
                                  style = "font-size: 14px;"),
                                
                                checkboxGroupInput("risk_categories", "Risikoniveau:",
                                                   choices = c("Høj" = "High", "Mellem" = "Medium", "Lav" = "Low"),
                                                   selected = c("High", "Medium", "Low")),
                                
                                pickerInput("postal_code", "Postnummer:",
                                            choices = c("Vælg alle" = "ALL", sort(unique(data_map$PostalCode))),
                                            multiple = TRUE,
                                            options = list(`actionsBox` = TRUE)),
                                
                                pickerInput("CompanyTypeName", "Virksomhedstype:",
                                            choices = c("Vælg alle" = "ALL", sort(unique(data_map$CompanyTypeName))),
                                            multiple = TRUE,
                                            options = list(`actionsBox` = TRUE)),
                                
                                pickerInput("Branche_navn", "Branche:",
                                            choices = c("Vælg alle" = "ALL", sort(unique(data_map$Branche_navn))),
                                            multiple = TRUE,
                                            options = list(`actionsBox` = TRUE)),
                                
                                sliderInput("churn_range", "Churn sandsynlighed (%):",
                                            min = 0, max = 100, value = c(0, 100), step = 1),
                                
                                actionButton("reset_filters", "Nulstil filtre")
                            )
                     ),
                     
                     column(9,
                            
                            # Én række med 7 info-bokse til konsulenter (med inline styling)
                            fluidRow(
                              div(
                                style = "display: flex; flex-wrap: nowrap; justify-content: space-between; gap: 12px; width: 100%; margin-bottom: 10px;",
                                
                                div(style = "flex: 1; padding: 20px; background-color: #B43C37; color: white; border-radius: 10px; text-align: center;",
                                    icon("phone-slash", style = "font-size: 24px; color: white; margin-bottom: 10px;"),
                                    h4("Manglende kontakt"),
                                    p("3 måneder"),
                                    h2("412")

                                ),
                                
                                div(style = "flex: 1; padding: 20px; background-color: #B43C37; color: white; border-radius: 10px; text-align: center;",
                                    icon("medal", style = "font-size: 24px; color: white; margin-bottom: 10px;"),
                                    h4("Manglende deltagelse"),
                                    p("I næste event"),
                                    h2("396")
                                ),
                                
                                div(style = "flex: 1; padding: 20px; background-color: #123940; color: white; border-radius: 10px; text-align: center;",
                                    icon("building", style = "font-size: 24px; color: white; margin-bottom: 10px;"),
                                    h4("Medlemsvirksomheder"),
                                    h2("1933")
                               
                                ),
                                
                                div(style = "flex: 1; padding: 20px; background-color: #123940; color: white; border-radius: 10px; text-align: center;",
                                    icon("medal", style = "font-size: 24px; color: white; margin-bottom: 10px;"),
                                    h4("Top performer"),
                                    p("Event"),
                                    h2("Søren Andersen")
                                ),
                                
                                div(style = "flex: 1; padding: 20px; background-color: #123940; color: white; border-radius: 10px; text-align: center;",
                                    icon("calendar-alt", style = "font-size: 24px; color: white; margin-bottom: 10px;"),
                                    h4("Næste event"),
                                    p("06-05-2025"),
                                    h2("AL Kursus"),
                                    
                                    
                                ),
                                
                                div(style = "flex: 1; padding: 20px; background-color: #123940; color: white; border-radius: 10px; text-align: center;",
                                    icon("calendar-check", style = "font-size: 24px; color: white; margin-bottom: 10px;"),
                                    h4("Eventdeltagere"),
                                    p("Næste event"),
                                    h2("86")
                                ),
                                
                                
                                div(style = "flex: 1; padding: 20px; background-color: #123940; color: white; border-radius: 10px; text-align: center;",
                                    icon("phone", style = "font-size: 24px; color: white; margin-bottom: 10px;"),
                                    h4("Opkald i dag"),
                                    h2("17")
                                )
                              )
                            ),
                            
                            # DataTable
                            div(style = "height: 750px; overflow-y: auto;",
                                DTOutput("dashboard_table", width = "100%")
                            )
                     )
                     
                     
                     
                   )
               )
      ),
      
      # ---------------------------------------------
      # FANEPANEL: Detaljer 
      # ---------------------------------------------
    
        tabPanel("Detaljer",
                 div(style = "padding: 30px 40px;",
                     
                     # Kun vis søgefelt hvis ingen virksomhed er valgt
                     conditionalPanel(
                       condition = "output.noCompanySelected",
                       div(
                         style = "margin-bottom: 20px;",
                         textInput("search_pnumber", "Søg virksomhed via P-nummer:", "", width = "300px")
                       )
                     ),
                     
                     uiOutput("selected_company_details")
                 )
        ),
    
      # ---------------------------------------------
      # FANEPANEL: Indsigt og analyse
      # ---------------------------------------------
    
    tabPanel("Indsigt & Analyse",
             plotOutput("risk_distribution"),
             plotOutput("postal_code_summary"),
             plotOutput("company_type_churn"),
             plotOutput("risk_category_distribution"),
             plotOutput("branche_churn_boxplot")
    ),
    
#-------------------------------------------------------------------------------
# Afslutning på UI 
#------------------------------------------------------------------------------- 
  )
)
)


#-------------------------------------------------------------------------------
# 3.1 Begyndelse på serverfunktion 
#-------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # ---------------------------------------------
  # Mørkt thema - kort 
  # ---------------------------------------------
  dark_theme <- theme(
    panel.background = element_rect(fill = "#0e2f33"),
    plot.background = element_rect(fill = "#0e2f33", color = NA),
    panel.grid.major = element_line(color = "#1c4a50"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", face = "bold"),
    legend.background = element_rect(fill = "#0e2f33"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )
  
  # ---------------------------------------------
  # Søger efter medlemsvirksomhed 
  # ---------------------------------------------
  searched_company <- reactive({
    req(input$search_pnumber)
    pnum <- as.numeric(input$search_pnumber)
    data_map %>% filter(PNumber == pnum)
  })
  
  # ---------------------------------------------
  # Filtrerer data baseret på inputfiltre fra UI
  # ---------------------------------------------
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
  
  # ---------------------------------------------
  # Viser samlet antal unikke aktive medlemmer (ikke churnede)
  # ---------------------------------------------
  output$info_total_members <- renderText({
    length(unique(data_map$PNumber[data_map$churn == 0]))
  })
  
  
  # ---------------------------------------------
  # Antal medlemmer der er churnet
  # ---------------------------------------------
  output$info_churned_members <- renderText({
    sum(data_map$churn == 1, na.rm = TRUE)
  })
  
  
  # ---------------------------------------------
  # Beregner det faktiske tab pga. churnede medlemmer
  # ---------------------------------------------
  output$churn_loss_box_churned <- renderText({
    df <- data_map %>% filter(churn == 1)
    loss <- sum(calculate_membership_fee(df$Employees), na.rm = TRUE)
    paste0(format(round(loss, 0), big.mark = ".", decimal.mark = ","))  # vis i hele kroner
  })
  
  
  # ---------------------------------------------
  # Antal medlemmer med churn-risiko over 75 %, men som ikke er churnet
  # ---------------------------------------------
  output$info_high_risk_members <- renderText({
    sum(data_map$churn == 0 & data_map$churn_prob > 0.75, na.rm = TRUE)
  })
  

  # ---------------------------------------------
  # Gennemsnitlig churn-risiko for aktive medlemmer (0–100%)
  # ---------------------------------------------
  output$info_avg_risk_score <- renderText({
    churn_values <- data_map$churn_prob[data_map$churn == 0]
    churn_values <- pmin(pmax(churn_values, 0), 1)  # begræns til [0, 1]
    avg <- mean(churn_values, na.rm = TRUE)
    paste0(round(avg * 100, 1), "%")
  })
  
  
  
  # ---------------------------------------------
  # Gennemsnitlig loyalitet i måneder
  # ---------------------------------------------
  output$info_avg_loyalty <- renderText({
    round(mean(data_map$medlem_antal_år, na.rm = TRUE), 1) %>% paste("år")
  })
  
  # ---------------------------------------------
  # Beregnet medlemsbudget i millioner kroner
  # ---------------------------------------------
  output$info_budget <- renderText({
    df <- data_map
    budget <- sum(calculate_membership_fee(df$Employees), na.rm = TRUE)
    paste0(format(round(budget / 1e6, 1), decimal.mark = ","), " mio.")
  })
  
  # ---------------------------------------------
  # Beregnet årligt medlemsbudget for aktive medlemmer (ikke churnede)
  # ---------------------------------------------
  output$info_budget_år <- renderText({
    df <- data_map %>% filter(churn == 0)
    annual_budget <- sum(calculate_membership_fee(df$Employees), na.rm = TRUE) * 12
    paste0(format(round(annual_budget / 1e6, 1), decimal.mark = ","), " mio.")
  })
  
  
  # ---------------------------------------------
  # Antal virksomheder uden kontakt
  # ---------------------------------------------
  output$info_contact_missing <- renderText({
    sum(data_map$har_haft_kontakt == "Nej", na.rm = TRUE) %>% paste("virksomheder")
  })
  
  # ---------------------------------------------
  # Beregner det samlede medlemsbudget for aktive medlemmer (ikke churnede)
  # ---------------------------------------------
  output$budget_box <- renderText({
    df <- data_map %>% filter(churn == 0)
    budget <- sum(calculate_membership_fee(df$Employees), na.rm = TRUE)
    paste0(format(round(budget / 1e6, 1), decimal.mark = ","), " mio.")
  })
  

  # ---------------------------------------------
  # Beregner det årlige medlemsbudget baseret på aktive medlemmer (ikke churnede)
  # ---------------------------------------------
  output$budget_box1 <- renderText({
    active_members <- data_map %>% filter(churn == 0)
    annual_budget <- sum(calculate_membership_fee(active_members$Employees), na.rm = TRUE) * 12
    paste0( format(round(annual_budget / 1e6, 1), decimal.mark = ","), " mio.")
  })
  
  # ---------------------------------------------
  # Beregner det potentielle tab for high-risk medlemmer (churn_prob > 75%, ikke churnet)
  # ---------------------------------------------
  output$churn_loss_box <- renderText({
    df <- data_map %>% filter(churn == 0, churn_prob > 0.75)
    loss <- sum(calculate_membership_fee(df$Employees), na.rm = TRUE)
    paste0( format(round(loss, 0), big.mark = ".", decimal.mark = ","))  # vis i hele kroner
  })
  
  # ---------------------------------------------
  # Leaflet-kort med farvede cirkler pr. virksomhed
  # ---------------------------------------------
  output$map <- renderLeaflet({
    df <- data_map
    risk_pal <- colorFactor(
      palette = c("darkred", "yellow", "darkgreen"),
      levels = c("High", "Medium", "Low")
    )
    
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
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
      setView(lng = 10.0, lat = 56.0, zoom = 7.5) %>%
      addLegend(
        position = "bottomright",
        pal = risk_pal,
        values = ~risk_category,
        title = "Churn Risiko",
        opacity = 1
      )
  })
  
  # ---------------------------------------------
  # Simpel datavisning af filtrerede data
  # ---------------------------------------------
  output$data_table <- renderDT({
    datatable(data_map)
  })
  
  # ---------------------------------------------
  # Dashboard-tabel med udvalgte kolonner og styling
  # ---------------------------------------------
  output$dashboard_table <- renderDT({
    data_map %>%
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
        rownames = TRUE,
        selection = "single"
      )
  })
  
  # ---------------------------------------------
  # Download-handler: Eksportér filtrerede data som CSV
  # ---------------------------------------------
  output$download_data <- downloadHandler(
    filename = function() { paste("churn_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )
  
  # ---------------------------------------------
  # Farveskema til risikokategorier i grafer
  # ---------------------------------------------
  risk_pal <- c(
    "Low" = "#2ca02c",      # grøn
    "Medium" = "#ffcc00",   # gul
    "High" = "#d62728"      # rød
  )
  
  # ---------------------------------------------
  # Plot: Histogram over churn sandsynlighed
  # ---------------------------------------------
  output$risk_distribution <- renderPlot({
    data_map %>%
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
  
  # ---------------------------------------------
  # Plot: Gennemsnitlig churn pr. postnummer
  # ---------------------------------------------
  output$postal_code_summary <- renderPlot({
    data_map %>%
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
  
  # ---------------------------------------------
  # Plot: Gennemsnitlig churn pr. virksomhedstype
  # ---------------------------------------------
  output$company_type_churn <- renderPlot({
    data_map %>%
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
      theme_minimal(base_size = 13) +
      dark_theme
  })
  
  
  # ---------------------------------------------
  # Plot: Mest repræsenterede brancher (Top 10)
  # ---------------------------------------------
  output$branche_distribution <- renderPlot({
    data_map %>%
      count(Branche_navn, sort = TRUE) %>%
      top_n(10) %>%
      ggplot(aes(x = reorder(Branche_navn, n), y = n)) +
      geom_col(fill = "#325e63") +
      coord_flip() +
      labs(title = "Top 10 brancher blandt filtrerede virksomheder",
           x = "Branche",
           y = "Antal virksomheder") +
      theme_minimal(base_size = 13) +
      dark_theme
  })
  
  # ---------------------------------------------
  # Plot: Hjælpekategorier virksomheder har brug for
  # ---------------------------------------------
  output$help_needed_plot <- renderPlot({
    data_map %>%
      count(hjælp_kategori, sort = TRUE) %>%
      top_n(10) %>%
      ggplot(aes(x = reorder(hjælp_kategori, n), y = n)) +
      geom_col(fill = "#7FC8A3") +
      coord_flip() +
      labs(title = "Virksomheders efterspurgte hjælp",
           x = "Hjælpeområde",
           y = "Antal virksomheder") +
      theme_minimal(base_size = 13) +
      dark_theme
  })
  
  # ---------------------------------------------
  # Plot: Churn-risiko fordelt på virksomhedsstørrelse
  # ---------------------------------------------
  output$plot_churn_by_size <- renderPlot({
    data_map %>%
    data_map$size_group <- cut(df$Employees, breaks = c(0, 1, 4, 9, 24, 49, Inf),
                         labels = c("0–1", "2–4", "5–9", "10–24", "25–49", "50+"))
    
    ggplot(df, aes(x = size_group, y = churn_prob)) +
      stat_summary(fun = mean, geom = "bar", fill = "#7FC8A3") +
      labs(title = "Churn-risiko fordelt på virksomhedsstørrelse",
           x = "Antal ansatte", y = "Churn-risiko (gns.)") +
      scale_y_continuous(labels = scales::percent) +
      dark_theme
  })
  
  # ---------------------------------------------
  # Plot: Antal virksomheder pr. risikokategori
  # ---------------------------------------------
  output$plot_risk_category <- renderPlot({
    data_map %>%
    ggplot(df, aes(x = risk_category, fill = risk_category)) +
      geom_bar() +
      scale_fill_manual(values = c("Low" = "#5cb85c", "Medium" = "#f0ad4e", "High" = "#d9534f")) +
      labs(title = "Fordeling på churn-risikokategori", x = "Kategori", y = "Antal") +
      dark_theme
  })
  
  # ---------------------------------------------
  # Plot: Medlemskabets længde (histogram)
  # ---------------------------------------------
  output$plot_membership_years <- renderPlot({
    data_map %>%
    ggplot(df, aes(x = medlem_antal_år)) +
      geom_histogram(binwidth = 1, fill = "#7FC8A3", color = "black") +
      labs(title = "Fordeling af medlemslængde", x = "År", y = "Antal virksomheder") +
      dark_theme
  })
  
  # ---------------------------------------------
  # Plot: Antal virksomheder i hver risikokategori
  # ---------------------------------------------
  output$risk_category_distribution <- renderPlot({
    data_map %>%
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
  
  # ---------------------------------------------
  # Plot: Churn-sandsynlighed fordelt på brancher (boxplot)
  # ---------------------------------------------
  output$branche_churn_boxplot <- renderPlot({
    data_map %>%
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
  
  
  # ---------------------------------------------
  # Nulstil alle filtre når knappen trykkes
  # ---------------------------------------------
  observeEvent(input$reset_filters, {
    updateSliderInput(session, "churn_range", value = c(0, 1))
    updateCheckboxGroupButtons(session, "risk_categories", selected = c("High", "Medium", "Low"))
    updateRadioGroupButtons(session, "view_by", selected = "all")
    updatePickerInput(session, "postal_code", selected = character(0))
    updateSelectizeInput(session, "member_id", selected = "")
  })
  
  # ---------------------------------------------
  # Vælg alle postnumre ved klik på "vælg alle"
  # ---------------------------------------------
  observeEvent(input$select_all_postal, {
    updatePickerInput(
      session,
      inputId = "postal_code",
      selected = unique(data_map$PostalCode))
  })
  
  # ---------------------------------------------
  # Skift til "Detaljer"-fanen ved valg i dashboard-tabel
  # ---------------------------------------------
  observeEvent(input$dashboard_table_rows_selected, {
    updateTabsetPanel(session, "main_tabs", selected = "Detaljer")
  })
  
  # ---------------------------------------------
  # Tabelvisning: Top-medlemmer sorteret efter churn-prob
  # ---------------------------------------------
  output$top_members_list <- renderDT({
    data_map %>%
      mutate(churn_prob = scales::percent(churn_prob, accuracy = 0.1)) %>%
      arrange(desc(churn_prob)) %>%
      select(PNumber, CompanyTypeName, PostalCode, churn_prob) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ---------------------------------------------
  # Kortvisning: Markør for valgt virksomhed
  # ---------------------------------------------
  output$company_map <- renderLeaflet({
    selected_row <- input$dashboard_table_rows_selected
    if (length(selected_row)) {
      d <- data_map[selected_row, ]
      
      if (!is.na(d$lat) && !is.na(d$lng)) {
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addMarkers(lng = d$lng, lat = d$lat,
                     popup = paste0("<strong>", d$CompanyName, "</strong><br>Postnr: ", d$PostalCode)) %>%
          setView(lng = d$lng, lat = d$lat, zoom = 11)
      } else {
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron)
      }
    }
  })
  
  # ---------------------------------------------
  # Tabel: Oversigt over filtrerede virksomheder
  # ---------------------------------------------
  output$filtered_list <- renderDT({
    data_map %>%
      select(PNumber, CompanyTypeName, PostalCode, churn_prob) %>%
      mutate(churn_prob = scales::percent(churn_prob, accuracy = 0.1)) %>%
      arrange(desc(churn_prob)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
# ---------------------------------------------
# Viser detaljeret virksomhedsinformation
# Baseret på enten valgt række i tabel eller søgning på P-nummer
# ---------------------------------------------
  output$selected_company_details <- renderUI({
    selected_row <- input$dashboard_table_rows_selected
    selected_pnum <- NULL
    
    # Hent PNumber fra den viste tabel (som er sorteret udgave af filtered_data)
    if (length(selected_row)) {
      visible_data <- filtered_data() %>%
        select(PNumber, CompanyTypeName, Branche_navn, PostalCode, churn_prob) %>%
        arrange(desc(churn_prob))  # matcher visningen i dashboard_table
      selected_pnum <- visible_data$PNumber[selected_row]
    } else if (input$search_pnumber != "") {
      selected_pnum <- as.numeric(input$search_pnumber)
    }
    
    # Find den fulde række i data_map baseret på PNumber
    data_to_show <- data_map %>% filter(PNumber == selected_pnum)
    
    if (!is.null(data_to_show) && nrow(data_to_show) > 0) {
      d <- data_to_show[1, ]
      
      churn_prob <- d$churn_prob
      fee <- calculate_membership_fee(d$Employees)
      risk <- ifelse(churn_prob > 0.75, "Høj risiko",
                     ifelse(churn_prob > 0.5, "Mellem risiko", "Lav risiko"))
      risk_color <- ifelse(churn_prob > 0.75, "#d9534f",
                           ifelse(churn_prob > 0.5, "#f0ad4e", "#5cb85c"))
      tabspotentiale <- fee
      
      # Layout med 1. kolonne (information)
      div(style = "zoom: 0.7;",
          fluidRow(
            h3("Virksomhedsprofil", style = "background-color: #fff5cc; color: #0e2f33; padding: 10px 2px; text-align:center;"),
            column(4,
                   div(class = "company-detail-card",
                       div(class = "info-box3", "Information"),
                       div(class = "info-box2",  style = "background-color: #123940; border-radius: 10px; padding: 20px;",
                           h3("Information om medlemsvirksomheden", style = "color: white; font-weight: bold; margin-bottom: 20px; text-align: center;"),
                           tags$table(style = "width: 100%; color: white; font-size: 23px; line-height: 1.8;",
                                      tags$tr(
                                        tags$td(style = "font-weight: 600; padding-right: 20px; width: 40%;", "Pnummer:"),
                                        tags$td(as.character(d$PNumber))
                                      ),
                                      tags$tr(
                                        tags$td(style = "font-weight: 600; padding-right: 20px;", "Virksomhedstype:"),
                                        tags$td(as.character(d$CompanyTypeName))
                                      ),
                                      tags$tr(
                                        tags$td(style = "font-weight: 600; padding-right: 20px;", "Branche:"),
                                        tags$td(as.character(d$Branche_navn))
                                      ),
                                      tags$tr(
                                        tags$td(style = "font-weight: 600; padding-right: 20px;", "Antal ansatte:"),
                                        tags$td(as.character(d$Employees))
                                      ),
                                      tags$tr(
                                        tags$td(style = "font-weight: 600; padding-right: 20px;", "Medlemskab:"),
                                        tags$td(paste0(d$medlem_antal_år, " år"))
                                      ),
                                      tags$tr(
                                        tags$td(style = "font-weight: 600; padding-right: 20px;", "Bidrag/måned:"),
                                        tags$td(paste0("Kr. ", format(fee, big.mark = ".", decimal.mark = ",")))
                                      )
                           )
                       ),
                  
                       
                       # Kontakt og Hjælp – venstrestillet
                       div(class = "info-box2",
                           h3("Kontakt & Hjælp", style = "color: white; font-weight: bold; margin-bottom: 20px; text-align: center;"),
                           
                           tags$table(style = "width: 100%; color: white; font-size: 15px; line-height: 1.8;",
                                      tags$tr(
                                        tags$td(style = "font-weight: 600; padding-right: 20px; width: 40%;", "Hjælp:"),
                                        tags$td(d$hjælp_kategori)
                                      ),
                                      tags$tr(
                                        tags$td(style = "font-weight: 600; padding-right: 20px;", "Konsulent:"),
                                        tags$td("Marie Sørensen")
                                      ),
                                      tags$tr(
                                        tags$td(style = "font-weight: 600; padding-right: 20px;", "01/03/2025:"),
                                        tags$td("Opfølgning sendt.")
                                      ),
                                      tags$tr(
                                        tags$td(style = "font-weight: 600; padding-right: 20px;", "15/01/2025:"),
                                        tags$td("Tilbagemelding mangler.")
                                      )
                           )
                       )
                   )
            ),
            
            
            
            # -----------------------------------------
            # KOLONNE 2: Strategiske handlinger og CTA
            # -----------------------------------------
            column(4,
                   div(class = "company-detail-card",
                       
                       div(class = "info-box3", "Strategiske tiltag"),
                       
                       # CTA: Invitation til event
                       div(class = "info-box2",
                           h2("Inviter til event"),
                           h4("Næste event: 1. juni – AL kursus"),
                           actionButton("send_event_invite", "Send invitation",
                                        style = "background-color: #88c5aa !important; color: #0e2f33 !important; width:100%; margin-bottom:5px; font-weight: bold;")
                       ),
                       
                       # CTA: Kontaktmuligheder
                       div(class = "info-box2",
                           h2("Kontakt virksomheden"),
                           actionButton("call_now", "Ring op", 
                                        style = "background-color: #88c5aa !important; color: #0e2f33 !important; width:100%; margin-bottom:5px; font-weight: bold;"),
                           actionButton("send_email", "Send mail",
                                        style = "background-color: #88c5aa !important; color: #0e2f33 !important; width:100%; margin-bottom:5px; font-weight: bold;"),
                           actionButton("book_meeting", "Book møde",
                                        style = "background-color: #88c5aa !important; color: #0e2f33 !important; width:100%; margin-bottom:5px; font-weight: bold;")
                       ),
                       
                       # CTA: Dynamiske anbefalinger (ul med conditional logik)
                       div(class = "info-box2",
                           h2("Call-to-action", style = "color: #88c5aa font-size: 20px; margin-bottom: 15px;"),
                           h4("Anbefalinger"),
                           tags$ul(style = "text-align: left; padding-left: 20px; font-size: 15px;",
                                   
                                   # Tilpasning baseret på hjælpekategori
                                   if (!is.na(d$hjælp_kategori) && d$hjælp_kategori != "" && !grepl("ingen", tolower(d$hjælp_kategori))) {
                                     tags$li(style = "margin-bottom: 10px;",
                                             strong("Målret indsatsen:"), 
                                             paste0("Virksomheden har angivet behov for hjælp til ", tolower(d$hjælp_kategori),
                                                    ". Tilbyd sparring og peg på relevante netværksevents.")
                                     )
                                   } else {
                                     tags$li(style = "margin-bottom: 10px;",
                                             strong("Åbn samtalen:"), 
                                             "Spørg ind til aktuelle udfordringer og tilbyd uforpligtende rådgivning. Ofte kan vi matche dem med relevante tilbud."
                                     )
                                   },
                                   
                                   # Branchebaseret forslag
                                   tags$li(style = "margin-bottom: 10px;",
                                           strong("Branchefokus:"),
                                           paste0(
                                             case_when(
                                               grepl("Bygge", d$Branche_navn, ignore.case = TRUE) ~ "Peg på tilbud om bæredygtighed, grøn omstilling og rekruttering i byggebranchen.",
                                               grepl("Handel", d$Branche_navn, ignore.case = TRUE) ~ "Tal om synlighed, digitalisering og konkurrenceevne i detailhandlen.",
                                               grepl("IT|Kommunikation", d$Branche_navn, ignore.case = TRUE) ~ "Foreslå dialog om digital vækst, netværk og SMV Digital støtte.",
                                               grepl("Produktion", d$Branche_navn, ignore.case = TRUE) ~ "Fremhæv sparring om automatisering, effektiv drift og grøn produktion.",
                                               grepl("Finans|Rådgivning", d$Branche_navn, ignore.case = TRUE) ~ "Tal om forretningsudvikling, markedstilgang og partnerskaber.",
                                               grepl("Service", d$Branche_navn, ignore.case = TRUE) ~ "Drøft kundeloyalitet, medarbejderfastholdelse og synlighed i servicesektoren.",
                                               TRUE ~ "Vis hvordan lignende virksomheder har fået værdi af netværk, events og personlig sparring."
                                             )
                                           )
                                   ),
                                   
                                   # Kontaktstatus
                                   if (!is.na(d$har_haft_kontakt) && d$har_haft_kontakt == "Nej") {
                                     tags$li(style = "margin-bottom: 10px;",
                                             strong("Genstart dialogen:"),
                                             "Der har ikke været kontakt det seneste år – læg op til et kort, uforpligtende møde."
                                     )
                                   },
                                   
                                   # Eventdeltagelse
                                   if (!is.na(d$deltaget_i_event) && d$deltaget_i_event == "Nej") {
                                     tags$li(style = "margin-bottom: 10px;",
                                             strong("Invitér til deltagelse:"),
                                             "De har ikke deltaget i events – foreslå noget relevant og giv en hurtig forklaring på værdien."
                                     )
                                   },
                                   
                                   # Generelle CTA'er
                                   tags$li(style = "margin-bottom: 10px;",
                                           strong("Gør det konkret:"),
                                           "Foreslå et opkald, møde eller deltagelse i næste event – gør det nemt at sige ja."),
                                   tags$li(style = "margin-bottom: 10px;",
                                           strong("Understreg medlemsfordele:"),
                                           "Forklar kort hvordan rådgivning, netværk og tilbud kan hjælpe dem her og nu.")
                           )
                       ),
                       
                       # Ekstra: Lignende events
                       div(class = "info-box2",
                           h2("Lignende virksomheder deltog i",
                              style = "color: #ffffff; font-size: 20px; margin-bottom: 15px;"),
                           tags$ul(style = "text-align: left; padding-left: 20px; font-size: 15px;",
                                   tags$li(style = "margin-bottom: 10px;", "Webinar: Grøn omstilling"),
                                   tags$li(style = "margin-bottom: 10px;", "Netværksmøde: SMV Digital"),
                                   tags$li(style = "margin-bottom: 10px;", "1-1 sparring: Strategi og vækst")
                           )
                       )
                   )
            ),
            
            # -----------------------------------------
            # KOLONNE 3: Analyse og kortvisning
            # -----------------------------------------
            column(4,
                   
                   # Advarsel om manglende eventdeltagelse
                   if (d$deltaget_i_event == "Nej") {
                     div(style = "background-color: #fff5cc; color: #ffffff; padding: 15px 20px; 
                      border-left: 10px solid #f0ad4e; border-radius: 10px; 
                      margin-bottom: 15px; font-size: 16px;",
                         HTML("<strong><i class='fa fa-exclamation-triangle'></i> Anbefaling</strong><br>
                      <span style='color:#444;'>Tag kontakt snarest – virksomheden har ikke deltaget i et event de sidste 12 måneder.</span>")
                     )
                   },
                   
                   div(class = "company-detail-card",
                       div(class = "info-box3", "Churn Analyse"),
                       
                       div(class = "info-box2", style = "background-color: #B43C37;", 
                           p("Tabspotentiale i måneden:"),
                           h2(paste0("Kr. ", format(tabspotentiale, big.mark = ".")))),
                       
                       plotlyOutput("gauge_company_detail", height = "200px"),
                       
                       div(style = "text-align:center; margin-top: -10px;",
                           h4(paste0(round(churn_prob * 100, 1), "% – ", risk),
                              style = paste0("color:", risk_color, "; font-weight:bold; font-size: 18px;")),
                           
                           div(
                             class = "info-box2",
                             style = "background-color: #ffffff; color: #0e2f33; padding: 15px 20px;",
                             h3(paste("Virksomheden er", ifelse(d$Employees > 50, "mellemstor", "lille"),
                                      "og har", ifelse(d$har_haft_kontakt == "Nej", "ikke", "haft"),
                                      "været i kontakt det seneste år. Der er", tolower(risk), "for frafald, men stort potentiale."))
                           )
                       ),
                       
                       leafletOutput("company_map", height = "220px")
                   )
            )
          )
      )
    } else {
      # Hvis intet er valgt eller fundet
      h4("Vælg en virksomhed i tabellen i 'Indsigt'-fanen for at se detaljer.")
    }
  })
  
  # ---------------------------------------------
  # Plot: Simuleret churn-trend for valgt virksomhed
  # Viser udvikling over de sidste 12 måneder
  # ---------------------------------------------
  output$churn_trend_plot <- renderPlot({
    selected_row <- input$dashboard_table_rows_selected
    if (length(selected_row)) {
      d <- filtered_data()[selected_row, ]
      set.seed(1)
      churn_history <- runif(12, min = d$churn_prob * 0.8, max = d$churn_prob * 1.1)
      months <- format(seq(Sys.Date() - months(11), by = "month", length.out = 12), "%b")
      churn_df <- data.frame(Month = factor(months, levels = months), Churn = churn_history)
      
      ggplot(churn_df, aes(x = Month, y = Churn)) +
        geom_line(color = "black", size = 1.2) +
        ylim(0, 1) +
        theme_minimal() +
        labs(title = NULL, y = NULL, x = NULL) +
        theme(
          axis.text = element_text(size = 10),
          axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(5, 5, 5, 5)
        )
    }
  })
  
  
  
  # ---------------------------------------------
  # UI-output: Viser churn-resultat fra simulation
  # ---------------------------------------------
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
        
        # Forudsig churn sandsynlighed
        prediction <- predict(final_model, new_data = new_company(), type = "prob")
        churn_prob <- prediction$.pred_1
        risk_category <- ifelse(churn_prob > 0.75, "Høj",
                                ifelse(churn_prob > 0.5, "Medium", "Lav"))
        
        color <- ifelse(risk_category == "Høj", "#d9534f",
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
      # Hvis simulation endnu ikke er kørt
      div(
        style = "text-align: center; padding-top: 50px; color: #666;",
        icon("sliders-h", style = "font-size: 50px; margin-bottom: 20px;"),
        h4("Indtast parametre og klik på 'Kør Simulation'")
      )
    }
  })
  
  # ---------------------------------------------
  # Plot: Gauge (måler) der viser churn-risiko visuelt
  # ---------------------------------------------
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
  
  # ---------------------------------------------
  # Plot: Simulerede faktorers betydning i modellen
  # ---------------------------------------------
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
  
  # ---------------------------------------------
  # UI-output: Resultat af simulationen
  # Viser churn-sandsynlighed og risikokategori
  # ---------------------------------------------
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
        
        # Forudsig churn-sandsynlighed
        prediction <- predict(final_model, new_data = new_company(), type = "prob")
        churn_prob <- prediction$.pred_1
        risk_category <- ifelse(churn_prob > 0.75, "Høj",
                                ifelse(churn_prob > 0.5, "Medium", "Lav"))
        
        color <- ifelse(risk_category == "Høj", "#d9534f",
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
      # Standardvisning før simulation er kørt
      div(
        style = "text-align: center; padding-top: 50px; color: #666;",
        icon("sliders-h", style = "font-size: 50px; margin-bottom: 20px;"),
        h4("Indtast parametre og klik på 'Kør Simulation'")
      )
    }
  })
  
  # ---------------------------------------------
  # Plot: Boxplot over churn pr. branche
  # ---------------------------------------------
  output$plot_branch_churn <- renderPlot({
    df <- data_map
    ggplot(df, aes(x = Branche_navn, y = churn_prob)) +
      geom_boxplot(fill = "#7FC8A3") +
      coord_flip() +
      labs(title = "Branche vs. Churn", x = "", y = "Churn") +
      theme_minimal()
  })
  
  # ---------------------------------------------
  # Plot: Gennemsnitlig churn ved eventdeltagelse
  # ---------------------------------------------
  output$plot_event_churn <- renderPlot({
    df <- data_map
    df$deltaget_i_event <- factor(df$deltaget_i_event)
    df %>%
      group_by(deltaget_i_event) %>%
      summarise(avg = mean(churn_prob)) %>%
      ggplot(aes(x = deltaget_i_event, y = avg, fill = deltaget_i_event)) +
      geom_col() +
      labs(title = "Eventdeltagelse vs. Churn", x = "", y = "Churn") +
      theme_minimal() +
      guides(fill = FALSE)
  })
  
  # ---------------------------------------------
  # Plot: Scatter + smooth for churn ift. antal ansatte
  # ---------------------------------------------
  output$plot_employee_churn <- renderPlot({
    df <- data_map
    ggplot(df, aes(x = Employees, y = churn_prob)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "loess") +
      labs(title = "Ansatte vs. Churn", x = "Antal ansatte", y = "Churn") +
      theme_minimal()
  })
  
  # ---------------------------------------------
  # Plot: Gauge til visning af churn-risiko efter simulation
  # ---------------------------------------------
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
  
  # ---------------------------------------------
  # Plot: Simuleret churn-trend over 12 måneder (søjlediagram)
  # ---------------------------------------------
  output$churn_trend_plot <- renderPlot({
    selected_row <- input$dashboard_table_rows_selected
    if (length(selected_row)) {
      d <- filtered_data()[selected_row, ]
      churn_prob <- d$churn_prob
      
      set.seed(1)
      churn_history <- runif(12, min = churn_prob * 0.8, max = churn_prob * 1.1)
      months <- format(seq(Sys.Date() - months(11), by = "month", length.out = 12), "%b")
      churn_df <- data.frame(Month = factor(months, levels = months), Churn = churn_history)
      
      ggplot(churn_df, aes(x = Month, y = Churn * 100, fill = Churn)) +
        geom_col(width = 0.6) +
        scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
        scale_fill_gradient(low = "#5cb85c", high = "#d9534f") +
        labs(title = "Simuleret churn over 12 måneder", x = NULL, y = "Churn (%)") +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(5, 15, 5, 5)
        )
    }
  })
  
  # ---------------------------------------------
  # Leaflet: Kortvisning med markør for valgt virksomhed
  # ---------------------------------------------
  output$company_map <- renderLeaflet({
    selected_row <- input$dashboard_table_rows_selected
    if (length(selected_row)) {
      d <- filtered_data()[selected_row, ]
      
      leaflet() %>%
        addTiles() %>%
        setView(lng = d$lng, lat = d$lat, zoom = 12) %>%
        addMarkers(
          lng = d$lng,
          lat = d$lat,
          popup = paste("<strong>", d$CompanyTypeName, "</strong><br>", d$Branche_navn)
        )
    }
  })
  
  # ---------------------------------------------
  # Plotly: Gauge til visning af churn-risiko (virksomhedsdetalje)
  # ---------------------------------------------
  output$gauge_company_detail <- renderPlotly({
    selected_row <- input$dashboard_table_rows_selected
    if (length(selected_row)) {
      d <- filtered_data()[selected_row, ]
      churn_prob <- round(d$churn_prob * 100, 0)
      
      risk <- ifelse(churn_prob > 75, "Høj risiko",
                     ifelse(churn_prob > 50, "Mellem risiko", "Lav risiko"))
      
      risk_color <- switch(risk,
                           "Lav risiko" = "#5cb85c",
                           "Mellem risiko" = "#f0ad4e",
                           "Høj risiko" = "#d9534f")
      
      plot_ly(
        type = "indicator",
        mode = "gauge",
        value = churn_prob,
        title = list(text = "", font = list(size = 1)),  # skjul titel
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "#888"),
          bar = list(color = risk_color, thickness = 0.3),
          bgcolor = "#f9f9f9",
          borderwidth = 1,
          bordercolor = "#ccc",
          steps = list(
            list(range = c(0, 50), color = "#5cb85c"),
            list(range = c(50, 75), color = "#f0ad4e"),
            list(range = c(75, 100), color = "#d9534f")
          )
        ),
        domain = list(x = c(0, 1), y = c(0, 1))
      ) %>%
        layout(
          margin = list(l = 10, r = 10, t = 10, b = 10),
          paper_bgcolor = "#fdfdfd"
        )
    }
  })
  

  
  
  # ---------------------------------------------
  # Plot: Viser hvilke faktorer der påvirker churn i simulation
  # ---------------------------------------------
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
  
  # ---------------------------------------------
  # Reactive: Angiver om der er valgt en virksomhed i tabellen
  # ---------------------------------------------
  output$noCompanySelected <- reactive({
    is.null(input$dashboard_table_rows_selected) || length(input$dashboard_table_rows_selected) == 0
  })
  outputOptions(output, "noCompanySelected", suspendWhenHidden = FALSE)
  
  # ---------------------------------------------
  # Reactive: Samler inputs til en ny simuleret virksomhed
  # Bruges ved klik på "Kør Simulation"
  # ---------------------------------------------
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