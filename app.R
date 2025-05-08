pacman::p_load(shiny,leaflet,dplyr,readr,shinyWidgets,
  DT,ggplot2,tibble,tidymodels,auth0,ggforce,scales,ranger)

readRenviron("Renviron.sh")
options(shiny.port = 8080)
Sys.getenv("AUTH0_USER")

# 1.2 Datasæt og churn-model
full_results <- readRDS("data/full_results.rds")          
final_model <- readRDS("model/final_churn_model.rds") 

# 1.5 Medlemskabspris - Antal ansatte
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

# 1.6 Join datasæt 
data_map <- full_results %>%
  mutate(
    risk_category = case_when(
      churn_prob > 0.75 ~ "High",
      churn_prob > 0.5 ~ "Medium",
      TRUE ~ "Low"
    ),
    risk_category = factor(risk_category, levels = c("High", "Medium", "Low")),
    

    membership_fee = if_else(churn == 0, calculate_membership_fee(Employees), NA_real_)
  )

# Farver til risikokategorier
risk_pal <- c(
  "Lav" = "darkgreen",     # grøn
  "Medium" = "#f0ad4e",  # gul/orange
  "Høj" = "#B43C37"     # rød
)


# 2.1 UI
ui <- fluidPage(

# 2.2 CSS 
tags$head(
  tags$style(HTML("

body {
  background:#fff;
  font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;
  zoom:.8;
}
.small-paragraph {
  font-size:16px;
  margin-top:10px;
}
.top-bar {
  padding:15px 30px;
  display:flex;
  align-items:center;
  justify-content:space-between;
  border-bottom:2px solid #eee;
}
.main_tabs {
  background:linear-gradient(to bottom,#0e2f33 0%,#2a6c73 100%);
  padding:10px 2px;
  text-align:center;
  margin-bottom:2px;
}
.main_tabs .tab-content {
  background:#0e2f33;
}
.tab-content {
  display:flex;
  justify-content:flex-start;
  margin-top:25px;
}
.nav-tabs {
  background:transparent!important;
  border:none!important;
  margin:0;
  padding-left:30px;
}
.nav-tabs>li>a {
  color:#fff!important;
  font-weight:500;
  border:none!important;
}
.nav-tabs>li.active>a,
.nav-tabs>li.active>a:focus,
.nav-tabs>li.active>a:hover {
  background:transparent!important;
  border:none!important;
  border-bottom:3px solid #7fc8a3!important;
  color:#fff!important;
  font-weight:bold;
}
.btn:hover {
  background:#0e2f33!important;
}
.btn-group .btn {
  background:#e0e0e0!important;
  color:#333!important;
  border-radius:5px!important;
}
.btn-group .btn.active,
.btn-group .btn:active,
.btn-group .btn:focus {
  background:#0e2f33!important;
  color:#fff!important;
  font-weight:600;
}
.custom-btn {
  background:#325e63!important;
  color:#fff!important;
  font-weight:700;
  border:none;
  border-radius:10px;
  padding:20px;
  margin-top:5px;
  display:block;
  width:100%;
  text-align:center;
}
.info-box-container {
  display:flex;
  flex-wrap:wrap;
  gap:15px;
  margin-bottom:20px;
  justify-content:flex-start;
}
.info-box {
  flex:1 1 calc(14.28% - 20px);
  background:#325e63;
  border-radius:10px;
  min-width:180px;
  padding:20px;
  text-align:center;
  box-shadow:0 2px 5px rgba(0,0,0,.2);
}
.info-box h4,
.info-box p {
  font-weight:400;
  margin:0;
}
.info-box h4 {
  font-size:13px;
  color:#fff;
  margin-bottom:6px;
}
.info-box p {
  font-size:18px;
  color:#325e63;
}
.info-box2,
.info-box3 {
  text-align:center;
  margin-bottom:10px;
  border-radius:8px;
}
.info-box2 {
  background:#173234;
  color:#fff;
  padding:12px;
}
.info-box3 {
  background:#f5f5f5;
  color:#173234;
  padding:10px 20px;
  border-radius:6px;
  font-weight:700;
  margin-top:0;
  font-size:26px;
}
.info-box h2, .info-box2 h2 {
  font-size: 32px;
  line-height: 1;
  margin-top: 10px;
  margin-bottom: 0;
  font-weight: bold;
  min-height: 40px;
}
.dashboard-wrapper {
  padding:0 80px;
}
.sidebarPanel {
  background:#fff;
  padding:30px;
  border-radius:10px;
  border-left:10px solid #2a6c73;
}
.filter-box {
  background:#fff;
  color:#000;
  padding:25px;
  border-radius:10px;
  border:1px solid #ddd;
  box-shadow:0 2px 5px rgba(0,0,0,.05);
  margin-top:20px;
  text-align:left;
}
.bootstrap-select .dropdown-toggle {
  background:#0e2f33!important;
  color:#fff!important;
  border:1px solid #0e2f33!important;
  font-weight:500;
  border-radius:5px;
  padding:6px 12px;
}
.bootstrap-select .dropdown-toggle:focus {
  box-shadow:0 0 0 .15rem rgba(26,78,99,.5)!important;
  outline:none!important;
}
.bootstrap-select .dropdown-toggle:hover {
  background:#0e2f33!important;
}
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-from,
.irs-to {
  background:#0e2f33!important;
  border-color:#1f3e47!important;
}
#dashboard_table,
#dashboard_table * {
  color:#fff!important;
}
#dashboard_table,
.dataTables_wrapper {
  height:100%;
  min-height:100%;
  width:100%!important;
}
.dataTables_wrapper .dataTables_scrollBody {
  max-height:none!important;
}
#dashboard_table td {
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  max-width: 300px;
}
#dashboard_table th {
    white-space: nowrap !important;
  }
"))
),

# 2.3 Top-bar
 
div(class = "top-bar",

  div(style = "display: flex; align-items: center; gap: 15px;",
        img(src = "businessviborgny.jpeg", height = "80px")
    ),
  
   div(style = "text-align: center; flex-grow: 1;",
        h2(strong("Velkommen til medlemsindsigt", 
                  style = "font-size: 30px; margin: 0; color: #0e2f33;")),
        p("Overvåg og analyser churn risiko blandt medlemmer",
          style = "color: #88c5aa; font-size: 18px; margin: 0;")
),
div(style = "display: flex; flex-direction: column; align-items: flex-end;",
    div(style = "display: flex; align-items: center; gap: 8px; margin-bottom: 5px;",
        icon("user"),
        span(textOutput("user_name"), style = "margin-left: 5px;"),
        logoutButton(label = "Log ud", style = "background-color: #dc3545; color: white; border: none;")
    ),
    textInput("search", NULL, placeholder = "Søg...", width = "200px")
   )
   ),

# 2.5 Tabs

div(class = "main_tabs",
    
tabsetPanel(
      id = "main_tabs", 
      
      tabPanel("Dashboard",
               
               fluidRow(
                 
                 column(2,
                        div(class = "filter-box",
                            h3("Strategisk ledelsesoverblik"),
                            h4(strong("Vigtige nøgletal")),
                            p("Dashboardet giver et solidt, datadrevet fundament for strategiske beslutninger om medlemsudvikling og fastholdelse. Få indsigt i churn-risiko og økonomi.",
                              style = "font-size:14px;")
                        )
                        
                 ),
                 column(9,
                        div(style = "display:flex; flex-wrap:nowrap; gap:12px; margin-bottom:10px; padding-left:20px;",
                            lapply(list(
                              list(icon="sign-out-alt", title="Churned", text="Hele perioden", output="info_churned_members", bg="#B43C37"),
                              list(icon="minus-circle", title="Tab pga. churn", text="Om året", output="churn_loss_box_churned", bg="#B43C37"),
                              list(icon="exclamation-triangle", title="Potentiel churn", text="Over 75% risiko", output="info_high_risk_members", bg="#B43C37"),
                              list(icon="exclamation-triangle", title="Potentielt tab", text="Om året", output="churn_loss_box", bg="#B43C37"),
                              list(icon="percent", title="Risiko score", text="Gennemsnitlig", output="info_avg_risk_score", bg="#B43C37"),
                              list(icon="clock", title="Loyalitet", text="Gennemsnitlig", output="info_avg_loyalty", bg="#173234"),
                              list(icon="users", title="Medlemmer", text="Antal Aktive", output="info_total_members1", bg="#173234"),
                              list(icon="money-bill-wave", title="Indtjening", text="Om måneden", output="budget_box", bg="#173234"),
                              list(icon="money-bill-wave", title="Indtjening", text="Om året", output="budget_box1", bg="#173234")
                            ), function(x) {
                              div(style = paste0("flex:1; padding:20px; background-color:", x$bg, "; color:white; border-radius:10px; text-align:center;"),
                                  icon(x$icon, style = "font-size:24px; color:white; margin-bottom:10px;"),
                                  h4(x$title), p(x$text),
                                  h2(textOutput(x$output))
                              )
                            })
                        )
                 )
                
                 
               ),
               
               fluidRow(
                 column(4, 
                        div(style = "background:#f8f9fa; padding:20px; border-radius:5px; margin-top:40px;margin-bottom:60px;",
                            h4("Churn-drivere", style = "color:#0e2f33;"),
                            plotOutput("simulation_factors_dashboard", height = "370px")
                        )
                 ),
                 column(4, 
                        div(style = "background:#f8f9fa; padding:20px; border-radius:5px; margin-top:40px;margin-bottom:60px;",
                            h4("Churn pr. postnummer", style = "color:#0e2f33;"),
                            plotOutput("postal_code_summary", height = "370px")
                        )
                 ),
                 column(4, 
                        div(style = "background:#f8f9fa; padding:20px; border-radius:5px;margin-top:40px;margin-bottom:60px;",
                            h4("Churn pr. virksomhedstype", style = "color:#0e2f33;"),
                            plotOutput("company_type_churn", height = "370px")
                        )
                 )
               )
               
               ),
     
    
      
tabPanel("Simulation",
         fluidRow(
           column(6, wellPanel(
             style = "background:#f8f9fa;border-radius:8px;padding:20px;color:#000;",
             h4("Simuler Churn", style = "color:#0e2f33;"),
             numericInput("sim_employees", "Antal ansatte:", 10, 1, 500),
             pickerInput("sim_postal", "Postnummer:", unique(data_map$PostalCode)),
             pickerInput("sim_company_type", "Virksomhedstype:", levels(data_map$CompanyTypeName)),
             radioGroupButtons("sim_contact", "Kontakt:", c("Ja" = "Ja", "Nej" = "Nej"), "Ja"),
             radioGroupButtons("sim_event", "Event:", c("Ja" = "Ja", "Nej" = "Nej"), "Nej"),
             sliderInput("sim_member_years", "Medlemsår:", 0, 20, 2),
             pickerInput("sim_branche", "Branche:", levels(data_map$Branche_navn)),
             numericInput("sim_meeting_length", "Mødelængde (min):", 60, 0, 300),
             actionButton("run_simulation", "Kør simulation", class = "btn-primary", icon = icon("play"))
           )),
           
           column(6, wellPanel(
             style = "background:#fff;border-radius:8px;min-height:400px;padding:20px;color:#000;",
             h4("Resultat", style = "color:#0e2f33;"),
             div(style = "text-align:center;margin-top:30px;",
                 htmlOutput("simulation_result"),
                 plotOutput("simulation_gauge", height = "200px")
             )
           ))
         )
),

tabPanel("Indsigt", 
         div(style = "padding: 2px 40px;", 
             fluidRow(
               column(3,
                      div(style = "background-color: white; color: #000; padding: 20px; border-radius: 10px; text-align: left;",
                      h5("Medlemskonsulent"),
                      h3(strong("Vælg dine indsigter")),
                      p("Brug værktøjerne nedenfor til at filtrere og analysere churn-risiko blandt virksomheder.", style = "font-size: 14px;"),
                      checkboxGroupInput("risk_categories", "Risikoniveau:", 
                                         choices = c("Høj" = "High", "Mellem" = "Medium", "Lav" = "Low"),
                                         selected = c("High", "Medium", "Low")),
                      pickerInput("postal_code", "Postnummer:", 
                                  choices = c("Vælg alle" = "ALL", sort(unique(data_map$PostalCode))),
                                  multiple = TRUE, options = list(actionsBox = TRUE)),
                      pickerInput("CompanyTypeName", "Virksomhedstype:", 
                                  choices = c("Vælg alle" = "ALL", sort(unique(as.character(data_map$CompanyTypeName)))),
                                  multiple = TRUE, options = list(actionsBox = TRUE)),
                      pickerInput("Branche_navn", "Branche:", 
                                  choices = c("Vælg alle" = "ALL", sort(unique(as.character(data_map$Branche_navn)))),
                                  multiple = TRUE, options = list(actionsBox = TRUE)),
                      sliderInput("churn_range", "Churn sandsynlighed (%):", 0, 100, c(0, 100)),
                      )
               ),
               
               column(9,
                      fluidRow(
                        column(9,
                               div(style = "display:flex; flex-wrap:nowrap; gap:12px; margin-bottom:10px;",
                                   lapply(list(
                                     list(icon="phone-slash", title="Manglende kontakt", text="Aktive virksomheder", output="info_no_contact", bg="#B43C37"),
                                     list(icon="medal", title="Manglende deltagelse", text="I event", output="info_no_event", bg="#B43C37"),
                                     list(icon="building", title="Medlemmer", text="Antal aktive", output="info_total_members2", bg="#123940"),
                                     list(icon="calendar-alt", title="Næste event", text="04-06-2025", output=NULL, value="ESG", bg="#123940"),
                                     list(icon="calendar-check", title="Eventdeltagere", text="Næste event", output=NULL, value="86", bg="#123940"),
                                     list(icon="phone", title="Opkald i dag", text="Aktive virksomheder", output="info_calls_today", bg="#123940")
                                   ), function(x) {
                                     div(style = paste0("flex:1; padding:20px; background-color:", x$bg, "; color:white; border-radius:10px; text-align:center;"),
                                         icon(x$icon, style = "font-size:24px; color:white; margin-bottom:10px;"),
                                         h4(x$title), p(x$text),
                                         if (!is.null(x$output)) h2(textOutput(x$output)) else h2(x$value)
                                     )
                                   })
                               )
                        ),
                        column(3,
                               div(style = "background:#f8f9fa; padding:20px; border-radius:10px;",
                                   h4("Churn-drivere", style = "color:#0e2f33;"),
                                   plotOutput("simulation_factors", height = "150px")
                               )
                        )
                      ),
                      div(style = "height:750px;  overflow-y:auto;",
                          DTOutput("dashboard_table", width = "100%")
                      )
               )
             )
         )
)
 
  )
)
)


# 3.1 Server

server <- function(input, output, session) {
  
#Dashboard 
  output$info_total_members1 <- renderText({
    length(unique(data_map$PNumber[data_map$churn == 0]))
  })
  
  output$info_churned_members <- renderText({
    sum(data_map$churn == 1, na.rm = TRUE)
  })
  
  output$churn_loss_box_churned <- renderText({
    df <- data_map %>% filter(churn == 1)
    loss <- sum(calculate_membership_fee(df$Employees), na.rm = TRUE)
    monthly_loss <- loss / 12
    paste0(format(round(monthly_loss / 1e6, 2), decimal.mark = ","), " mio.")
  })
  
  
  output$info_high_risk_members <- renderText({
    sum(data_map$churn == 0 & data_map$churn_prob > 75, na.rm = TRUE)
  })
  
  output$info_avg_risk_score <- renderText({
    active <- subset(data_map, churn == 0 & !is.na(churn_prob))
    if (nrow(active) == 0) return("0%")
    
    avg <- mean(pmin(pmax(active$churn_prob, 0), 100))  
    paste0(round(avg, 1), "%")
  })

  output$info_avg_loyalty <- renderText({
    round(mean(data_map$medlem_antal_år, na.rm = TRUE), 1) %>% paste("år")
  })
  
  output$budget_box <- renderText({
    df <- data_map %>% filter(churn == 0)
    budget <- sum(calculate_membership_fee(df$Employees), na.rm = TRUE)
    monthly_budget <- budget / 12
    paste0(formatC(monthly_budget / 1e6, format = "f", digits = 2, big.mark = ".", decimal.mark = ","), " mio.")
  })
  
  output$budget_box1 <- renderText({
    df <- data_map %>% filter(churn == 0)
    budget <- sum(calculate_membership_fee(df$Employees), na.rm = TRUE)
    paste0(format(round(budget / 1e6, 2), decimal.mark = ","), " mio.")
  })
  
  output$churn_loss_box <- renderText({
    df <- data_map %>% filter(churn == 0, churn_prob > 75)
    loss <- sum(calculate_membership_fee(df$Employees), na.rm = TRUE)
    paste0(format(round(loss / 1e6, 2), decimal.mark = ","), " mio.")
  })
  
  output$simulation_factors_dashboard <- renderPlot({
    var_imp <- tibble(
      factor = c("Deltaget i event - Nej", "Deltaget i event - Ja", "Meeting Length", "Employees", "Medlem antal år"),
      importance = c(0.66, 0.65, 0.28, 0.26, 0.20)
    )
    
    ggplot(var_imp, aes(x = reorder(factor, importance), y = importance)) +
      geom_col(fill = "#0e2f33", width = 0.7) +
      coord_flip() +
      labs(x = "", y = "Relativ betydning") +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  })
  
  
# Indsigt

  output$dashboard_table <- renderDT({
    req(input$risk_categories)
    
    filtered_data <- data_map %>%
      filter(churn == 0) %>%
      mutate(
        risk_level = case_when(
          churn_prob > 75 ~ "High",
          churn_prob > 50 ~ "Medium",
          TRUE ~ "Low"
        )
      ) %>%
      filter(
       
        risk_level %in% input$risk_categories,
        
        if ("ALL" %in% input$postal_code || is.null(input$postal_code)) TRUE else PostalCode %in% input$postal_code,
        
        if ("ALL" %in% input$CompanyTypeName || is.null(input$CompanyTypeName)) TRUE else CompanyTypeName %in% input$CompanyTypeName,
        
        if ("ALL" %in% input$Branche_navn || is.null(input$Branche_navn)) TRUE else Branche_navn %in% input$Branche_navn,
        
        churn_prob >= input$churn_range[1],
        churn_prob <= input$churn_range[2]
      ) %>%
      select(PNumber, CompanyTypeName, Branche_navn, PostalCode, churn_prob, membership_fee ) %>%
      arrange(desc(churn_prob))
    
    datatable(
      filtered_data %>%
        rename(
          "P-nummer" = PNumber,
          "Virksomhedstype" = CompanyTypeName,
          "Branche" = Branche_navn,
          "Postnummer" = PostalCode,
          "Churn-risiko (%)" = churn_prob,
          "Medlemskab om året (kr)" = membership_fee
        ),
      options = list(
        pageLength = 9,
        autoWidth = TRUE,
        searching = FALSE,
        lengthChange = FALSE
      ),
      rownames = FALSE,
      selection = "single"
    )
    
  })
  
  output$info_total_members2 <- renderText({
    length(unique(data_map$PNumber[data_map$churn == 0]))
  })

  output$info_no_contact <- renderText({
    sum(data_map$churn == 0 & tolower(data_map$har_haft_kontakt) == "nej", na.rm = TRUE)
  })
  
  output$info_no_event <- renderText({
    sum(data_map$churn == 0 & tolower(data_map$deltaget_i_event) == "nej", na.rm = TRUE)
  })
  
  output$info_next_event <- renderText({
    "ESG" 
  })
  
  output$info_event_participants <- renderText({
    "36" 
  })
  
  output$info_calls_today <- renderText({
    "17"  
  })
  

#Plot
  output$postal_code_summary <- renderPlot({
    data_map %>%
      group_by(PostalCode) %>%
      summarise(avg_churn = mean(churn_prob, na.rm = TRUE), n = n()) %>%
      mutate(
        risk_category = case_when(
          avg_churn > 75 ~ "Høj",
          avg_churn > 50 ~ "Medium",
          TRUE ~ "Lav"
        )
      ) %>%
      arrange(desc(avg_churn)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(PostalCode, avg_churn), y = avg_churn, fill = risk_category)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = paste0(round(avg_churn, 1), "% (n=", n, ")")),
                hjust = -0.1, size = 3.5, color = "black") +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
      scale_fill_manual(values = risk_pal, name = "Risikokategori") +
      coord_flip() +
      labs(
        title = "Gennemsnitlig churn-sandsynlighed pr. postnummer",
        x = "Postnummer",
        y = "Gennemsnitlig churn (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "right",
        plot.margin = margin(10, 20, 10, 10)
      )
  
  
  })
  
  output$company_type_churn <- renderPlot({
    data_map %>%
      group_by(CompanyTypeName) %>%
      summarise(avg_churn = mean(churn_prob, na.rm = TRUE), n = n()) %>%
      mutate(
        risk_category = case_when(
          avg_churn > 75 ~ "Høj",
          avg_churn > 50 ~ "Medium",
          TRUE ~ "Lav"
        ),
        risk_category = factor(risk_category, levels = c("Høj", "Medium", "Lav"))
      ) %>%
      arrange(desc(avg_churn)) %>%
      ggplot(aes(x = reorder(CompanyTypeName, avg_churn), y = avg_churn, fill = risk_category)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = paste0(round(avg_churn, 1), "% (n=", n, ")")),
                hjust = -0.1, size = 3.5, color = "black") +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
      scale_fill_manual(values = risk_pal, name = "Risikokategori") +
      coord_flip() +
      labs(
        title = "Gennemsnitlig churn pr. virksomhedstype",
        x = "Virksomhedstype",
        y = "Gennemsnitlig churn (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "right",
        plot.margin = margin(10, 20, 10, 10)
      )
  })

  output$simulation_factors <- renderPlot({
    req(input$run_simulation)
    
    var_imp <- tibble(
      factor = c("Deltaget i event - Nej", "Deltaget i event - Ja", "Meeting Length", "Employees", "Medlem antal år"),
      importance = c(0.66, 0.65, 0.28, 0.26, 0.20)
    )
    
    ggplot(var_imp, aes(x = reorder(factor, importance), y = importance)) +
      geom_col(fill = "#0e2f33", width = 0.7) +
      coord_flip() +
      labs(x = "", y = "Relativ betydning") +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  })
  
#Simuliation-------------------------------------------------------------------- 

  output$simulation_result <- renderUI({
    if (input$run_simulation > 0) {
      isolate({
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
      div(
        style = "text-align: center; padding-top: 50px; color: #666;",
        icon("sliders-h", style = "font-size: 50px; margin-bottom: 20px;"),
        h4("Indtast parametre og klik på 'Kør Simulation'")
      )
    }
  })
  

  new_company <- reactive({
    tibble(
      Employees = input$sim_employees,
      PostalCode = factor(input$sim_postal, levels = levels(data_map$PostalCode)),
      CompanyTypeName = factor(input$sim_company_type, levels = levels(data_map$CompanyTypeName)),
      har_haft_kontakt = factor(input$sim_contact, levels = c("Ja", "Nej")),
      deltaget_i_event = factor(input$sim_event, levels = c("Ja", "Nej")),
      medlem_antal_år = input$sim_member_years,
      Branche_navn = factor(input$sim_branche, levels = levels(data_map$Branche_navn)),
      MeetingLength = input$sim_meeting_length,
      PNumber = 99999999
    )
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
}
# 4. Kørsel af Shiny-app
auth0::shinyAppAuth0(ui, server)