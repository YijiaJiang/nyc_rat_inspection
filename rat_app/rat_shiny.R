library(shiny)
library(shinythemes)
library(tidyverse)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(dplyr)
library(fresh)
library(leaflet)
library(plotly)
library(htmlwidgets)
library(shinyWidgets)
library(rgdal)

# Create theme
rat_theme <- create_theme(
  adminlte_color(
    light_blue = "#40BC9C"
  ),
  adminlte_sidebar(
    width = "230px",
    dark_bg = "#2B3E4F",
    dark_color = "#FFFFFF"
  ),
  adminlte_global(
    content_bg = "#FFFFF"
  ),
  theme = c('flatly')
)

# Data 
rat_tidy <- read_csv('./www/rat_linear_1.csv') %>%
  mutate(inspection_month_n = as.numeric(inspection_month_n)) %>% 
  mutate(month = factor(month.name[inspection_month_n], levels = month.name)) %>% 
  arrange(month)
#rat_binary <- read_csv('./www/rat_logistic.csv')
nyc_boro = readOGR("./www/geo_export_2204bc6b-9c17-46ed-8a67-7245a1e15877.shp", layer = "geo_export_2204bc6b-9c17-46ed-8a67-7245a1e15877")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "NYC Rat Inspection",
                    tags$li(
                      class = "dropdown",
                      tags$a(href = 'https://yijiajiang.github.io/nyc_rat_inspection/index.html',
                             icon("house")
                             )
                      ),
                  tags$li(
                    class = "dropdown",
                    tags$a(icon("github"),
                           href = "https://github.com/YijiaJiang/nyc_rat_inspection"
                           )
                    )
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("App Description", tabName = "description", icon = icon("chalkboard-user")),
      menuItem("Rat Population Prediction", tabName = "prediction", icon = icon("arrow-trend-up")),
      #menuItem("Whether Rat Prediction", tabName = "log_pred", icon = icon("arrow-trend-down")),
      menuItem("Interactive Map", tabName = "inter_map", icon = icon("map")),
      menuItem("Reference", tabName = "reference", icon = icon("leanpub"))
    )
  ),
  dashboardBody(
    use_theme(rat_theme),
    tabItems(
      tabItem(
        tabName = "description",
        h1(strong("Welcome to the NYC Rat Inspection App")),
        hr(),
        h3(strong("Rat Population Prediction:")),
        p("Are you curious about the number of rats in NYC in the future? Can you imagine a world that human must get along well with rats in NYC? 
               Under the tab of Rat Population Prediction, by inputting your own value, you will get a predicted number base on our model."
        ),
        h3(strong("Interactive Map:")),
        p("Want to know how many rats live in the same area as you in the corner that you never noticed? Come to the Interactive Map and move your mouse to explore!")
      ),
      tabItem(
        tabName = "prediction",
        fluidRow(
          box(
            title = strong("Inputs"), status = "warning", width = 6,
            p('According to our model, the monthly observed rat population is related to borough, month, precipitation (mm),
              snow depth (mm), and maximum temperature(\u00B0C). Now make your choices and input the data you interested in to explore.'),
            selectInput("borough1", "Choose borough:", 
                        choices = unique(rat_tidy$borough),
                        selected = NULL
                        ),
            selectInput('month', "Choose month:",
                        choices = unique(rat_tidy$month),
                        selected = NULL),
            numericInput("prcp", "Input precipitation (mm):", ' ', min = 0),
            numericInput("snwd", "Input snow depth (mm)", ' ', min = 0),
            numericInput("temp", "Input average temperature (\u00B0C)", ' ', min = 0),
            radioButtons("covid", "Still Covid?",
                         c("Yes" = 1,
                           "No" = 0)),
          ),
          box(
            title = strong("The rat population will be..."), status = "primary", width = 6,
            h1(strong(htmlOutput("rat_num")))
            )
        )
      ),
      tabItem(
        tabName = "inter_map",
        fluidRow(
          box(
            title = "Inputs", status = "warning", width = 3,
            selectInput("year2", "Choose year:", 
                        choices = unique(rat_tidy$inspection_year),
                        selected = NULL),
            selectInput("month2", "Choose month:",
                        choices = unique(rat_tidy$month),
                        selected = NULL)
            ),
          box(
            title = "Interactive Map", status = "primary", width = 9, height = '100%',
            leafletOutput('int_map', width = '100%', height = 540)
          )
        )
      ),
      # tabItem(
      #   tabName = "log_pred",
      #   fluidRow(
      #     box(
      #       title = strong("Inputs"), status = "warning", width = 6,
      #       p('According to our model, the answer of  question (whether there will be a rat?) is related to borough, daytime, precipitation (mm),
      #         and snow depth (mm). Now make your choices and input the data you interested in to explore.'),
      #       selectInput("borough2", "Choose borough:", 
      #                   choices = unique(rat_binary$borough),
      #                   selected = NULL
      #       ),
      #       selectInput('daytime2', "Choose daytime:",
      #                   choices = unique(rat_binary$inspection_daytime),
      #                   selected = NULL),
      #       selectInput('feel2', "How do you feel about the weather?",
      #                   choices = unique(rat_binary$feeling),
      #                   selected = NULL),
      #       radioButtons("prcp2", "Whether there is precipitation:",
      #                    c("Yes" = 1,
      #                      "No" = 0)),
      #       radioButtons("snow2", "Snowing?",
      #                    c("Yes" = 1,
      #                      "No" = 0)),
      #       radioButtons("snwd2", "Whether snow left on the street?",
      #                    c("Yes" = 1,
      #                      "No" = 0))
      #     ),
      #     box(
      #       title = strong("Will there be rats?"), status = "primary", width = 6,
      #       h1(strong(htmlOutput("whether_rat")))
      #     )
      #   )
      # ),
      tabItem(
        tabName = "reference",
        h3(strong('Reference')),
        p(
          class = "hangingindent",
          "Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2022). _shiny: Web Application Framework for R_. R package
          version 1.7.2, <https://CRAN.R-project.org/package=shiny>."
        ),
        p(
          class = "hangingindent",
          "Chang W (2021). _shinythemes: Themes for Shiny_. R package version 1.2.0, <https://CRAN.R-project.org/package=shinythemes>."
        ),
        p(
          class = "hangingindent",
          "Wickham H, Averick M, Bryan J, Chang W, McGowan LD, Fran??ois R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, M??ller K, Ooms J,
          Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). ???Welcome to the tidyverse.??? _Journal of Open Source Software_, *4*(43),
          1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>."
        ),
        p(
          class = "hangingindent",
          "Chang W, Borges Ribeiro B (2021). _shinydashboard: Create Dashboards with 'Shiny'_. R package version 0.7.2, <https://CRAN.R-project.org/package=shinydashboard>."
        ),
        p(
          class = "hangingindent",
          "H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016."
        ),
        p(
          class = "hangingindent",
          "Wickham H, Fran??ois R, Henry L, M??ller K (2022). _dplyr: A Grammar of Data Manipulation_. R package version 1.0.10, <https://CRAN.R-project.org/package=dplyr>."
        ),
        p(
          class = "hangingindent",
          "Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software, 40(1), 1-29. URL https://www.jstatsoft.org/v40/i01/."
        ),
        p(
          class = "hangingindent",
          "Perrier V, Meyer F (2020). _fresh: Create Custom 'Bootstrap' Themes to Use in 'Shiny'_. R package version 0.2.0, <https://CRAN.R-project.org/package=fresh>."
        ),
        p(
          class = "hangingindent",
          "Cheng J, Karambelkar B, Xie Y (2022). _leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library_. R package version 2.1.1, <https://CRAN.R-project.org/package=leaflet>."
        ),
        p(
          class = "hangingindent",
          "C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida, 2020."
        ),
        p(
          class = "hangingindent",
          "Vaidyanathan R, Xie Y, Allaire J, Cheng J, Sievert C, Russell K (2021). _htmlwidgets: HTML Widgets for R_. R package version 1.5.4, <https://CRAN.R-project.org/package=htmlwidgets>."
        ),
        p(
          class = "hangingindent",
          "Perrier V, Meyer F, Granjon D (2022). _shinyWidgets: Custom Inputs Widgets for Shiny_. R package version 0.7.5, <https://CRAN.R-project.org/package=shinyWidgets>."
        ),
        p(
          class = "hangingindent",
          "Bivand R, Keitt T, Rowlingson B (2022). _rgdal: Bindings for the 'Geospatial' Data Abstraction Library_. R package version 1.6-2, <https://CRAN.R-project.org/package=rgdal>."
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$rat_num <- renderText({
    # Model
    model_linear_original = lm(log(borough_monthly_cases) ~ month + borough + covid_yn + avg_prcp + avg_snwd + avg_temp, data = rat_tidy) 
    
    req(input$prcp)
    req(input$snwd)
    req(input$temp)
    pred1 = predict(model_linear_original, 
                    newdata = data.frame(month = input$month, borough = input$borough1, covid_yn = as.numeric(input$covid), avg_prcp = input$prcp, avg_snwd = input$snwd, avg_temp = input$temp))
    return(round(as.numeric(exp(pred1)), digits = 0))
  })
  
  # output$whether_rat <- renderText({
  #   # Model
  #   model_logit = glm(inspection_result ~ borough + covid_yn + inspection_daytime + prcp_yn + snwd_yn + snow_yn + feeling , data = rat_binary, family="binomial")
  # 
  #   req(input$prcp2)
  #   req(input$snwd2)
  #   pred2 = predict(model_logit, newdata = data.frame(borough = input$borough2, covid_yn = 0, inspection_daytime = input$daytime2, prcp_yn = as.numeric(input$prcp2), snwd_yn = as.numeric(input$snwd2), snow_yn = as.numeric(input$snow2), feeling = input$feel2), type = "response")
  #   pred3 <- ifelse(pred2 > 0.75, "Oops, Rat!", ifelse(pred2 < 0.25, "Congrat, no rats!", 'Emmm, not sure...'))
  #   return(pred3)
  # })
  
  output$int_map <- renderLeaflet({
    
    real_rat <-  rat_tidy %>% 
      filter(
        inspection_year == input$year2,
        month == input$month2
      )
    
    real_rat %>% 
      leaflet() %>% 
      addTiles() %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(data = nyc_boro,
                  weight = 0.85,
                  label = ~paste(nyc_boro@data$boro_name, ' Rat population:', real_rat$borough_monthly_cases),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", 
                                 padding = "1px 2px"),
                    textsize = "11px",  sticky = TRUE,
                    opacity = 0.55),
                  fillColor = c("#38C5A3", "#F09968", "#8DA0CB", "#EE90BA", "#A8D14F"),
                  stroke = FALSE,
                  opacity = 1,
                  smoothFactor = .5,
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(weight = 5, 
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)
      ) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
