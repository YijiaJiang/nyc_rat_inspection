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
library(vroom)

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
rat_tidy <- read_csv('./www/rat_data.csv')

# # Model
# model_linear_original = lm(borough_monthly_cases ~ inspection_month + borough + covid_yn + avg_prcp + avg_snwd + avg_tmax, data = rat_tidy) 

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
               Under the tab of Rat Population Prediction, by choosing the location and year, you will get a predicted number base on our model."
        ),
        h3(strong("Interactive Map:")),
        p("Want to know how many rats live in the same area as you in the corner that you never noticed? Come to the Interactive Map and move your mouse to explore!")
      ),
      tabItem(
        tabName = "prediction",
        fluidRow(
          box(
            title = strong("Inputs"), status = "warning", width = 6,
            p('According to our model, the monthly observed rat population is related to borough, month, precipitation (tenths of mm),
              snow depth (mm), and maximum temperature. Now make your choices and input the data you interested in to explore.'),
            selectInput("borough1", "Choose borough:", 
                        choices = unique(rat_tidy$borough),
                        selected = NULL
                        ),
            selectInput('month', "Choose month:",
                        choices = unique(rat_tidy$inspection_month),
                        selected = NULL),
            numericInput("prcp", "Input precipitation (tenths of mm):", ' ', min = 0),
            numericInput("snwd", "Input snow depth (mm)", ' ', min = 0),
            numericInput("tmax", "Input maximum temperature", ' ', min = 0)
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
            selectInput("year2", "Choose year:", '')
          ),
          box(
            title = "Interactive Map", status = "primary", width = 9,
            leafletOutput('int_map')
          )
        )
      ),
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
          "Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J,
          Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43),
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
          "Wickham H, François R, Henry L, Müller K (2022). _dplyr: A Grammar of Data Manipulation_. R package version 1.0.10, <https://CRAN.R-project.org/package=dplyr>."
        ),
        p(
          class = "hangingindent",
          "Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software, 40(1), 1-29. URL https://www.jstatsoft.org/v40/i01/."
        ),
        p(
          class = "hangingindent",
          "Perrier V, Meyer F (2020). _fresh: Create Custom 'Bootstrap' Themes to Use in 'Shiny'_. R package version 0.2.0, <https://CRAN.R-project.org/package=fresh>."
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  output$rat_num <- renderText({
    # Model
    model_linear_original = lm(borough_monthly_cases ~ inspection_month + borough + covid_yn + avg_prcp + avg_snwd + avg_tmax, data = rat_tidy) 
    
    req(input$prcp)
    req(input$snwd)
    req(input$tmax)
    pred1 = predict(model_linear_original, 
                    newdata = data.frame(inspection_month = input$month, borough = input$borough1, covid_yn = 0, avg_prcp = input$prcp, avg_snwd = input$snwd, avg_tmax = input$tmax))
    return(round(as.numeric(pred1), digits = 0))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
