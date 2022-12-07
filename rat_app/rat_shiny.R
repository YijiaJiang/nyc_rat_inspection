library(shiny)
library(shinythemes)
library(tidyverse)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plyr)
library(fresh)

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

# UI
ui <- dashboardPage(
  dashboardHeader(title = strong("NYC Rat Inspection"),
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
            title = "Inputs", status = "warning", width = 6,
            selectInput("area1", "Choose area:", ''),
            selectInput("year1", "Choose year:", '')
          ),
          box(
            title = "The rat population will be...", status = "primary", width = 6
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
            title = "The rat population will be...", status = "primary", width = 9
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
  
  # # Download sample data
  # output$download_sample_data = downloadHandler(
  #   filename = "sample_data.csv",
  #   content = function(file) {
  #     file.copy("./www/swimmer_data.csv", file)
  #   }
  # )
  # 
  # #show sample dataset
  # output$ex_table = renderDataTable({
  #   ex = #data.frame(
  #     tibble(
  #       ID = c(1, 2, 3, 3, 4, 4, 5, 5, 5, 6), 
  #       Event = c("Disease Progression", "Disease Progression", "Dose Reduced", 
  #                 "Consent Withdrawn", "Dose Reduced", "Disease Progression",
  #                 "Dose Reduced", "Dose Reduced", "Study Complete", "Disease Progression"),
  #       Event_Time = c(7.14, 8.14, 3.00, 6.71, 9.00, 99.43, 44.00, 47.00, 118.71, 7.57), 
  #       HR = c(rep("HR Positive", 10))
  #     )
  #   return(ex)
  # })
  # 
  # # Table display of uploaded file
  # output$uploaded_table = renderTable({
  #   req(input$file)
  #   return(head(data_raw(), 30))
  # })
  # 
  # #read in data
  # data_raw = reactive({
  #   if (is.null(input$file)) {
  #     return(NULL)
  #   } 
  #   else if (file_ext(input$file$datapath) == "csv"){
  #     read.csv(input$file$datapath, na.strings = "")
  #   } else if (file_ext(input$file$datapath) == "xlsx" | file_ext(input$file$datapath) == "xls"){
  #     read_excel(input$file$datapath)
  #   }else (return(NULL))
  # })
  # 
  # # Process data for plotting
  # data_clean = reactive({
  #   req(data_raw())
  #   
  #   data = data_raw()
  #   keeps = c('id', 'outcome', 'time', 'group')
  #   
  #   # Set up variables
  #   data$id = data %>% pull(input$id)
  #   data$outcome = data %>% pull(input$outcome)
  #   data$time = data %>% pull(input$time)
  #   data$group = factor(data %>% pull(input$group))
  #   
  #   data$id = as.factor(data$id)
  #   data$outcome = as.factor(data$outcome)
  #   data$time = as.numeric(data$time)
  #   data$group = as.factor(data$group)
  #   
  #   data=data[keeps]
  #   # Add change from baseline variable
  #   data %>%
  #     
  #     group_by(id) %>%
  #     arrange(id, time) %>%
  #     ungroup() %>%
  #     mutate(id = as.factor(id)) %>%
  #     mutate(id = fct_reorder(id, time)) %>%
  #     as.data.frame()
  # })
  # 
  # data_clean2 = reactive({
  #   data = data_clean()
  #   
  #   data %>%
  #     group_by(id) %>%
  #     filter(time == max(time, na.rm=TRUE)) %>%
  #     ungroup() %>%
  #     as.data.frame()
  # 
  # })
  # 
  # 
  # # Update event variable selector
  # observeEvent(data_raw(), {
  #   updateSelectInput(session, "outcome", choices = names(data_raw()), selected = "NULL")
  # })
  # 
  # # Update time variable selector
  # observeEvent(data_raw(), {
  #   updateSelectInput(session, "time", choices = names(data_raw()), selected = "NULL")
  # })
  # 
  # # Update group variable selector
  # observeEvent(data_raw(), {
  #   updateSelectInput(session, "group", choices = names(data_raw()), selected = "NULL")
  # }) 
  # 
  # # Update ID variable selector
  # observeEvent(data_raw(), {
  #   updateSelectInput(session, "id", choices = names(data_raw()), selected = "NULL")
  # }) 
  # 
  # # swimmer plot
  # swimmer_plot = reactive({
  # 
  #   
  #   plot = ggplot()+
  #     #time frame
  #     geom_col(data = data_clean2(), aes(x=fct_reorder(id, time), y=time, fill=group)) +
  #     coord_flip() +
  #     #event point
  #     geom_point(data = data_clean(), aes(x=id, y=time, shape=outcome), size = input$symbolsize) +
  #     #axis and title
  #     labs(x = input$yaxis_label,
  #          y = input$xaxis_label,
  #          title = input$plot_title,
  #          shape = " ",
  #          color = " ") +
  #     theme_minimal()+
  #     theme_bw()+
  #     #font size and background
  #     guides(fill=guide_legend(title=" "))+
  #     theme(panel.grid.minor = element_blank(),
  #           panel.grid.major = element_blank(),
  #           panel.background = element_blank(),
  #           plot.title = element_text(hjust = 0.5, size = 16),
  #           axis.text=element_text(size=16),
  #           axis.title=element_text(size=16),
  #           legend.text = element_text(size=16))
  #   
  #   #color scheme
  #   if(input$color_scheme == 'def'){
  #     plot = plot + scale_fill_manual(values=c("#1d4f91", "#FF9800", "#228848", '#AE2573', '#D14124'))
  #   }
  #   if(input$color_scheme == 'blue'){
  #     plot = plot + scale_fill_manual(values=c("#163c6f", "#005c99", "#2a74d5", '#6198e0', '#9dbfec'))
  #   }
  #   if(input$color_scheme == 'grey'){
  #     plot = plot + scale_fill_manual(values=c("#c3bcb7", "#a0968d", "#757575", '#5f564f', '#403a35'))
  #   }
  #   
  #   #indicator variable
  #   if(input$referenceyesno == 'Yes'){
  #     #Reference Line
  #     plot = plot + geom_hline(yintercept = input$reference1, show.legend = FALSE, linetype = "dashed", size=1)
  #   }
  #   return(plot)
  #   
  # })
  # 
  # observe({
  #   output$swimmer_plot <- renderPlot({
  #     swimmer_plot() }, height = input$figureheight, width = input$figurewidth)
  # })
  # 
  # #download
  # output$download = downloadHandler(
  #   filename = function() {
  #     paste("swimmer_plot", input$extension, sep = ".")
  #   },
  #   content = function(file){
  #     ggsave(file, swimmer_plot(), width = 12, height = 8, dpi = input$dpi, device = input$extension, limitsize = FALSE)
  #   }
  # )
}

# Run the application 
shinyApp(ui = ui, server = server)
