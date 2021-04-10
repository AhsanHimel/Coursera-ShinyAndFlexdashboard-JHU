library(shiny)
library(tidyverse)
library(plotly)
library(DT)

#####Import Data

dat<-read_csv("cces_sample_coursera.csv")
dat<- dat %>% select(c("pid7","ideo5","newsint","gender","educ","CC18_308a","region"))
dat<-drop_na(dat)

#####Make your app

ui <- navbarPage(
  title = "My Application",
  tabPanel(title = "Page 1",
           sidebarPanel(
             sliderInput(
               inputId = "year_input",
               label = "Select Five Point Ideology (1=Very liberal, 5=Very conservative)",
               min = min(dat$ideo5),
               max = max(dat$ideo5),
               value = 3
             )
           ),
           mainPanel(tabsetPanel(
             tabPanel(title = "Tab 1", plotOutput(outputId = "plot1")),
             tabPanel(title = "Tab 2", plotOutput(outputId = "plot2"))
           ))),
  tabPanel(title = "Page 2",
           sidebarPanel(
             checkboxGroupInput(inputId = "checkGroup", 
                                label = "Select Gender", 
                                choices = list("Male" = 1, "Female" = 2),
                                selected = 1)
           ),
           mainPanel(
             plotlyOutput(outputId = "plot3")
           )),
  tabPanel(title = "Page 3",
           sidebarPanel(
             selectInput(inputId = "select_region",
                         label = "Select Region",
                         choices = unique(dat$region),
                         multiple = T,
                         selectize = T,
                         selected = 1
                         )
             ),
           mainPanel(
             DTOutput(outputId = "table")
           )
           ),
  h6(downloadLink("code", label = "Source Code"), align="right"),
  h6(downloadLink("data", label = "Data"), align="right")
)
server <- function(input, output) {
  #####Hint: when you make the data table on page 3, you may need to adjust the height argument in the dataTableOutput function. Try a value of height=500
  output$plot1 <- renderPlot({
    dat %>%
      filter(ideo5 == input$year_input) %>%
      ggplot() + geom_bar(aes(pid7)) +
      expand_limits(y = c(0,100)) +
      labs(y = "Count", x = "7 Party Point, 1 = Very D, 7 = Very L")
  })
  
  output$plot2 <- renderPlot({
    dat %>% 
      filter(ideo5 == input$year_input) %>% 
      ggplot() + geom_bar(aes(CC18_308a)) +
      scale_x_discrete(limits = c(0:5)) +
      labs(y = "Count", x = "Trump Support")
  })
  
  output$plot3 <- renderPlotly({
    plot <- dat %>%  
      filter(gender %in% input$checkGroup) %>% 
      ggplot(aes(x = educ, y = pid7)) + 
      geom_jitter() +
      geom_smooth(method = "lm")
    ggplotly(plot)
  })
  
  output$table <- renderDT({
    subset(dat, region %in% input$select_region)
  })
  
  output$code <- downloadHandler(
    filename <- function() paste("Code for project 2", "R", sep="."),
    content <- function(file) file.copy("c4_peer2_starter.R", file),
    contentType = "application/zip"
  )
  
  output$data <- downloadHandler(
    filename <- function() paste("Data for project 2", "csv", sep="."),
    content <- function(file) file.copy("cces_sample_coursera.csv", file),
    contentType = "text/csv"
  )
} 

shinyApp(ui,server)