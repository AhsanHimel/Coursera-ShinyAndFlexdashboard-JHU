library(shiny)
library(tidyverse)

#####Import Data

dat<-read_csv("cces_sample_coursera.csv")
dat<- dat %>% select(c("pid7","ideo5"))
dat<-drop_na(dat)

ui<- fluidPage(
  titlePanel("Distribution of Party Membership of Survey Respondents", 
             windowTitle = "Project 1"),
  
  sliderInput(inputId = "ideology",
              label = "Select Five Point Ideology (1=Very liberal, 5=Very conservative)",
              min = 1, max = 5, value = 3
              ),
  plotOutput(outputId = "bar"),
  downloadButton("download", label = "Download Code")
)
  

server<-function(input,output){
  output$bar <- renderPlot({
    dat <- filter(dat, ideo5 == input$ideology)

    ggplot(dat) +
      geom_bar(aes(x = pid7)) +
      labs(x = "7 Point Party ID, 1=Very D, 7=Very R",
           y = "Count", caption = "Data Source: studycces.gov.harvard.edu")
  })
  
  output$download <- downloadHandler(
    filename <- function() {
      paste("Code for project 1", "R", sep=".")
    },
    content <- function(file) {
      file.copy("c4_peer1_solution_starter.R", file)
    },
    contentType = "application/zip"
  )
}

shinyApp(ui,server)
