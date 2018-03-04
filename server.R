options(java.parameters = "-Xss2048k")
library(rJava)
library(shiny)
source("D:/Coursera/function.R")
shinyServer(function(input, output) {
  output$suggest <- renderText({
    suggestNextWord(input$text)
  })
})