options(java.parameters = "-Xss2048k")
library(rJava)
library(shiny)
shinyUI(fluidPage(
  titlePanel("Next Word Suggestion App"),
  sidebarLayout(
    sidebarPanel(
      textInput("text",
                "Your text:",
                value = "")
    ),
    mainPanel(
        h5("Suggestion:"),
        textOutput("suggest")
    )
  )
))
