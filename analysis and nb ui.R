
library(shinydashboard)
shinyUI(fluidPage(
  tags$style("body{background-color:whitesmoke}", "h1{text-align:center;color:steelblue}"),

  	h1("STACK OVERFLOW ANALYSIS SYSTEM"),uiOutput("stack")

))

