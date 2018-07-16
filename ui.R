library(shiny)
library(dplyr)
library(ggplot2)
library(plotrix)
library(data.table)


shinyUI(fluidPage( 
  tags$style("body{background-color:whitesmoke}", "h3{text-align:center;color:blue}"),
  titlePanel(tags$h3("STACK OVERFLOW ANALYSING SYSTEM")), sidebarLayout(
    sidebarPanel
    (height=300,
      
      fileInput("file","UPLOAD FILE",
                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
      tags$hr()
      
    )
    , 
    mainPanel
    (
      
      tabsetPanel
      (
        type = "tab",
        tabPanel( title="Home", h3("WELCOME TO STACK OVERFLOW ANALYSING SYSTEM"),textOutput("home"),tags$img(src = "ffff.png", height = 420, width= 810)),
        tabPanel( title = "View_Uploaded_File", h3("UPLOADED_DEVELOPER_DATA"), tableOutput("upload")),
        tabPanel("General Analysis", 
                 tabsetPanel(type = "tab",
                             tabPanel("Country",plotOutput("country"), downloadButton(outputId = "cGraph", label = "download the graph"), textOutput("cdes")),
                             tabPanel("Years_Of_Coding", plotOutput("comp_cd"),downloadButton(outputId = "comp_cd_Graph", label = "download the graph"), textOutput("comp_cd_des") ),
                             tabPanel("Company_Size", plotOutput("comp_sz"), downloadButton(outputId = "comp_sz_Graph", label = "download the graph"), textOutput("comp_sz_des")),
                             tabPanel("Currency", plotOutput("Ageing"), downloadButton(outputId = "elderly", label = "download the graph"), textOutput("des4")),
                             tabPanel("Age", plotOutput("Ageing1"), downloadButton(outputId = "elderly1", label = "download the graph"), textOutput("des5")),
                             tabPanel("Sexual_Orientation", plotOutput("Ageing2"), downloadButton(outputId = "elderly2", label = "download the graph"), textOutput("des6")),
                             tabPanel("Education_Level", plotOutput("Ageing3"), downloadButton(outputId = "elderly3", label = "download the graph"), textOutput("des7")),
                             tabPanel("Gender", plotOutput("dependency"), downloadButton(outputId = "depRatio", label = "download the graph"), textOutput("des8"))
                 ), tags$hr() ),
        
        tabPanel("Models", 
                 tabsetPanel(type = "tab", 
                             tabPanel("Naive_Bayes_Model",selectInput(inputId = "Agegroup", label ="The Probability Of",choices = c("Country","Age","10--14","15-19")),
                                                  selectInput(inputId = "Agegro", label ="Given",choices = list("Country","Age","10--14")), submitButton("View", icon("search")), verbatimTextOutput("home2")
                                      ),
                             tabPanel("Linear_Regression_Model",verbatimTextOutput("home3"), plotOutput("lim"), downloadButton(outputId = "lim3", label = "download the graph")),
                             tabPanel("KNN_Model")
                 ),tags$hr() )
        
        
      )
    )
    
  )
  )
  )
