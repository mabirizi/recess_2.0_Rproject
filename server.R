library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(pyramid)
library(plotrix)
library(glmnet)
library(data.table)
library(plotly)
library(tidyr)
library(scales)
library(e1071)
library(sqldf)
library(RSQLite)
library(stringi)

options(shiny.maxRequestSize=200*1024^2)

shinyServer(
  function(input,output,session) {
    
    
    
    
    output$stack <- renderUI(
      
    
        
        fluidRow(
          
          
          uiOutput("simple")
          
        )
        
        
      
      
    )
    
    output$simple<-renderUI({
      dashboardPage(
        dashboardHeader(title =  " ",titleWidth = 0),
        dashboardSidebar(width = 200,
                         sidebarMenu(
                           
                           menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                           menuItem("Upload File", tabName = "file" ),
                           menuItem("Uploaded File", tabName = "view" ),
                           menuItem("Analysis", tabName = "general" ),
                           
                           menuItem("Models", tabName = "models" )
                         )
        ),
        dashboardBody(
          tabItems(
            
            tabItem(tabName = "dashboard",
                    fluidRow(
                      tabPanel( title="Home",style='text-align: center;', h3("AN OVERVIEW ABOUT THE STACK OVERFLOW ANALYSIS SYSTEM"),textOutput("home"),tags$img(src = "ffff.png", height = 420, width= 810,style='text-align: center;'))
                      
                    )
            ),
            
            tabItem(tabName = "file",
                    fluidRow(
                      fileInput("file","UPLOAD FILE",
                                accept=c('csv'))
                    )
            ),
            
            tabItem(tabName = "general",
                    fluidRow(
                      tabsetPanel(
                        
                        
                        
                        tabPanel(title = "Salary",status = "primary",solidHeader = T,
                                 tabsetPanel(type = "tab",
                                             tabPanel("JobSatisfaction",plotOutput("SJobSatisfaction"), textOutput("SJobSatisfaction1")),
                                             tabPanel("CompanySize",plotOutput("SCompanySize"), textOutput("SCompanySize1")),
                                             tabPanel("YearsCodingProf",plotOutput("SYearsCodingProf"), textOutput("SYearsCodingProf1")),
                                             tabPanel("Country",plotOutput("SCountry"), textOutput("SCountry1"))
                                             
                                 ),tags$hr()
                        ),
                        tabPanel(title = "Age",status = "primary",solidHeader = T,
                                 tabsetPanel(type = "tab",
                                             tabPanel("LastNewJob",plotOutput("ALastNewJob"), textOutput("ALastNewJob1")),
                                             tabPanel("Student",plotOutput("AStudent"), textOutput("AStudent1")),
                                             tabPanel("WakeTime",plotOutput("AWakeTime"), textOutput("AWakeTime1")),
                                             tabPanel("RaceEthnicity",plotOutput("ARaceEthnicity"), textOutput("ARaceEthnicity1"))
                                             
                                 ),tags$hr()
                        ),
                        
                        
                        
                        tabPanel(title = "RaceEthnicity",status = "primary",solidHeader = T,
                                 tabsetPanel(type = "tab",
                                             tabPanel("Employment",plotOutput("REmployment"), textOutput("REmployment1")),
                                             tabPanel("Student",plotOutput("RStudent"), textOutput("RStudent1")),
                                             tabPanel("HypotheticalTools2",plotOutput("RHypotheticalTools2")),
                                             tabPanel("TimeAfterBootcamp",plotOutput("RTimeAfterBootcamp"))
                                             
                                 ),tags$hr()
                        )
                        
                        
                      )
                    )
            ),
            tabItem(tabName = "view",tableOutput("upload")
            ),
            tabItem(tabName = "models",
                    fluidRow(
                      tabsetPanel(type = "tab", 
                                  tabPanel("Naive_Bayes_Model",selectInput(inputId = "age", label ="Given",choices = c("OpenSource","Student","Employment","FormalEducation","CompanySize",
                                                                                                                       "YearsCoding","YearsCodingProf","JobSatisfaction","HopeFiveYears",
                                                                                                                       "LastNewJob","TimeAfterBootcamp","AgreeDisagree3","WakeTime",
                                                                                                                       "HoursComputer","Age","Gender","RaceEthnicity","HypotheticalTools2")),
                                           selectInput(inputId = "gro", label ="The Probability Of",choices = c("OpenSource","Student","Employment","FormalEducation","CompanySize","YearsCoding",
                                                                                                                "YearsCodingProf","JobSatisfaction","HopeFiveYears","LastNewJob","TimeAfterBootcamp",
                                                                                                                "AgreeDisagree3","WakeTime","HoursComputer","Age","Gender","RaceEthnicity",
                                                                                                                "HypotheticalTools2")),
                                           submitButton("View", icon("search")), verbatimTextOutput("home2")
                                  )
                                  
                      ),tags$hr() 
                    )
            )
            
          )
          
        )
      )
    })
    
    
    dataS <- reactive({
      file1 <- input$file
      if(is.null(file1)){
        return("No file uploaded")
      }
      
      dataS <- read.csv(file1$datapath)
      
      
      return(dataS[c(1:129)])
      
    })
    
    dataX <- read.csv("survey_results_public.csv")
    
    # ADJUSTMENTS#####
    levels(dataX$FormalEducation)[1] <- "Associate"
    levels(dataX$FormalEducation)[2] <- "Bachelor"
    levels(dataX$FormalEducation)[3] <- "Illiterate"
    levels(dataX$FormalEducation)[4] <- "Masters"
    levels(dataX$FormalEducation)[5] <- "Phd"
    levels(dataX$FormalEducation)[6] <- "Primary"
    levels(dataX$FormalEducation)[7] <- "Professional"
    levels(dataX$FormalEducation)[8] <- "Secondary"
    levels(dataX$FormalEducation)[9] <- "Uni dropout"
    #levels(dataX$FormalEducation)[c(5,4,2,7,1,9,8,6,3)]
    
    
    levels(dataX$RaceEthnicity)[19] <- "mixed2"
    levels(dataX$RaceEthnicity)[2] <- "mixed9"
    levels(dataX$RaceEthnicity)[3] <- "mixed10"
    levels(dataX$RaceEthnicity)[7] <- "mixed3"
    levels(dataX$RaceEthnicity)[18] <- "mixed4"
    levels(dataX$RaceEthnicity)[4] <- "mixed5"
    levels(dataX$RaceEthnicity)[8] <- "mixed6"
    levels(dataX$RaceEthnicity)[15] <- "mixed7"
    levels(dataX$RaceEthnicity)[11] <- "mixed8"
    levels(dataX$RaceEthnicity)[6] <- "mixed1"
    
    ##############################################
    #         REORDERING FEATURES                #
    ##############################################
    dataX$LastNewJob <- ordered(dataX$LastNewJob, levels = c("More than 4 years ago","Between 2 and 4 years ago","Between 1 and 2 years ago",
                                                             "Less than a year ago" ,"I've never had a job"  ))
    
    dataX$Age <- ordered(dataX$Age, levels = c("Under 18 years old","18 - 24 years old","25 - 34 years old",
                                               "35 - 44 years old","45 - 54 years old", "55 - 64 years old"  ))
    
    dataX$CompanySize <- ordered(dataX$CompanySize,
                                 levels = c("10,000 or more employees","5,000 to 9,999 employees",
                                            "1,000 to 4,999 employees","500 to 999 employees",
                                            "100 to 499 employees","20 to 99 employees",
                                            "10 to 19 employees", "Fewer than 10 employees" ))
    
    dataX$AgreeDisagree3 <- ordered(dataX$AgreeDisagree3, levels = c("Strongly agree","Agree","Neither Agree nor Disagree", "Disagree"  ,"Strongly disagree"  ))
    
    
    dataX$HoursComputer <- ordered(dataX$HoursComputer, levels = c("Over 12 hours","9 - 12 hours","5 - 8 hours","1 - 4 hours", "Less than 1 hour"))
    
    dataX$FormalEducation <- ordered(dataX$FormalEducation, levels = c("Phd" ,"Masters" ,"Bachelor", "Associate" ,"Pofessional degree","Uni dropout","Secondary","Primary","Illiterate" ))
    
    
    ###############################################################################################################################################
    ###############################################################################################################################################
    ##                                                                                                                                           ##           
    ##                         WARNING!!!!!       VORSICHT!!!!!!           OKULABULA!!!!                                                         ##
    ##              PLEASE DON'T TAMPER WITH THE CODE ENCLOSED BY ######FILTERING FACTORS##### FOR THAT MIGHT RUINYOUR LIFE AND YOUR WIFE'S!!!!  ## 
    ##                                                                                                                                           ##    
    ###############################################################################################################################################
    ###############################################################################################################################################
    
    ####### FILTERING FACTORS
    target <- c("Male","Female")
    filt <- filter(dataX, Gender %in% target)
    
    #dataX <- filt
    targetC <- c("United States","India","United Kingdom","Germany","France","Canada","Russian Federation","Brazil","Australia","Netherlands")
    country <- filter(filt, Country %in% targetC)
    
    targetRace <- c("White or of European descent","South Asian","Hispanic or Latino/Latina","East Asian","Middle Eastern","Black or of African descent")
    race <- filter(filt, RaceEthnicity %in% targetRace)
    
    dataX <- race
    
    #        END OF FILTERING FACTORS ############
    
    
    output$home <- renderText({
      'Founded in 2008, Stack Overflow is the largest, most trusted online community for developers to learn, share their knowledge, 
      and build their careers. More than 50 million professional and aspiring programmers visit Stack Overflow each month to help solve coding problems, develop new skills, and find job opportunities.
      Each year, Stack Overflow asks the developer community about everything from their favorite technologies to their job preferences. 
      This year marks the eighth year Stack Overflow has published their Annual Developer Survey results-with the largest number of respondents yet. 
      Over 100,000 developers took the 30-minute survey in January 2018.'
      
    })
    
    output$upload <- renderTable({
      
      df <- data.frame(dataX)
      df[1:100,2:15]
      
      
    })
    
    
    
    
    ############################################################################################
    ############################################################################################
    ###############################General Analysis Age#########################################
    output$ALastNewJob <- renderPlot({
      
      
      x <- subset(dataX,!is.na(Age))
      p2<-ggplot(subset(x,!is.na(LastNewJob)), aes(Age,group = LastNewJob)) +
        geom_bar(aes(y= ..prop..,fill=factor(..x..)),width = 0.3,stat = "count") + scale_y_continuous(labels = scales::percent) +
        geom_text(aes(label = scales::percent(..prop..),y = ..prop..),stat = "count",vjust = -.5) +labs(title="A Graph Of LastNewJob Vs Age",y= "percent", fill = "Age")  + facet_wrap(~LastNewJob) + theme_bw()
      # Horizontal bar plot
      p2 +coord_flip()
      
    })
    
    output$ALastNewJob1 <- renderText(
      "Those older than 35 stay longer in their jobs,since they have more dependents,more coding experience and are hence incentivised ro stay longer at their jobs by their employers"
    )
    
    output$AStudent <- renderPlot({
      
      
      p2<-ggplot(subset(dataX,!is.na(Student)), aes(Age,group = Student)) +
        geom_bar(aes(y= ..prop..,fill=factor(..x..)),width = 0.3,stat = "count") + scale_y_continuous(labels = scales::percent) +
        geom_text(aes(label = scales::percent(..prop..),y = ..prop..),stat = "count",vjust = -.5) +labs(title="A Graph Of Student Vs Age",y= "percent", fill = "Age")  + facet_wrap(~Student) + theme_bw()
      # Horizontal bar plot
      p2 +coord_flip()
    })
    
    output$AStudent1 <- renderText(
      "Developers above 35years are least likely to be students while most of those below 25years are"
    )
    
    output$AWakeTime <- renderPlot({
      
      
      p2<-ggplot(subset(dataX,!is.na(WakeTime)), aes(Age,group = WakeTime)) +
        geom_bar(aes(y= ..prop..,fill=factor(..x..)),width = 0.3,stat = "count") + scale_y_continuous(labels = scales::percent) +
        geom_text(aes(label = scales::percent(..prop..),y = ..prop..),stat = "count",vjust = -.5) +labs(title="A Graph Of WakeTime Vs Age",y= "percent", fill = "Age")  + facet_wrap(~WakeTime) + theme_bw()
      # Horizontal bar plot
      p2 +coord_flip()
    })
    
    output$AWakeTime1 <- renderText(
      "Most developers below 24years works night shifts because they are students"
    )
    
    
    output$ARaceEthnicity <- renderPlot({
      
      
      p2<-ggplot(subset(dataX,!is.na(Age)), aes(fill = RaceEthnicity,x=Age)) +
        geom_bar(position="fill",width = 0.3) + theme_bw() +labs(title="A Graph Of RaceEthnicity Vs Age")
      # Horizontal bar plot
      p2
    })
    
    output$ARaceEthnicity1 <- renderText(
      "Younger age groups are more ethnically diverse,since more of them are getting access to easier access to technological information.This is unlike older generations when information was limited"
    )
    
    
    
    ############################################################################################
    ############################################################################################
    ################################RACE ETHINICITY############################################# 
    output$RStudent <- renderPlot({
      
      
      p2<-ggplot(subset(dataX,!is.na(Student)), aes(RaceEthnicity,group = Student)) +
        geom_bar(aes(y= ..prop..,fill=factor(..x..)),width = 0.3,stat = "count") + scale_y_continuous(labels = scales::percent) +
        geom_text(aes(label = scales::percent(..prop..),y = ..prop..),stat = "count",vjust = -.5) +labs(title="A Graph Of Student Vs RaceEthnicity",y= "percent", fill = "RaceEthnicity") + facet_wrap(~Student) + theme_bw()
      # Horizontal bar plot
      p2 # + coord_flip()
    })
    
    output$RStudent1 <- renderText(
      "Majority of South asians are students.this explains their higher level of participation in stack overflow Q&A"
    )
    
    
    
    output$REmployment <- renderPlot({
      
      
      p2<-ggplot(subset(dataX,!is.na(Employment)), aes(RaceEthnicity,group = Employment)) +
        geom_bar(aes(y= ..prop..,fill=factor(..x..)),width = 0.3,stat = "count") + scale_y_continuous(labels = scales::percent) +
        geom_text(aes(label = scales::percent(..prop..),y = ..prop..),stat = "count",vjust = -.5) +labs(title="A Graph Of Employment Vs RaceEthnicity",y= "percent", fill = "RaceEthnicity") + facet_wrap(~Employment) + theme_bw()
      # Horizontal bar plot
      p2 # + coord_flip()
    })
    
    output$REmployment1 <- renderText(
      "Whites are less likely to be unemployed or interested in any job opportunities.Most south asians are either employed full time or actively looking for work.Most blacks are unemployed or freelancing
      "
    )
    
    
    
    output$RHypotheticalTools2 <- renderPlot({
      
      
      p2<-ggplot(subset(dataX,!is.na(HypotheticalTools2)), aes(RaceEthnicity,group = HypotheticalTools2)) +
        geom_bar(aes(y= ..prop..,fill=factor(..x..)),width = 0.3,stat = "count") + scale_y_continuous(labels = scales::percent) +
        geom_text(aes(label = scales::percent(..prop..),y = ..prop..),stat = "count",vjust = -.5) +labs(title="A Graph Of HypotheticalTools2 Vs RaceEthnicity",y= "percent", fill = "RaceEthnicity") + facet_wrap(~HypotheticalTools2) + theme_bw()
      # Horizontal bar plot
      p2
    })
    
    
    
    
    output$RTimeAfterBootcamp <- renderPlot({
      
      
      p2<-ggplot(subset(dataX,!is.na(TimeAfterBootcamp)), aes(RaceEthnicity,group = TimeAfterBootcamp)) +
        geom_bar(aes(y= ..prop..,fill=factor(..x..)),width = 0.3,stat = "count") + scale_y_continuous(labels = scales::percent) +
        geom_text(aes(label = scales::percent(..prop..),y = ..prop..),stat = "count",vjust = -.5) +labs(title="A Graph Of TimeAfterBootcamp Vs RaceEthnicity",y= "percent", fill = "RaceEthnicity") + facet_wrap(~TimeAfterBootcamp) + theme_bw()
      # Horizontal bar plot
      p2  + coord_flip() +labs(title="Time taken to get a job after bootcamp",
                               subtitle="This chart shows the time taken by develoers to get a job after a training  bootcamp.
                               It shows that whites get jobs more easily than other races after the bootcamp.
                               Also, blacks take the longest time to get a job after bootcamp training ",caption="Source: stackoverflow-survey")
      
    })
    
    
    ###########################################################################################
    ###########################################################################################
    ##########################END Of GENERAL ANALYSIS##########################################
    
    
    
    
    #############################################################################################
    #############################################################################################
    ################################Naive Bayes Classification###################################
    
    
    
    output$home2 <- hello <-renderPrint({
      
      first <- input$age
      second <- input$gro
      
      
      caseTita = as.data.frame(dataS())
      
      
      
      
      
      ##################################################################################################OpenSource
      
      
      if (input$age=="OpenSource" && input$gro=="Age"){
        status= naiveBayes(OpenSource ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      else if (input$age=="OpenSource" && input$gro=="RaceEthnicity"){
        status= naiveBayes(OpenSource ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      else if (input$age=="Student" && input$gro=="Age"){
        status= naiveBayes(Student ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      else if (input$age=="Student" && input$gro=="RaceEthnicity"){
        status= naiveBayes(Student ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="Age"){
        status= naiveBayes(Employment ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      else if (input$age=="Employment" && input$gro=="RaceEthnicity"){
        status= naiveBayes(Employment ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      else if (input$age=="FormalEducation" && input$gro=="Age"){
        status= naiveBayes(FormalEducation ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      
      else if (input$age=="FormalEducation" && input$gro=="RaceEthnicity"){
        status= naiveBayes(FormalEducation ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      
      else if (input$age=="CompanySize" && input$gro=="Age"){
        status= naiveBayes(CompanySize ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      else if (input$age=="CompanySize" && input$gro=="RaceEthnicity"){
        status= naiveBayes(CompanySize ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      else if (input$age=="YearsCoding" && input$gro=="Age"){
        status= naiveBayes(YearsCoding ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      
      else if (input$age=="YearsCoding" && input$gro=="RaceEthnicity"){
        status= naiveBayes(YearsCoding ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      
      else if (input$age=="YearsCodingProf" && input$gro=="Age"){
        status= naiveBayes(YearsCodingProf ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      else if (input$age=="YearsCodingProf" && input$gro=="RaceEthnicity"){
        status= naiveBayes(YearsCodingProf ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      
      else if (input$age=="JobSatisfaction" && input$gro=="Age"){
        status= naiveBayes(JobSatisfaction ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      
      else if (input$age=="JobSatisfaction" && input$gro=="RaceEthnicity"){
        status= naiveBayes(JobSatisfaction ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      else if (input$age=="HopeFiveYears" && input$gro=="Age"){
        status= naiveBayes(HopeFiveYears ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      
      
      else if (input$age=="HopeFiveYears" && input$gro=="RaceEthnicity"){
        status= naiveBayes(HopeFiveYears ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
    
      
      
      else if (input$age=="LastNewJob" && input$gro=="Age"){
        status= naiveBayes(LastNewJob ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      else if (input$age=="LastNewJob" && input$gro=="RaceEthnicity"){
        status= naiveBayes(LastNewJob ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      else if (input$age=="TimeAfterBootcamp" && input$gro=="Age"){
        status= naiveBayes(TimeAfterBootcamp ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      
      else if (input$age=="TimeAfterBootcamp" && input$gro=="RaceEthnicity"){
        status= naiveBayes(TimeAfterBootcamp ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      
      else if (input$age=="AgreeDisagree3" && input$gro=="Age"){
        status= naiveBayes(AgreeDisagree3 ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      
      
      else if (input$age=="AgreeDisagree3" && input$gro=="RaceEthnicity"){
        status= naiveBayes(AgreeDisagree3 ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      
      
      else if (input$age=="WakeTime" && input$gro=="Age"){
        status= naiveBayes(WakeTime ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      else if (input$age=="WakeTime" && input$gro=="RaceEthnicity"){
        status= naiveBayes(WakeTime ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      else if (input$age=="HoursComputer" && input$gro=="Age"){
        status= naiveBayes(HoursComputer ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      
      else if (input$age=="HoursComputer" && input$gro=="RaceEthnicity"){
        status= naiveBayes(HoursComputer ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      
      
      
      
      
      
      ############################################################################################### Age
      
      else if (input$age=="Age" && input$gro=="OpenSource"){
        status= naiveBayes(Age ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="Age" && input$gro=="Country"){
        status= naiveBayes(Age ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
      
      
      else if (input$age=="Age" && input$gro=="Student"){
        status= naiveBayes(Age ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      
      
      else if (input$age=="Age" && input$gro=="FormalEducation"){
        status= naiveBayes(Age ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
      
      
      else if (input$age=="Age" && input$gro=="CompanySize"){
        status= naiveBayes(Age ~ CompanySize, data = caseTita)
        status
        
      }
      
      else if (input$age=="Age" && input$gro=="YearsCoding"){
        status= naiveBayes(Age ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      else if (input$age=="Age" && input$gro=="YearsCodingProf"){
        status= naiveBayes(Age ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      
      
      
      
      else if (input$age=="Age" && input$gro=="HopeFiveYears"){
        status= naiveBayes(Age ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
      
      
      else if (input$age=="Age" && input$gro=="LastNewJob"){
        status= naiveBayes(Age ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
      }
      
      
      
      else if (input$age=="Age" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(Age ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
      }
      
      
      
      else if (input$age=="Age" && input$gro=="AgreeDisagree3"){
        status= naiveBayes(Age ~ AgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="Age" && input$gro=="OperatingSystem"){
        status= naiveBayes(Age ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
        
      }
      
      else if (input$age=="Age" && input$gro=="WakeTime"){
        status= naiveBayes(Age ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      
      else if (input$age=="Age" && input$gro=="Salary"){
        status= naiveBayes(Age ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }
      
      
      else if (input$age=="Age" && input$gro=="Gender"){
        status= naiveBayes(Age ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }
      
      
      
      
      
      
      else if (input$age=="Age" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(Age ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }
      
      else if (input$age=="Age" && input$gro=="RaceEthnicity"){
        status= naiveBayes(Age ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      
      
      else if (input$age=="Gender" && input$gro=="RaceEthnicity"){
        status= naiveBayes(Gender ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      else if (input$age=="Gender" && input$gro=="Age"){
        status= naiveBayes(Gender ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      
      
      else if (input$age=="HypotheticalTools2" && input$gro=="Age"){
        status= naiveBayes(HypotheticalTools2 ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      else if (input$age=="HypotheticalTools2" && input$gro=="RaceEthnicity"){
        status= naiveBayes(HypotheticalTools2 ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      
      
      ########################################################################################RaceEthnicity
      
      else if (input$age=="RaceEthnicity" && input$gro=="OpenSource"){
        status= naiveBayes(RaceEthnicity ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="RaceEthnicity" && input$gro=="Country"){
        status= naiveBayes(RaceEthnicity ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
      
      
      else if (input$age=="RaceEthnicity" && input$gro=="Student"){
        status= naiveBayes(RaceEthnicity ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      
      
      else if (input$age=="RaceEthnicity" && input$gro=="FormalEducation"){
        status= naiveBayes(RaceEthnicity ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
      
      
      else if (input$age=="RaceEthnicity" && input$gro=="CompanySize"){
        status= naiveBayes(RaceEthnicity ~ CompanySize, data = caseTita)
        status
        
      }
      
      else if (input$age=="RaceEthnicity" && input$gro=="YearsCoding"){
        status= naiveBayes(RaceEthnicity ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      else if (input$age=="RaceEthnicity" && input$gro=="YearsCodingProf"){
        status= naiveBayes(RaceEthnicity ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      
      
      
      
      else if (input$age=="RaceEthnicity" && input$gro=="HopeFiveYears"){
        status= naiveBayes(RaceEthnicity ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
      
      
      else if (input$age=="RaceEthnicity" && input$gro=="LastNewJob"){
        status= naiveBayes(RaceEthnicity ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
      }
      
      
      
      else if (input$age=="RaceEthnicity" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(RaceEthnicity ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
      }
      
      
      
      else if (input$age=="RaceEthnicity" && input$gro=="AgreeDisagree3"){
        status= naiveBayes(RaceEthnicity ~ AgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="RaceEthnicity" && input$gro=="OperatingSystem"){
        status= naiveBayes(RaceEthnicity ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
        
      }
      
      else if (input$age=="RaceEthnicity" && input$gro=="WakeTime"){
        status= naiveBayes(RaceEthnicity ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      else if (input$age=="RaceEthnicity" && input$gro=="Salary"){
        status= naiveBayes(RaceEthnicity ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }
      
      
      else if (input$age=="RaceEthnicity" && input$gro=="Gender"){
        status= naiveBayes(RaceEthnicity ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }
      
      
      else if (input$age=="RaceEthnicity" && input$gro=="Age"){
        status= naiveBayes(RaceEthnicity ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }
      
      
      else if (input$age=="RaceEthnicity" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(RaceEthnicity ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }
      
      
      
    })
    
    
  })
