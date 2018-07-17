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

    db <- dbConnect(SQLite(), dbname="stackoverflow.db")
  
  
    USER <- reactiveValues(Logged = FALSE)
  
    observeEvent(input$login, {
     if(input$username!="" && input$password!=""){
        c<-sqldf("SELECT * FROM data ", dbname = "stackoverflow.db")  
        nrow(c)
        sta<- 0;

        for (row in 1:nrow(c)) {
          username <- c[row, "name"]
          passwo  <- c[row, "password"]
          
        
         
          
          if(input$username == username && input$password ==passwo ) {
            USER$Logged <- TRUE
           
          }
          else{
          output$message = renderText("Invalid user name or password,please try again!")
          
          show("message")
          }
        
        }
       
     }
      else{
        output$message = renderText("Enter user name or password,please!!!")
        
        show("message")
        
      }
    
    })

    observeEvent(input$logout,{
      USER$Logged<-FALSE
    })
    

    output$stack <- renderUI(
	  if (!isTRUE(USER$Logged)) {
		fluidRow(column(width=4, offset = 4,
						wellPanel(id = 'panel_login',
								  textInput('username', 'Username:'),
								  passwordInput('password', 'Password:'),
								  div(actionButton('login', 'Log in'),actionButton('exit', 'Exit'), style='text-align: center;')
						),
						textOutput("message")
		))
	  } 
	  else {
	
		 fluidRow(
			
		  actionButton("logout","LOGOUT"),
		  uiOutput("simple")
		 
		 )
		
			
	  } 
	  
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

                              tabPanel(title = "Student",status = "primary",solidHeader = T,
                                tabsetPanel(type = "tab",
                                  tabPanel("Employment",plotOutput("Employment")),
                                  tabPanel("FormalEducation",plotOutput("Formal1")),
                                  tabPanel("HoursComputer",plotOutput("HoursComputer"))
                                  ),tags$hr()
                              ),

                              
                              tabPanel(title = "Gender",status = "primary",solidHeader = T,
                                tabsetPanel(type = "tab",
                                 
                                  tabPanel("AgreeDisagree3",plotOutput("GAgreeDisagree3"), textOutput("GAgreeDisagree31")),
                                  tabPanel("FormalEducation",plotOutput("GFormalEducation"), textOutput("GFormalEducation1")),
                                  tabPanel("OpenSource",plotOutput("GOpenSource"), textOutput("GOpenSource1")),
                                  tabPanel("Country",plotOutput("GCountry"), textOutput("GCountry1")),
                                  tabPanel("HopeFiveYears",plotOutput("GHopeFiveYears"), textOutput("GHopeFiveYears1"))


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
                                tabPanel("Naive_Bayes_Model",selectInput(inputId = "age", label ="Given",choices = c("OpenSource","Student","Employment","FormalEducation","CompanySize","YearsCoding","YearsCodingProf","JobSatisfaction","HopeFiveYears","LastNewJob","TimeAfterBootcamp","AgreeDisagree3","WakeTime","HoursComputer","Age","Gender","RaceEthnicity","HypotheticalTools2")),
                                         selectInput(inputId = "gro", label ="The Probability Of",choices = c("OpenSource","Student","Employment","FormalEducation","CompanySize","YearsCoding","YearsCodingProf","JobSatisfaction","HopeFiveYears","LastNewJob","TimeAfterBootcamp","AgreeDisagree3","WakeTime","HoursComputer","Age","Gender","RaceEthnicity","HypotheticalTools2")), submitButton("View", icon("search")), verbatimTextOutput("home2")
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
	  
	    ####### FILTERING FACTORS
    target <- c("Male","Female")
    filt <- filter(dataS, Gender %in% target)
    
    #dataX <- filt
    targetC <- c("United States","India","United Kingdom","Germany","France","Canada","Russian Federation","Brazil","Australia","Netherlands")
    country <- filter(filt, Country %in% targetC)
    
    targetRace <- c("White or of European descent","South Asian","Hispanic or Latino/Latina","East Asian","Middle Eastern","Black or of African descent")
    race <- filter(filt, RaceEthnicity %in% targetRace)
    
    dataS <- race
    
    #        END OF FILTERING FACTORS ############

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
    
    dataX$AgreeDisagree1 <- ordered(dataX$AgreeDisagree1, levels = c("Strongly agree","Agree","Neither Agree nor Disagree",
                                                                     "Disagree"  ,"Strongly disagree"  ))
    
    dataX$AgreeDisagree2 <- ordered(dataX$AgreeDisagree2, levels = c("Strongly agree","Agree","Neither Agree nor Disagree", "Disagree"  ,"Strongly disagree"  ))
    
    dataX$AgreeDisagree3 <- ordered(dataX$AgreeDisagree3, levels = c("Strongly agree","Agree","Neither Agree nor Disagree", "Disagree"  ,"Strongly disagree"  ))
    
    dataX$AdsAgreeDisagree1 <- ordered(dataX$AdsAgreeDisagree1, levels = c("Strongly agree","Somewhat agree",
                                                                           "Neither agree nor disagree", "Somewhat disagree" ,"Strongly disagree"  ))
    
    dataX$AdsAgreeDisagree2 <- ordered(dataX$AdsAgreeDisagree2, levels = c("Strongly agree","Somewhat agree","Neither agree nor disagree",
                                                                           "Somewhat disagree" ,"Strongly disagree"  ))
    dataX$AdsAgreeDisagree3 <- ordered(dataX$AdsAgreeDisagree3, levels = c("Strongly agree","Somewhat agree","Neither agree nor disagree", "Somewhat disagree" ,"Strongly disagree"  ))
    
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
    
    
      
    
})
