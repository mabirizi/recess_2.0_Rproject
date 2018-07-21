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
    
    
    


    

    ############################################################################################
    ############################################################################################
    #############################General Analysis Salary########################################


    output$SCountry <- renderPlot({
      
      
        x <- subset(country,!is.na(Country))
        y <- subset(country,!is.na(ConvertedSalary))
        p2 <- aggregate(as.numeric(y$ConvertedSalary), by=list(y$Country), FUN=mean)
        colnames(p2) <- c("Country","Salary") #change column names
        
        p2 <- p2[order(p2$Salary),] # sort
        p2$Country <- factor(p2$Country,levels = p2$Country) # retain the order in plot
        
        
        p3 <- ggplot(p2, aes(x=Country,y= Salary)) +
          geom_boxplot(varwidth = T,aes(fill = Country)) + theme_bw()+labs(title="A Graph Of Country Vs Salary") 
        # Horizontal bar plot
        p3 # +coord_flip()
        
    })

    output$SCountry1 <- renderText(
        "Developers in first world countries earn higher salary than those in developing countries"
      )


    output$SJobSatisfaction <- renderPlot({
      
      
        x <- subset(dataX,!is.na(JobSatisfaction))
        y <- subset(x,!is.na(ConvertedSalary))
        p2 <- aggregate(as.numeric(y$ConvertedSalary), by=list(y$JobSatisfaction), FUN=mean)
        colnames(p2) <- c("JobSatisfaction","Salary") #change column names
        
        p2 <- p2[order(p2$Salary),] # sort
        p2$JobSatisfaction <- factor(p2$JobSatisfaction,levels = p2$JobSatisfaction) # retain the order in plot
       
         p3 <- ggplot(p2, aes(x=JobSatisfaction,y= Salary)) +
          geom_boxplot(varwidth = T,aes(fill = JobSatisfaction)) + theme_bw() +labs(title="A Graph Of JobSatisfaction Vs Salary") 
        # Horizontal bar plot
        p3 # +coord_flip()
        
    })

    output$SJobSatisfaction1 <- renderText(
        "People who earn the highest salary are extremely satisfied with their jobs.Employers who wish to retain their workers should increase their salary"
      )


    output$SCompanySize <- renderPlot({
      
      
        x <- subset(dataX,!is.na(CompanySize))
        y <- subset(x,!is.na(ConvertedSalary))
        p2 <- aggregate(as.numeric(y$ConvertedSalary), by=list(y$CompanySize), FUN=mean)
        colnames(p2) <- c("CompanySize","Salary") #change column names
        
        p2 <- p2[order(p2$Salary),] # sort
        p2$CompanySize <- factor(p2$CompanySize,levels = p2$CompanySize) # retain the order in plot
        
        p3 <- ggplot(p2, aes(x=CompanySize,y= Salary)) +
          geom_boxplot(varwidth = T,aes(fill = CompanySize)) + theme_bw() +labs(title="A Graph Of CompanySize Vs Salary") 
        # Horizontal bar plot
        p3 +coord_flip()
        
    })

    output$SCompanySize1 <- renderText(
        "Bigger companies pay higher salaries to their employees because they can afford to"
      )


    output$SYearsCodingProf <- renderPlot({
      
      
       x <- subset(dataX,!is.na(YearsCodingProf))
        y <- subset(x,!is.na(ConvertedSalary))
        p2 <- aggregate(as.numeric(y$ConvertedSalary), by=list(y$YearsCodingProf), FUN=mean)
        colnames(p2) <- c("YearsCodingProf","Salary") #change column names
        
        p2 <- p2[order(p2$Salary),] # sort
        p2$YearsCodingProf <- factor(p2$YearsCodingProf,levels = p2$YearsCodingProf) # retain the order in plot
        
        p3 <- ggplot(p2, aes(x=YearsCodingProf,y= Salary)) +
          geom_boxplot(varwidth = T,aes(fill = YearsCodingProf)) + theme_bw() +labs(title="A Graph Of YearsCodingProf Vs Salary") 
        # Horizontal bar plot
        p3 #+coord_flip()
        
    })

    output$SYearsCodingProf1 <- renderText(
        "Higher coding experience leads to higher salary"
      )


    
    
    ############################################################################################
    ############################################################################################
    ###################################General Analysis Student################################# 
    
    output$Formal1 <- renderPlot({
      
      
        x <- subset(dataX,!is.na(FormalEducation))
        y <- subset(x,!is.na(Student))
        
         ggplot(y, aes(fill =Student ,x=FormalEducation)) +
          geom_bar(position="fill",width = 0.3) + theme_bw() +labs(title="Abar graph showing the number of students using a particular type FormalEducation")
        
    })

    output$FormalEducation3 <- renderText(
        "This graph shows the total population growth from 1995 to 2016"
      )



    output$Employment <- renderPlot({
      
      
        x <- subset(dataX,!is.na(Employment))
        y <- subset(x,!is.na(Student))
        
        p2<-ggplot(y, aes(fill =Student ,x=Employment)) +
          geom_bar(position="fill",width = 0.3) + theme_bw() 
        # Horizontal bar plot
        p2+ labs(title="Abar graph showing the number of students using a particular form of Employment")
      
    })

    


    

   

    output$HoursComputer <- renderPlot({
      
      
        x <- subset(dataX,!is.na(HoursComputer))
        y <- subset(x,!is.na(Student))
        
        p2<-ggplot(y, aes(fill =Student ,x=HoursComputer)) +
          geom_bar(position="fill",width = 0.3) + theme_bw() 
        # Horizontal bar plot
        p2  + labs(title="Abar graph showing the students and numbe rof hours spent on a computer")
      
    })

   

    output$WakeTime <- renderPlot({
      
      
        x <- subset(dataX,!is.na(WakeTime))
        y <- subset(x,!is.na(Student))
        
        p2<-ggplot(y, aes(fill =Student ,x=WakeTime)) +
          geom_bar(position="fill",width = 0.3) + theme_bw() 
        # Horizontal bar plot
        p2  + coord_flip()+ labs(title="Abar graph showing the students and their wake up time")
      
    })

    

   

   


    #############################################################################################
    #############################################################################################
    ################################Naive Bayes Classification###################################
    
     
    
    output$home2 <- hello <-renderPrint({
      
      first <- input$age
      second <- input$gro
     
      
      caseTita = as.data.frame(dataS())
      
    
      
    
  
    ##################################################################################################OpenSource
      
      
      
      if (input$age=="OpenSource" && input$gro=="Country"){
        status= naiveBayes(OpenSource ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
      
      
      
      else if (input$age=="OpenSource" && input$gro=="Student"){
        status= naiveBayes(OpenSource ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      else if (input$age=="OpenSource" && input$gro=="Employment"){
        status= naiveBayes(OpenSource ~ Employment, data = caseTita)
        status
        
      }
      
      else if (input$age=="OpenSource" && input$gro=="FormalEducation"){
        status= naiveBayes(OpenSource ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
      
      
      else if (input$age=="OpenSource" && input$gro=="CompanySize"){
        status= naiveBayes(OpenSource ~ CompanySize, data = caseTita)
        status
        
      }
      
      else if (input$age=="OpenSource" && input$gro=="YearsCoding"){
        status= naiveBayes(OpenSource ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      else if (input$age=="OpenSource" && input$gro=="YearsCodingProf"){
        status= naiveBayes(OpenSource ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      else if (input$age=="OpenSource" && input$gro=="JobSatisfaction"){
        status= naiveBayes(OpenSource ~ JobSatisfaction, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$JobSatisfaction)
        twohhh
        
      }
      
      
      
      else if (input$age=="OpenSource" && input$gro=="HopeFiveYears"){
        status= naiveBayes(OpenSource ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
     
      
      else if (input$age=="OpenSource" && input$gro=="LastNewJob"){
        status= naiveBayes(OpenSource ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
        
      }
      
     
      
      else if (input$age=="OpenSource" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(OpenSource ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
      }
      
      
      
      else if (input$age=="OpenSource" && input$gro=="AgreeDisagree3"){
        status= naiveBayes(OpenSource ~ AgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="OpenSource" && input$gro=="OperatingSystem"){
        status= naiveBayes(OpenSource ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
        
      }
      
      
      
      else if (input$age=="OpenSource" && input$gro=="WakeTime"){
        status= naiveBayes(OpenSource ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      else if (input$age=="OpenSource" && input$gro=="HoursComputer"){
        status= naiveBayes(OpenSource ~ HoursComputer, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HoursComputer)
        twohhh
        
      }
      
      
      
      else if (input$age=="OpenSource" && input$gro=="Salary"){
        status= naiveBayes(OpenSource ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }


      else if (input$age=="OpenSource" && input$gro=="Gender"){
        status= naiveBayes(OpenSource ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }


      else if (input$age=="OpenSource" && input$gro=="Age"){
        status= naiveBayes(OpenSource ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }


      else if (input$age=="OpenSource" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(OpenSource ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }


       else if (input$age=="OpenSource" && input$gro=="RaceEthnicity"){
        status= naiveBayes(OpenSource ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
     
      
      
      
      
      
      
      ###################################################################################################Student	
      
      
      else if (input$age=="Student" && input$gro=="OpenSource"){
        status= naiveBayes(Student ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="Student" && input$gro=="Country"){
        status= naiveBayes(Student ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
      
      
      
      
      else if (input$age=="Student" && input$gro=="Employment"){
        status= naiveBayes(Student ~ Employment, data = caseTita)
        status
        
      }
      
      else if (input$age=="Student" && input$gro=="FormalEducation"){
        status= naiveBayes(Student ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
      
      
      else if (input$age=="Student" && input$gro=="CompanySize"){
        status= naiveBayes(Student ~ CompanySize, data = caseTita)
        status
        
      }
      
      else if (input$age=="Student" && input$gro=="YearsCoding"){
        status= naiveBayes(Student ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      else if (input$age=="Student" && input$gro=="YearsCodingProf"){
        status= naiveBayes(Student ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      else if (input$age=="Student" && input$gro=="JobSatisfaction"){
        status= naiveBayes(Student ~ JobSatisfaction, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$JobSatisfaction)
        twohhh
        
      }
      
      
      
      else if (input$age=="Student" && input$gro=="HopeFiveYears"){
        status= naiveBayes(Student ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
      
      
      else if (input$age=="Student" && input$gro=="LastNewJob"){
        status= naiveBayes(Student ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
      }
      
      
      
      else if (input$age=="Student" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(Student ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
      }
      
      
      
      else if (input$age=="Student" && input$gro=="AgreeDisagree3"){
        status= naiveBayes(Student ~ AgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="Student" && input$gro=="OperatingSystem"){
        status= naiveBayes(Student ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
      }
      
      
      else if (input$age=="Student" && input$gro=="WakeTime"){
        status= naiveBayes(Student ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      else if (input$age=="Student" && input$gro=="HoursComputer"){
        status= naiveBayes(Student ~ HoursComputer, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HoursComputer)
        twohhh
      }
      

      else if (input$age=="Student" && input$gro=="Salary"){
        status= naiveBayes(Student ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }


      else if (input$age=="Student" && input$gro=="Gender"){
        status= naiveBayes(Student ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }


      else if (input$age=="Student" && input$gro=="Age"){
        status= naiveBayes(Student ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }


      else if (input$age=="Student" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(Student ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }


       else if (input$age=="Student" && input$gro=="RaceEthnicity"){
        status= naiveBayes(Student ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
     
     
      
      
      
     
      
      
      ###################################################################################################Employment
      
      
      
      else if (input$age=="Employment" && input$gro=="OpenSource"){
        status= naiveBayes(Employment ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="Country"){
        status= naiveBayes(Employment ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
      
      
      else if (input$age=="Employment" && input$gro=="Student"){
        status= naiveBayes(Employment ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      
      
      else if (input$age=="Employment" && input$gro=="FormalEducation"){
        status= naiveBayes(Employment ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="UndergradMajor"){
        status= naiveBayes(Employment ~ UndergradMajor, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$UndergradMajor)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="CompanySize"){
        status= naiveBayes(Employment ~ CompanySize, data = caseTita)
        status
        
      }
      
      else if (input$age=="Employment" && input$gro=="YearsCoding"){
        status= naiveBayes(Employment ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="YearsCodingProf"){
        status= naiveBayes(Employment ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="JobSatisfaction"){
        status= naiveBayes(Employment ~ JobSatisfaction, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$JobSatisfaction)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="CareerSatisfaction"){
        status= naiveBayes(Employment ~ CareerSatisfaction, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$CareerSatisfaction)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="HopeFiveYears"){
        status= naiveBayes(Employment ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="JobSearchStatus"){
        status= naiveBayes(Employment ~ JobSearchStatus, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$JobSearchStatus)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="LastNewJob"){
        status= naiveBayes(Employment ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="Currency"){
        status= naiveBayes(Employment ~ Currency, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Currency)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="SalaryType"){
        status= naiveBayes(Employment ~ SalaryType, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$SalaryType)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="TimeFullyProductive"){
        status= naiveBayes(Employment ~ TimeFullyProductive, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeFullyProductive)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(Employment ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="AgreeDisagree1"){
        status= naiveBayes(Employment ~ AgreeDisagree1, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree1)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="AgreeDisagree2"){
        status= naiveBayes(Employment ~ AgreeDisagree2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree2)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="AgreeDisagree3"){
        status= naiveBayes(Employment ~ AgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="OperatingSystem"){
        status= naiveBayes(Employment ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
        
      }
      
      else if (input$age=="Employment" && input$gro=="NumberMonitors"){
        status= naiveBayes(Employment ~ NumberMonitors, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$NumberMonitors)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="CheckInCode"){
        status= naiveBayes(Employment ~ CheckInCode, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$CheckInCode)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="AdBlocker"){
        status= naiveBayes(Employment ~ AdBlocker, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AdBlocker)
        twohhh
        
        
      }
      
      else if (input$age=="Employment" && input$gro=="AdBlockerDisable"){
        status= naiveBayes(Employment ~ AdBlockerDisable, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AdBlockerDisable)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="AdsAgreeDisagree1"){
        status= naiveBayes(Employment ~ AdsAgreeDisagree1, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AdsAgreeDisagree1)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="AdsAgreeDisagree2"){
        status= naiveBayes(Employment ~ AdsAgreeDisagree2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AdsAgreeDisagree2)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="AdsAgreeDisagree3"){
        status= naiveBayes(Employment ~ AdsAgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AdsAgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="WakeTime"){
        status= naiveBayes(Employment ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      else if (input$age=="Employment" && input$gro=="HoursComputer"){
        status= naiveBayes(Employment ~ HoursComputer, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HoursComputer)
        twohhh
        
      }
      
      

      else if (input$age=="Employment" && input$gro=="Salary"){
        status= naiveBayes(Employment ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }


      else if (input$age=="Employment" && input$gro=="Gender"){
        status= naiveBayes(Employment ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }


      else if (input$age=="Employment" && input$gro=="Age"){
        status= naiveBayes(Employment ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }


      else if (input$age=="Employment" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(Employment ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }


       else if (input$age=="Employment" && input$gro=="RaceEthnicity"){
        status= naiveBayes(Employment ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      
      
      
      
      ###################################################################################################FormalEducation
      
      
      
      else if (input$age=="FormalEducation" && input$gro=="OpenSource"){
        status= naiveBayes(FormalEducation ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="FormalEducation" && input$gro=="Country"){
        status= naiveBayes(FormalEducation ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
      
      
      else if (input$age=="FormalEducation" && input$gro=="Student"){
        status= naiveBayes(FormalEducation ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      
      
      else if (input$age=="FormalEducation" && input$gro=="CompanySize"){
        status= naiveBayes(FormalEducation ~ CompanySize, data = caseTita)
        status
        
      }
      
      else if (input$age=="FormalEducation" && input$gro=="YearsCoding"){
        status= naiveBayes(FormalEducation ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      else if (input$age=="FormalEducation" && input$gro=="YearsCodingProf"){
        status= naiveBayes(FormalEducation ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      else if (input$age=="FormalEducation" && input$gro=="JobSatisfaction"){
        status= naiveBayes(FormalEducation ~ JobSatisfaction, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$JobSatisfaction)
        twohhh
        
      }
      
     
      
      else if (input$age=="FormalEducation" && input$gro=="HopeFiveYears"){
        status= naiveBayes(FormalEducation ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
      
      else if (input$age=="FormalEducation" && input$gro=="LastNewJob"){
        status= naiveBayes(FormalEducation ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
        
      }
      
      
      
      else if (input$age=="FormalEducation" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(FormalEducation ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
        
      }
      
      
      
      else if (input$age=="FormalEducation" && input$gro=="AgreeDisagree3"){
        status= naiveBayes(FormalEducation ~ AgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="FormalEducation" && input$gro=="OperatingSystem"){
        status= naiveBayes(FormalEducation ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
        
      }
      
      
      
      else if (input$age=="FormalEducation" && input$gro=="WakeTime"){
        status= naiveBayes(FormalEducation ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      else if (input$age=="FormalEducation" && input$gro=="HoursComputer"){
        status= naiveBayes(FormalEducation ~ HoursComputer, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HoursComputer)
        twohhh
        
      }


      else if (input$age=="FormalEducation" && input$gro=="Salary"){
        status= naiveBayes(FormalEducation ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }


      else if (input$age=="FormalEducation" && input$gro=="Gender"){
        status= naiveBayes(FormalEducation ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }


      else if (input$age=="FormalEducation" && input$gro=="Age"){
        status= naiveBayes(FormalEducation ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }


      else if (input$age=="FormalEducation" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(FormalEducation ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }


       else if (input$age=="FormalEducation" && input$gro=="RaceEthnicity"){
        status= naiveBayes(FormalEducation ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
    
      ###################################################################################################CompanySize
      
     
      
      else if (input$age=="CompanySize" && input$gro=="OpenSource"){
        status= naiveBayes(CompanySize ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="CompanySize" && input$gro=="Country"){
        status= naiveBayes(CompanySize ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
      
      
      else if (input$age=="CompanySize" && input$gro=="Student"){
        status= naiveBayes(CompanySize ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      
      
      else if (input$age=="CompanySize" && input$gro=="FormalEducation"){
        status= naiveBayes(CompanySize ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
      
      
      
      
      else if (input$age=="CompanySize" && input$gro=="YearsCoding"){
        status= naiveBayes(CompanySize ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      else if (input$age=="CompanySize" && input$gro=="YearsCodingProf"){
        status= naiveBayes(CompanySize ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      else if (input$age=="CompanySize" && input$gro=="JobSatisfaction"){
        status= naiveBayes(CompanySize ~ JobSatisfaction, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$JobSatisfaction)
        twohhh
        
      }
      
     
      
      else if (input$age=="CompanySize" && input$gro=="HopeFiveYears"){
        status= naiveBayes(CompanySize ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
      
      
      else if (input$age=="CompanySize" && input$gro=="LastNewJob"){
        status= naiveBayes(CompanySize ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
        
      }
      
      
      else if (input$age=="CompanySize" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(CompanySize ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
        
      }
      
      
      
      else if (input$age=="CompanySize" && input$gro=="AgreeDisagree3"){
        status= naiveBayes(CompanySize ~ AgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="CompanySize" && input$gro=="OperatingSystem"){
        status= naiveBayes(CompanySize ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
      }
      
      
      
      else if (input$age=="CompanySize" && input$gro=="WakeTime"){
        status= naiveBayes(CompanySize ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      else if (input$age=="CompanySize" && input$gro=="HoursComputer"){
        status= naiveBayes(CompanySize ~ HoursComputer, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HoursComputer)
        twohhh
        
      }
      

      else if (input$age=="CompanySize" && input$gro=="Salary"){
        status= naiveBayes(CompanySize ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }


      else if (input$age=="CompanySize" && input$gro=="Gender"){
        status= naiveBayes(CompanySize ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }


      else if (input$age=="CompanySize" && input$gro=="Age"){
        status= naiveBayes(CompanySize ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }


      else if (input$age=="CompanySize" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(CompanySize ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }


       else if (input$age=="CompanySize" && input$gro=="RaceEthnicity"){
        status= naiveBayes(CompanySize ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      ###################################################################################################YearsCoding
      
     
      
      else if (input$age=="YearsCoding" && input$gro=="OpenSource"){
        status= naiveBayes(YearsCoding ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="YearsCoding" && input$gro=="Country"){
        status= naiveBayes(YearsCoding ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
     
      
      else if (input$age=="YearsCoding" && input$gro=="Student"){
        status= naiveBayes(YearsCoding ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      
      
      else if (input$age=="YearsCoding" && input$gro=="FormalEducation"){
        status= naiveBayes(YearsCoding ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
      
      
      else if (input$age=="YearsCoding" && input$gro=="CompanySize"){
        status= naiveBayes(YearsCoding ~ CompanySize, data = caseTita)
        status
        
      }
      
      
      
      else if (input$age=="YearsCoding" && input$gro=="YearsCodingProf"){
        status= naiveBayes(YearsCoding ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      else if (input$age=="YearsCoding" && input$gro=="JobSatisfaction"){
        status= naiveBayes(YearsCoding ~ JobSatisfaction, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$JobSatisfaction)
        twohhh
        
      }
      
     
      
      else if (input$age=="YearsCoding" && input$gro=="HopeFiveYears"){
        status= naiveBayes(YearsCoding ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
      
      
      else if (input$age=="YearsCoding" && input$gro=="LastNewJob"){
        status= naiveBayes(YearsCoding ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
        
      }
      
      
      
      else if (input$age=="YearsCoding" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(YearsCoding ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
        
      }
      
     
      else if (input$age=="YearsCoding" && input$gro=="OperatingSystem"){
        status= naiveBayes(YearsCoding ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
      }
      
      
      
      else if (input$age=="YearsCoding" && input$gro=="WakeTime"){
        status= naiveBayes(YearsCoding ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      else if (input$age=="YearsCoding" && input$gro=="HoursComputer"){
        status= naiveBayes(YearsCoding ~ HoursComputer, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HoursComputer)
        twohhh
        
      }
      

      else if (input$age=="YearsCoding" && input$gro=="Salary"){
        status= naiveBayes(YearsCoding ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }


      else if (input$age=="YearsCoding" && input$gro=="Gender"){
        status= naiveBayes(YearsCoding ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }


      else if (input$age=="YearsCoding" && input$gro=="Age"){
        status= naiveBayes(YearsCoding ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }


      else if (input$age=="YearsCoding" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(YearsCoding ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }


       else if (input$age=="YearsCoding" && input$gro=="RaceEthnicity"){
        status= naiveBayes(YearsCoding ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
     
      
      
      ###################################################################################################YearsCodingProf
      
     
      
      else if (input$age=="YearsCodingProf" && input$gro=="OpenSource"){
        status= naiveBayes(YearsCodingProf ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="YearsCodingProf" && input$gro=="Country"){
        status= naiveBayes(YearsCodingProf ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
     
      
      else if (input$age=="YearsCodingProf" && input$gro=="Student"){
        status= naiveBayes(YearsCodingProf ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      
      
      else if (input$age=="YearsCodingProf" && input$gro=="FormalEducation"){
        status= naiveBayes(YearsCodingProf ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
      
      
      else if (input$age=="YearsCodingProf" && input$gro=="CompanySize"){
        status= naiveBayes(YearsCodingProf ~ CompanySize, data = caseTita)
        status
        
      }
      
      else if (input$age=="YearsCodingProf" && input$gro=="YearsCoding"){
        status= naiveBayes(YearsCodingProf ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      
      
      else if (input$age=="YearsCodingProf" && input$gro=="JobSatisfaction"){
        status= naiveBayes(YearsCodingProf ~ JobSatisfaction, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$JobSatisfaction)
        twohhh
        
      }
      
     
      
      else if (input$age=="YearsCodingProf" && input$gro=="HopeFiveYears"){
        status= naiveBayes(YearsCodingProf ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
     
      
      else if (input$age=="YearsCodingProf" && input$gro=="LastNewJob"){
        status= naiveBayes(YearsCodingProf ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
        
      }
      
      
      
      else if (input$age=="YearsCodingProf" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(YearsCodingProf ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
        
      }
      
      
      
      else if (input$age=="YearsCodingProf" && input$gro=="AgreeDisagree3"){
        status= naiveBayes(YearsCodingProf ~ AgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="YearsCodingProf" && input$gro=="OperatingSystem"){
        status= naiveBayes(YearsCodingProf ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
        
      }
      
      
      
      else if (input$age=="YearsCodingProf" && input$gro=="WakeTime"){
        status= naiveBayes(YearsCodingProf ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
      }
      
      else if (input$age=="YearsCodingProf" && input$gro=="HoursComputer"){
        status= naiveBayes(YearsCodingProf ~ HoursComputer, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HoursComputer)
        twohhh
        
      }
      
     
     else if (input$age=="YearsCodingProf" && input$gro=="Salary"){
        status= naiveBayes(YearsCodingProf ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }


      else if (input$age=="YearsCodingProf" && input$gro=="Gender"){
        status= naiveBayes(YearsCodingProf ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }


      else if (input$age=="YearsCodingProf" && input$gro=="Age"){
        status= naiveBayes(YearsCodingProf ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }


      else if (input$age=="YearsCodingProf" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(YearsCodingProf ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }


       else if (input$age=="YearsCodingProf" && input$gro=="RaceEthnicity"){
        status= naiveBayes(YearsCodingProf ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
      
      
      
      
      ###################################################################################################JobSatisfaction
      
      
      
      else if (input$age=="JobSatisfaction" && input$gro=="OpenSource"){
        status= naiveBayes(JobSatisfaction ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="JobSatisfaction" && input$gro=="Country"){
        status= naiveBayes(JobSatisfaction ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
     
      
      else if (input$age=="JobSatisfaction" && input$gro=="Student"){
        status= naiveBayes(JobSatisfaction ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      
      
      else if (input$age=="JobSatisfaction" && input$gro=="FormalEducation"){
        status= naiveBayes(JobSatisfaction ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
      
      
      else if (input$age=="JobSatisfaction" && input$gro=="CompanySize"){
        status= naiveBayes(JobSatisfaction ~ CompanySize, data = caseTita)
        status
        
      }
      
      else if (input$age=="JobSatisfaction" && input$gro=="YearsCoding"){
        status= naiveBayes(JobSatisfaction ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      else if (input$age=="JobSatisfaction" && input$gro=="YearsCodingProf"){
        status= naiveBayes(JobSatisfaction ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      
      
     
      
      else if (input$age=="JobSatisfaction" && input$gro=="HopeFiveYears"){
        status= naiveBayes(JobSatisfaction ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
      
      
      else if (input$age=="JobSatisfaction" && input$gro=="LastNewJob"){
        status= naiveBayes(JobSatisfaction ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
        
      }
      
     
      
      else if (input$age=="JobSatisfaction" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(JobSatisfaction ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
        
      }
      
     
      
      else if (input$age=="JobSatisfaction" && input$gro=="AgreeDisagree3"){
        status= naiveBayes(JobSatisfaction ~ AgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="JobSatisfaction" && input$gro=="OperatingSystem"){
        status= naiveBayes(JobSatisfaction ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
        
      }
      
      
      
      else if (input$age=="JobSatisfaction" && input$gro=="WakeTime"){
        status= naiveBayes(JobSatisfaction ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      else if (input$age=="JobSatisfaction" && input$gro=="HoursComputer"){
        status= naiveBayes(JobSatisfaction ~ HoursComputer, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HoursComputer)
        twohhh
        
      }
      
     

     else if (input$age=="JobSatisfaction" && input$gro=="Salary"){
        status= naiveBayes(JobSatisfaction ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }


      else if (input$age=="JobSatisfaction" && input$gro=="Gender"){
        status= naiveBayes(JobSatisfaction ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }


      else if (input$age=="JobSatisfaction" && input$gro=="Age"){
        status= naiveBayes(JobSatisfaction ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }


      else if (input$age=="JobSatisfaction" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(JobSatisfaction ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }


       else if (input$age=="JobSatisfaction" && input$gro=="RaceEthnicity"){
        status= naiveBayes(JobSatisfaction ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
     
      
     
      ###################################################################################################HopeFiveYears
      
     
      
      else if (input$age=="HopeFiveYears" && input$gro=="OpenSource"){
        status= naiveBayes(HopeFiveYears ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="HopeFiveYears" && input$gro=="Country"){
        status= naiveBayes(HopeFiveYears ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
      
      
      else if (input$age=="HopeFiveYears" && input$gro=="Student"){
        status= naiveBayes(HopeFiveYears ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      
      
      else if (input$age=="HopeFiveYears" && input$gro=="FormalEducation"){
        status= naiveBayes(HopeFiveYears ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
    
      else if (input$age=="HopeFiveYears" && input$gro=="CompanySize"){
        status= naiveBayes(HopeFiveYears ~ CompanySize, data = caseTita)
        status
        
      }
      
      else if (input$age=="HopeFiveYears" && input$gro=="YearsCoding"){
        status= naiveBayes(HopeFiveYears ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      else if (input$age=="HopeFiveYears" && input$gro=="YearsCodingProf"){
        status= naiveBayes(HopeFiveYears ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      
      
     
      
      
    
      
      else if (input$age=="HopeFiveYears" && input$gro=="LastNewJob"){
        status= naiveBayes(HopeFiveYears ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
        
      }
      
      
      
      else if (input$age=="HopeFiveYears" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(HopeFiveYears ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
        
      }
      
     
      
      else if (input$age=="HopeFiveYears" && input$gro=="AgreeDisagree3"){
        status= naiveBayes(HopeFiveYears ~ AgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="HopeFiveYears" && input$gro=="OperatingSystem"){
        status= naiveBayes(HopeFiveYears ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
        
      }
      
      
      
      else if (input$age=="HopeFiveYears" && input$gro=="WakeTime"){
        status= naiveBayes(HopeFiveYears ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      else if (input$age=="HopeFiveYears" && input$gro=="HoursComputer"){
        status= naiveBayes(HopeFiveYears ~ HoursComputer, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HoursComputer)
        twohhh
        
      }
      

      else if (input$age=="HopeFiveYears" && input$gro=="Salary"){
        status= naiveBayes(HopeFiveYears ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }


      else if (input$age=="HopeFiveYears" && input$gro=="Gender"){
        status= naiveBayes(HopeFiveYears ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }


      else if (input$age=="HopeFiveYears" && input$gro=="Age"){
        status= naiveBayes(HopeFiveYears ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }


      else if (input$age=="HopeFiveYears" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(HopeFiveYears ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }


       else if (input$age=="HopeFiveYears" && input$gro=="RaceEthnicity"){
        status= naiveBayes(HopeFiveYears ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      ###################################################################################################LastNewJob
      
      
      
      else if (input$age=="LastNewJob" && input$gro=="OpenSource"){
        status= naiveBayes(LastNewJob ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="LastNewJob" && input$gro=="Country"){
        status= naiveBayes(LastNewJob ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
     
      
      else if (input$age=="LastNewJob" && input$gro=="Student"){
        status= naiveBayes(LastNewJob ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      
      
      else if (input$age=="LastNewJob" && input$gro=="FormalEducation"){
        status= naiveBayes(LastNewJob ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
     
      
      else if (input$age=="LastNewJob" && input$gro=="CompanySize"){
        status= naiveBayes(LastNewJob ~ CompanySize, data = caseTita)
        status
        
      }
      
      else if (input$age=="LastNewJob" && input$gro=="YearsCoding"){
        status= naiveBayes(LastNewJob ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      else if (input$age=="LastNewJob" && input$gro=="YearsCodingProf"){
        status= naiveBayes(LastNewJob ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      
      
     
      
      else if (input$age=="LastNewJob" && input$gro=="HopeFiveYears"){
        status= naiveBayes(LastNewJob ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
      
      
      else if (input$age=="LastNewJob" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(LastNewJob ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
        
      }
      
      
      
      else if (input$age=="LastNewJob" && input$gro=="AgreeDisagree3"){
        status= naiveBayes(LastNewJob ~ AgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="LastNewJob" && input$gro=="OperatingSystem"){
        status= naiveBayes(LastNewJob ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
        
      }
      
     
      
      else if (input$age=="LastNewJob" && input$gro=="WakeTime"){
        status= naiveBayes(LastNewJob ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      else if (input$age=="LastNewJob" && input$gro=="HoursComputer"){
        status= naiveBayes(LastNewJob ~ HoursComputer, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HoursComputer)
        twohhh
        
      }
      

      else if (input$age=="LastNewJob" && input$gro=="Salary"){
        status= naiveBayes(LastNewJob ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }


      else if (input$age=="LastNewJob" && input$gro=="Gender"){
        status= naiveBayes(LastNewJob ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }


      else if (input$age=="LastNewJob" && input$gro=="Age"){
        status= naiveBayes(LastNewJob ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }


      else if (input$age=="LastNewJob" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(LastNewJob ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }


       else if (input$age=="LastNewJob" && input$gro=="RaceEthnicity"){
        status= naiveBayes(LastNewJob ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
     
      
      
      
      ###################################################################################################AgreeDisagree3
      
      
      
      else if (input$age=="AgreeDisagree3" && input$gro=="OpenSource"){
        status= naiveBayes(AgreeDisagree3 ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="AgreeDisagree3" && input$gro=="Country"){
        status= naiveBayes(AgreeDisagree3 ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
      
      
      else if (input$age=="AgreeDisagree3" && input$gro=="Student"){
        status= naiveBayes(AgreeDisagree3 ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      
      
      else if (input$age=="AgreeDisagree3" && input$gro=="FormalEducation"){
        status= naiveBayes(AgreeDisagree3 ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
     
      
      else if (input$age=="AgreeDisagree3" && input$gro=="CompanySize"){
        status= naiveBayes(AgreeDisagree3 ~ CompanySize, data = caseTita)
        status
        
      }
      
      else if (input$age=="AgreeDisagree3" && input$gro=="YearsCoding"){
        status= naiveBayes(AgreeDisagree3 ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      else if (input$age=="AgreeDisagree3" && input$gro=="YearsCodingProf"){
        status= naiveBayes(AgreeDisagree3 ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      
     
      
      else if (input$age=="AgreeDisagree3" && input$gro=="HopeFiveYears"){
        status= naiveBayes(AgreeDisagree3 ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
      
      
      else if (input$age=="AgreeDisagree3" && input$gro=="LastNewJob"){
        status= naiveBayes(AgreeDisagree3 ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
        
      }
      
      
      
      else if (input$age=="AgreeDisagree3" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(AgreeDisagree3 ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
        
      }
      
      
      
      
      
      else if (input$age=="AgreeDisagree3" && input$gro=="OperatingSystem"){
        status= naiveBayes(AgreeDisagree3 ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
        
      }
      
      
      
      else if (input$age=="AgreeDisagree3" && input$gro=="WakeTime"){
        status= naiveBayes(AgreeDisagree3 ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      else if (input$age=="AgreeDisagree3" && input$gro=="HoursComputer"){
        status= naiveBayes(AgreeDisagree3 ~ HoursComputer, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HoursComputer)
        twohhh
        
      }


      else if (input$age=="AgreeDisagree3" && input$gro=="Salary"){
        status= naiveBayes(AgreeDisagree3 ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }


      else if (input$age=="AgreeDisagree3" && input$gro=="Gender"){
        status= naiveBayes(AgreeDisagree3 ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }


      else if (input$age=="AgreeDisagree3" && input$gro=="Age"){
        status= naiveBayes(AgreeDisagree3 ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }


      else if (input$age=="AgreeDisagree3" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(AgreeDisagree3 ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }


       else if (input$age=="AgreeDisagree3" && input$gro=="RaceEthnicity"){
        status= naiveBayes(AgreeDisagree3 ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }
      
      
      
     
      
      
      
      
      
    
      ###################################################################################################HoursComputer
      
      else if (input$age=="HoursComputer" && input$gro=="OpenSource"){
        status= naiveBayes(HoursComputer ~ OpenSource, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OpenSource)
        twohhh
        
      }
      
      else if (input$age=="HoursComputer" && input$gro=="Country"){
        status= naiveBayes(HoursComputer ~ Country, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Country)
        twohhh
        
      }
      
      
      
      else if (input$age=="HoursComputer" && input$gro=="Student"){
        status= naiveBayes(HoursComputer ~ Student, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Student)
        twohhh
        
      }
      
      
      
      else if (input$age=="HoursComputer" && input$gro=="FormalEducation"){
        status= naiveBayes(HoursComputer ~ FormalEducation, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$FormalEducation)
        twohhh
        
      }
      
      
      
      else if (input$age=="HoursComputer" && input$gro=="CompanySize"){
        status= naiveBayes(HoursComputer ~ CompanySize, data = caseTita)
        status
        
      }
      
      else if (input$age=="HoursComputer" && input$gro=="YearsCoding"){
        status= naiveBayes(HoursComputer ~ YearsCoding, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCoding)
        twohhh
        
      }
      
      else if (input$age=="HoursComputer" && input$gro=="YearsCodingProf"){
        status= naiveBayes(HoursComputer ~ YearsCodingProf, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$YearsCodingProf)
        twohhh
        
      }
      
      
      
     
      
      else if (input$age=="HoursComputer" && input$gro=="HopeFiveYears"){
        status= naiveBayes(HoursComputer ~ HopeFiveYears, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HopeFiveYears)
        twohhh
        
      }
      
     
      
      else if (input$age=="HoursComputer" && input$gro=="LastNewJob"){
        status= naiveBayes(HoursComputer ~ LastNewJob, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$LastNewJob)
        twohhh
      }
      
      
      
      else if (input$age=="HoursComputer" && input$gro=="TimeAfterBootcamp"){
        status= naiveBayes(HoursComputer ~ TimeAfterBootcamp, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$TimeAfterBootcamp)
        twohhh
      }
      
      
      
      else if (input$age=="HoursComputer" && input$gro=="AgreeDisagree3"){
        status= naiveBayes(HoursComputer ~ AgreeDisagree3, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$AgreeDisagree3)
        twohhh
        
      }
      
      else if (input$age=="HoursComputer" && input$gro=="OperatingSystem"){
        status= naiveBayes(HoursComputer ~ OperatingSystem, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$OperatingSystem)
        twohhh
        
        
      }
      
      else if (input$age=="HoursComputer" && input$gro=="WakeTime"){
        status= naiveBayes(HoursComputer ~ WakeTime, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$WakeTime)
        twohhh
        
      }
      
      else if (input$age=="HoursComputer" && input$gro=="Salary"){
        status= naiveBayes(HoursComputer ~ Salary, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Salary)
        twohhh
        
      }


      else if (input$age=="HoursComputer" && input$gro=="Gender"){
        status= naiveBayes(HoursComputer ~ Gender, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Gender)
        twohhh
        
      }


      else if (input$age=="HoursComputer" && input$gro=="Age"){
        status= naiveBayes(HoursComputer ~ Age, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$Age)
        twohhh
        
      }


      else if (input$age=="HoursComputer" && input$gro=="HypotheticalTools2"){
        status= naiveBayes(HoursComputer ~ HypotheticalTools2, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$HypotheticalTools2)
        twohhh
        
      }


      else if (input$age=="HoursComputer" && input$gro=="RaceEthnicity"){
        status= naiveBayes(HoursComputer ~ RaceEthnicity, data = caseTita)
        print(status)
        
        twohhh <<- table(predict(status,caseTita),caseTita$RaceEthnicity)
        twohhh
        
      }



     





		


	



      


      
		
      
    })
      
    
})
