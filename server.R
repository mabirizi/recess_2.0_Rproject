library(shiny)
library(ggplot2)
library(dplyr)
library(pyramid)
library(plotrix)
library(glmnet)
library(e1071)
library(data.table)


shinyServer(
  
  function(input, output, session){
    
    dataX <- reactive({
      file1 <- input$file
      if(is.null(file1)){
        return("No file uploaded")
      }
      
      dataX <- read.csv(file1$datapath)
      return(dataX[c(1:129)])
      
    })


    output$home <- renderText({
      'ABOUT PAS'
      'The system gives the analysis of a given Population data putting many aspects of population into 
      consideration such as population growth, dependency rate, infancy rate, etc, and helps 
      the users to take appropriate measures in their feilds of conern.
      It displays the outcome of the analysis inform of graphs such as line graph 
      and population pyramid to aid understanding by the user.'
      
    })
    

    output$upload <- renderTable({
      
      dataX() 
    })
    

    #General Analysis .....Country
    output$country <- renderPlot({
      
        c1 <- filter(dataX(), dataX()$Country=='Kenya')
        Tc1 <- count(c1)
      
        c2 <- filter(dataX(), dataX()$Country=='United Kingdom')
        Tc2 = count(c2)
   
        c3 <- filter(dataX(), dataX()$Country=='United States')
        Tc3 = count(c3)
        
        c4 = filter(dataX(), dataX()$Country=='South Africa')
        Tc4 = count(c4)
        
        c5 = filter(dataX(),dataX()$Country=='Nigeria')
        Tc5 = count(c5)
        
        c6 = filter(dataX(),dataX()$Country=='India')
        Tc6 = count(c6)
        
        c7 = filter(dataX(), dataX()$Country=='Spain')
        Tc7 = count(c7)
        
        c8 = filter(dataX(),dataX()$Country=='Croatia')
        Tc8 = count(c8)
        
        c9 = filter(dataX(),dataX()$Country=='Netherlands')
        Tc9 = count(c9)
       
        c10 = filter(dataX(),dataX()$Country=='Israel')
        Tc10 = count(c10)
        
      x = c('Kenya','United Kingdom','United States','South Africa','Nigeria','India','Spain','Croatia','Netherlands','Israel')
     
      y = c(Tc1,Tc2,Tc3,Tc4,Tc5,Tc6,Tc7,Tc8,Tc9,Tc10)
      #plot(table(x),as.numeric(y),type="l",xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
      #plot(table(x),y,type="o",xlim = NULL,ylim = NULL ,xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
      plot(as.numeric(y),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
      #lines(table(x),y,type="o",col="blue")
    
      output$cGraph <- downloadHandler(
        filename = function(){
          paste("dataX",input$fileType, sep = ".")
        },
        content = function(file){
          #if(input$fileType == "pdf")
          #  pdf(file)
        # else
            png(file)
          
          #plot(x,y,type="o",,xlim = NULL,ylim = NULL ,xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
          plot(as.numeric(y),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
          dev.off()
        },
        contentType = "pdf/png"
      ) 
      output$cdes <- renderText(
        "This graph shows the Country and the corresponding number of respondents from each"
      )
      
    }) 




#General Analysis .....Company_Size
    output$comp_sz <- renderPlot({
      
        cs1 <- filter(dataX(), dataX()$CompanySize=='20 to 99 employees')
        Tcs1 <- count(cs1)
      
        cs2 <- filter(dataX(), dataX()$CompanySize=='10,000 or more employees')
        Tcs2 = count(cs2)
   
        cs3 <- filter(dataX(), dataX()$CompanySize=='100 to 499 employees')
        Tcs3 = count(cs3)
        
        cs4 = filter(dataX(), dataX()$CompanySize=='10 to 19 employees')
        Tcs4 = count(cs4)
        
        cs5 = filter(dataX(),dataX()$CompanySize=='500 to 999 employees')
        Tcs5 = count(cs5)
        
        cs6 = filter(dataX(),dataX()$CompanySize=='1,000 to 4,999 employees')
        Tcs6 = count(cs6)
        
        cs7 = filter(dataX(), dataX()$CompanySize=='5,000 to 9,999 employees')
        Tcs7 = count(cs7)
        
        cs8 = filter(dataX(),dataX()$CompanySize=='Fewer than 10 employees')
        Tcs8 = count(cs8)
        
        #c9 = filter(dataX(),dataX()$CompanySize=='Netherlands')
        #Tc9 = count(c9)
       
        #c10 = filter(dataX(),dataX()$CompanySize=='Israel')
        #Tc10 = count(c10)
        
      x = c('20 to 99 employees','10,000 or more employees','100 to 499 employees','10 to 19 employees','500 to 999 employees','1,000 to 4,999 employees','5,000 to 9,999 employees','Fewer than 10 employees')
     
      y = c(Tcs1,Tcs2,Tcs3,Tcs4,Tcs5,Tcs6,Tcs7,Tcs8)
      #plot(table(x),as.numeric(y),type="l",xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
      #plot(table(x),y,type="o",xlim = NULL,ylim = NULL ,xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
      plot(as.numeric(y),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
      #lines(table(x),y,type="o",col="blue")
    
      output$comp_sz_Graph <- downloadHandler(
        filename = function(){
          paste("dataX",input$fileType, sep = ".")
        },
        content = function(file){
          #if(input$fileType == "pdf")
          #  pdf(file)
         #else
            png(file)
          
          #plot(x,y,type="o",,xlim = NULL,ylim = NULL ,xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
          plot(as.numeric(y),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
          dev.off()
        },
        contentType = "pdf/png"
      ) 
      output$comp_sz_des <- renderText(
        "This graph shows the Company Sizes ranging "
      )
      
    }) 


#years of coding
    output$comp_cd <- renderPlot({
      
        yc1 <- filter(dataX(), dataX()$YearsCoding=='3-5 years')
        Tyc1 <- count(yc1)
      
        yc2 <- filter(dataX(), dataX()$YearsCoding=='DevOps specialist')
        Tyc2 = count(yc2)
   
        yc3 <- filter(dataX(), dataX()$YearsCoding=='Full-stack developer')
        Tyc3 = count(yc3)
        
        yc4 = filter(dataX(), dataX()$YearsCoding=='18-20 years')
        Tyc4 = count(yc4)
        
        yc5 = filter(dataX(),dataX()$YearsCoding=='Desktop or enterprise applications developer')
        Tyc5 = count(yc5)
        
        yc6 = filter(dataX(),dataX()$YearsCoding=='Database administrator')
        Tyc6 = count(yc6)
        
        yc7 = filter(dataX(), dataX()$YearsCoding=='Front-end developer')
        Tyc7 = count(yc7)
        
        yc8 = filter(dataX(),dataX()$YearsCoding=='30 or more years')
        Tyc8 = count(yc8)
        
        yc9 = filter(dataX(),dataX()$YearsCoding=='0-2 years')
        Tyc9 = count(yc9)
       
        yc10 = filter(dataX(),dataX()$YearsCoding=='6-8 years')
        Tyc10 = count(yc10)
        
        yc11 = filter(dataX(),dataX()$YearsCoding=='Data scientist or machine learning specialist')
        Tyc11 = count(yc11)
        
        yc12 = filter(dataX(),dataX()$YearsCoding=='9-11 years')
        Tyc12 = count(yc12)
       
        yc13 = filter(dataX(),dataX()$YearsCoding=='Engineering manager')
        Tyc13 = count(yc13)
 
        yc14 = filter(dataX(),dataX()$YearsCoding=='Designer')
        Tyc14 = count(yc14)
        
        yc15 = filter(dataX(),dataX()$YearsCoding=='Data or business analyst')
        Tyc15 = count(yc15)
        
        yc16 = filter(dataX(),dataX()$YearsCoding=='Product manager')
        Tyc16 = count(yc16)
       
        yc17 = filter(dataX(),dataX()$YearsCoding=='Student')
        Tyc17 = count(yc17)

        yc18 = filter(dataX(),dataX()$YearsCoding=='24-26 years')
        Tyc18 = count(yc18)
 
        yc19 = filter(dataX(),dataX()$YearsCoding=='Embedded applications or devices developer')
        Tyc19 = count(yc19)
        
        yc20 = filter(dataX(),dataX()$YearsCoding=='12-14 years')
        Tyc20 = count(yc20)
        
        yc21 = filter(dataX(),dataX()$YearsCoding=='Mobile developer')
        Tyc21 = count(yc21)
       
        yc22 = filter(dataX(),dataX()$YearsCoding=='15-17 years')
        Tyc22 = count(yc22)

        yc23 = filter(dataX(),dataX()$YearsCoding=='QA or test developer')
        Tyc23 = count(yc23)

        yc24 = filter(dataX(),dataX()$YearsCoding=='Game or graphics developer')
        Tyc24 = count(yc24)
 
        yc25 = filter(dataX(),dataX()$YearsCoding=='Educator or academic researcher')
        Tyc25 = count(yc25)
        
        yc26 = filter(dataX(),dataX()$YearsCoding=='21-23 years')
        Tyc26 = count(yc26)
        
        yc27 = filter(dataX(),dataX()$YearsCoding=='System administrator')
        Tyc27 = count(yc27)
       
        yc28 = filter(dataX(),dataX()$YearsCoding=='27-29 years')
        Tyc28 = count(yc28)

        yc29 = filter(dataX(),dataX()$YearsCoding=='Marketing or sales professional')
        Tyc29 = count(yc29)

        yc30 = filter(dataX(),dataX()$YearsCoding=='Game or graphics developer')
        Tyc30 = count(yc30)
 
        yc31 = filter(dataX(),dataX()$YearsCoding=='NA')
        Tyc31 = count(yc31)

      x = c('3-5 years','DevOps specialist','Full-stack developer','18-20 years','Desktop or enterprise applications developer','Database administrator','Front-end developer','30 or more years', '0-2 years', '6-8 years', 'Data scientist or machine learning specialist', '9-11 years', 'Engineering manager', 'Designer', 'Data or business analyst', 'Product manager', 'Student','24-26 years','Embedded applications or devices developer', '12-14 years', 'Mobile developer', '15-17 years', 'QA or test developer', 'Game or graphics developer', 'Educator or academic researcher', '21-23 years', 'System administrator', '27-29 years', 'Marketing or sales professional', 'NA')
      y = c(Tyc1,Tyc2,Tyc3,Tyc4,Tyc5,Tyc6,Tyc7,Tyc8,Tyc9,Tyc10,Tyc11,Tyc12,Tyc13,Tyc14,Tyc15,Tyc16,Tyc17,Tyc18,Tyc19,Tyc20,Tyc21,Tyc22,Tyc23,Tyc24,Tyc25,Tyc26,Tyc27,Tyc28,Tyc29,Tyc30,Tyc31)
      #plot(table(x),as.numeric(y),type="l",xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
      #plot(table(x),y,type="o",xlim = NULL,ylim = NULL ,xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
      plot(as.numeric(y),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
      #lines(table(x),y,type="o",col="blue")
    
      output$comp_cd_Graph <- downloadHandler(
        filename = function(){
          paste("dataX",input$fileType, sep = ".")
        },
        content = function(file){
          #if(input$fileType == "pdf")
          #  pdf(file)
         #else
            png(file)
          
          #plot(x,y,type="o",,xlim = NULL,ylim = NULL ,xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
          plot(as.numeric(y),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
          dev.off()
        },
        contentType = "pdf/png"
      ) 
      output$comp_cd_des <- renderText(
        "This graph shows the Company Sizes ranging "
      )
      
    }) 







#General Analysis .....Gender
    output$comp_sz <- renderPlot({
      
        cs1 <- filter(dataX(), dataX()$CompanySize=='20 to 99 employees')
        Tcs1 <- count(cs1)
      
        cs2 <- filter(dataX(), dataX()$CompanySize=='10,000 or more employees')
        Tcs2 = count(cs2)
   
        cs3 <- filter(dataX(), dataX()$CompanySize=='100 to 499 employees')
        Tcs3 = count(cs3)
        
        cs4 = filter(dataX(), dataX()$CompanySize=='10 to 19 employees')
        Tcs4 = count(cs4)
        
        cs5 = filter(dataX(),dataX()$CompanySize=='500 to 999 employees')
        Tcs5 = count(cs5)
        
        cs6 = filter(dataX(),dataX()$CompanySize=='1,000 to 4,999 employees')
        Tcs6 = count(cs6)
        
        cs7 = filter(dataX(), dataX()$CompanySize=='5,000 to 9,999 employees')
        Tcs7 = count(cs7)
        
        cs8 = filter(dataX(),dataX()$CompanySize=='Fewer than 10 employees')
        Tcs8 = count(cs8)
        
        #c9 = filter(dataX(),dataX()$CompanySize=='Netherlands')
        #Tc9 = count(c9)
       
        #c10 = filter(dataX(),dataX()$CompanySize=='Israel')
        #Tc10 = count(c10)
        
      x = c('20 to 99 employees','10,000 or more employees','100 to 499 employees','10 to 19 employees','500 to 999 employees','1,000 to 4,999 employees','5,000 to 9,999 employees','Fewer than 10 employees')
     
      y = c(Tcs1,Tcs2,Tcs3,Tcs4,Tcs5,Tcs6,Tcs7,Tcs8)
      #plot(table(x),as.numeric(y),type="l",xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
      #plot(table(x),y,type="o",xlim = NULL,ylim = NULL ,xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
      plot(as.numeric(y),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
      #lines(table(x),y,type="o",col="blue")
    
      output$comp_sz_Graph <- downloadHandler(
        filename = function(){
          paste("dataX",input$fileType, sep = ".")
        },
        content = function(file){
          #if(input$fileType == "pdf")
          #  pdf(file)
         #else
            png(file)
          
          #plot(x,y,type="o",,xlim = NULL,ylim = NULL ,xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
          plot(as.numeric(y),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
          dev.off()
        },
        contentType = "pdf/png"
      ) 
      output$comp_sz_des <- renderText(
        "This graph shows the Company Sizes ranging "
      )
      
    }) 
    
    
    #NaiveBayes
    output$agegroup1 <- renderPlot({
      
      
      
      caseTita = as.data.frame(dataX())
      first=input$Agegroup1
      second=input$Agegroup
      status= naiveBayes(first~second, data = caseTita)
      
      
      ##x = c('20 to 99 employees','10,000 or more employees','100 to 499 employees','10 to 19 employees','500 to 999 employees','1,000 to 4,999 employees','5,000 to 9,999 employees','Fewer than 10 employees')
      
      ##y = c(Tcs1,Tcs2,Tcs3,Tcs4,Tcs5,Tcs6,Tcs7,Tcs8)
      #plot(table(x),as.numeric(y),type="l",xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
      #plot(table(x),y,type="o",xlim = NULL,ylim = NULL ,xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
      ##plot(as.numeric(y),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
      #lines(table(x),y,type="o",col="blue")
      
      p2 =predict(status,caseTita)
      
      t2 = table(p2,caseTita$first)
      
      plot(as.numeric(t2),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
      
      output$agegraph1 <- downloadHandler(
        filename = function(){
          paste("dataX",input$fileType, sep = ".")
        },
        content = function(file){
          
          png(file)
          
          #plot(x,y,type="o",,xlim = NULL,ylim = NULL ,xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
          ##plot(as.numeric(y),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
          plot(as.numeric(t2),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
          dev.off()
        },
        contentType = "pdf/png"
      ) 
      
      
    }) 
    
    
    output$home2 <- renderPrint({
      
      first <- input$Agegroup
      second <- input$Agegro
      
      caseTita = as.data.frame(dataX())
      
      status= naiveBayes(Country~Age, data = caseTita)
      status
      
    })
    
    output$home3 <- renderPrint({
      
      advertising = dataX()
      
      set.seed(123)
      sample <- sample(c(TRUE, FALSE), nrow(advertising), replace = T, prob = c(0.6,0.4))
      train <- advertising[sample, ]
      test <- advertising[!sample, ]
      
      model1 <- lm(Country ~ Age, data = train)
      model1
      
      #summary(model1)
      

    })
    
    
    
    output$lim <- renderPlot({
      advertising = dataX()
      
      set.seed(123)
      sample <- sample(c(TRUE, FALSE), nrow(advertising), replace = T, prob = c(0.6,0.4))
      train <- advertising[sample, ]
      test <- advertising[!sample, ]
      
      model1 <- lm(Country ~ Age, data = train)
      model1
      
      plot(as.double(model1),type="o")
      
      
      
      
      output$lim3 <- downloadHandler(
        filename = function(){
          paste("dataX",input$fileType, sep = ".")
        },
        content = function(file){
          
          png(file)
          
          #plot(x,y,type="o",,xlim = NULL,ylim = NULL ,xlab="years",ylab="population growth (%)", main="POPULATION GROWTH ALONG THE YEARS",col="red")
          ##plot(as.numeric(y),type="o",xlab="Countries",ylab="Number Of Respondents", main="Respondets per Country",col="red")
          barplot(model,type="o")
          dev.off()
        },
        contentType = "pdf/png"
      ) 
      
      
    }) 


  }
)
