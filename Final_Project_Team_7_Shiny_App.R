#Team #7 R script Final Project. Nick Stelzer, Grant Shireman, Ricardo pignatta

library(shiny)
library(ggplot2)
library(curl)
library(h2o)
library(RCurl)
#Loads the libraries that we want to use

options(scipen = 999)
#removes scientific notation


h2o.init(nthreads=2, max_mem_size="8g")
# Imports the library for h2o into the model and sets the parameters to run the model based on data and cores


#m1 = h2o.loadModel("C:\\Users\\Nick Stelzer\\Box Sync\\Team 7\\Model_Forecasted")
m1 = h2o.loadModel("Model_Forecasted")
#loads the model that was created from the overnight run of h2o

#d = read.csv("C:\\Users\\Nick Stelzer\\Box Sync\\Team 7\\app_data.csv")
d = read.csv("app_data.csv")
#loads the data that was manipulated to easily use to run our descriptive statistics 

ui= fluidPage(
  sidebarPanel(
  selectInput('title','Please Indicate Your Job Title.',choices =sort(unique(na.omit(d$title))))
  ,selectInput('sex','Please Indicate Your Sex.',choices =sort(unique(na.omit(d$gender))))
  ,selectInput('country','Please Indicate The Country You Work Within.',choices =sort(unique(na.omit(d$Country))))
  ,selectInput('state','Please Indicate The State You Work Within.',choices =sort(unique(na.omit(d$State))))
  ,selectInput('education','Please Indicate Your Education Level.',choices =sort(unique(na.omit(d$Education))))
  ,selectInput('race','Please Indicate Your Race.',choices =sort(unique(na.omit(d$Race))))
  ,sliderInput('work_experience','Indicate How Many Years Of Work Experience You Have.',min=0,max=35,value=5)
  ,sliderInput('work_experience_comp','Indicate How Many Years Of Work Experience You Have With Your Current Employer.',min=0,max=35,value=5)
  ,actionButton('run','Forecast Projected Yearly Total Compensation')
  ),
  
  mainPanel(
    textOutput('lead_up')
    ,textOutput('prediction')
    ,fluidRow(
      splitLayout(style = "border: 1px solid silver:", cellWidths = c(750,750), 
                  plotOutput("hist"), 
                  plotOutput("hist1")))
    ,fluidRow(
      splitLayout(style = "border: 1px solid silver:", cellWidths = c(500,500,500), 
                  plotOutput("plotgraph1"), 
                  plotOutput("plotgraph2"),
                  plotOutput("plotgraph3")))
  
 
   ,fluidRow(
    splitLayout(style = "border: 1px solid silver:", cellWidths = c(750,750), 
                plotOutput("plotgraph4"), 
                plotOutput("plotgraph5")))
  ,plotOutput("plotgraph6")
  ,dataTableOutput('fin_data'))
)


server = function(input,output){
  observeEvent(input$run, {
  
  t = reactive({aggregate(totalyearlycompensation~company,data=d[d$title==input$title & d$Country==input$country,],mean)})
  #p = reactive({as.data.frame(h2o.predict(m1,as.h2o(data.frame(timestamp=Sys.Date(),title=input$title,yearsofexperience=input$work_experience,yearsatcompany=input$work_experience_comp,gender=input$sex,Race=input$race,Education=input$education,Country=input$country,State=input$state))))})

  output$lead_up = renderText({"Your Yearly Forecasted Compensation Is:"})

  
  #n = reactive({data.frame(timestamp=Sys.time(),title=input$title,yearsofexperience=input$work_experience,yearsatcompany=input$work_experience_comp,gender=input$sex,Race=input$race,Education=input$education,Country=input$country,State=input$state)})

  #p = reactive({as.h2o(n())})
  #preed = reactive({as.data.frame(h2o.predict(m1,p()))[1,1]})
  
  
  
  #output$prediction= renderText({preed()})
  output$prediction = renderText({as.data.frame(h2o.predict(m1,as.h2o(data.frame(timestamp=Sys.time(),title=input$title,yearsofexperience=input$work_experience,yearsatcompany=input$work_experience_comp,gender=input$sex,Race=input$race,Education=input$education,Country=input$country,State=input$state))))[1,1]})
  
  
  output$hist = renderPlot({ggplot(data= d[d$title==input$title & d$Country==input$country,])+geom_histogram(aes(x=d[d$title==input$title & d$Country==input$country,"totalyearlycompensation"],fill=d[d$title==input$title & d$Country==input$country,"gender"]),position='identity',alpha=0.4)+labs(title=paste(' Annual Total Compensation For A ',input$title, ' Within The Country: ',input$country),fill='Sex')+ylab('Count')+xlab("Total Yearly Compensation")})
  output$hist1 = renderPlot({ggplot(data= d[d$title==input$title & d$Country==input$country,])+geom_histogram(aes(x=d[d$title==input$title & d$Country==input$country,"totalyearlycompensation"],fill=d[d$title==input$title & d$Country==input$country,"Education"]),position='identity',alpha=0.4)+labs(title=paste(' Annual Total Compensation For A ',input$title, ' Within The Country: ',input$country),fill='Education')+ylab('Count')+xlab("Total Yearly Compensation")})  
  
  output$plotgraph1 = renderPlot({ggplot(data= d[d$title==input$title & d$Country==input$country,])+geom_histogram(aes(x=d[d$title==input$title & d$Country==input$country,"basesalary"],fill=d[d$title==input$title & d$Country==input$country,"Education"]),position='identity',alpha=0.4)+labs(title=paste('Base Salary For A ',input$title, ' Within The Country: ',input$country),fill='Education')+ylab('Count')+xlab("Base Salary Compensation")})
  output$plotgraph2 = renderPlot({ggplot(data= d[d$title==input$title & d$Country==input$country,])+geom_histogram(aes(x=d[d$title==input$title & d$Country==input$country,"stockgrantvalue"],fill=d[d$title==input$title & d$Country==input$country,"Education"]),position='identity',alpha=0.4)+labs(title=paste('Stock Grant Value For A ',input$title, ' Within The Country: ',input$country),fill='Sex')+ylab('Education')+xlab("Stock Grant Value")})
  output$plotgraph3 = renderPlot({ggplot(data= d[d$title==input$title & d$Country==input$country,])+geom_histogram(aes(x=d[d$title==input$title & d$Country==input$country,"bonus"],fill=d[d$title==input$title & d$Country==input$country,"Education"]),position='identity',alpha=0.4)+labs(title=paste('Bonus For A ',input$title, ' Within The Country: ',input$country),fill='Educaiton')+ylab('Count')+xlab("Bonus Value")})

  output$plotgraph4 = renderPlot({ggplot(data= d[d$title==input$title & d$Country==input$country,],aes(x=d[d$title==input$title & d$Country==input$country,"yearsofexperience"],y=d[d$title==input$title & d$Country==input$country,"totalyearlycompensation"]))+geom_point(aes(colour= Education))+labs(title=paste('Years Of Experience VS Annual Compensation For A ',input$title, ' Within The Country:',input$country))+ylab('Annual Compensation')+xlab("Years Of Experience")})
  output$plotgraph5 = renderPlot({ggplot(data= d[d$title==input$title & d$Country==input$country,],aes(x=d[d$title==input$title & d$Country==input$country,"yearsatcompany"],y=d[d$title==input$title & d$Country==input$country,"totalyearlycompensation"]))+geom_point(aes(colour= Education))+labs(title=paste('Years At Company VS Annual Compensation For A ',input$title, ' Within The Country: ',input$country))+ylab('Annual Compensation')+xlab("Years At Company")})

  output$plotgraph6 = renderPlot({ggplot(data= d[d$title==input$title & d$Country==input$country,],aes(x=reorder(company,-totalyearlycompensation),y=totalyearlycompensation))+stat_summary(fun.y="mean",geom="bar")+labs(title=paste("Top 10 Companies With The Highest Annual Compensation For ", input$title, " Within The Country: ",input$country))+xlab("Companies")+ylab("Average Annual Compensation")+xlim(t()[order(-t()$totalyearlycompensation),"company"][1:10])})
  output$fin_data = renderDataTable({d[d$title==input$title & d$Country==input$country,2:length(d)]})
  })
  
}


shinyApp(ui = ui, server = server)
#code to tell to run the application
