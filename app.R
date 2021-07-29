#Ensure the appropriate packages below are in the library 

library(shiny)
library(shinythemes)
library(shinydashboard)
library(readr)
library(dplyr)
library(plotly)
library(reshape2)
library(ggplot2)
library(choroplethr)
library(TSDT)
library(pals)
library(scales)


#load the data sets 

gccdata <- read.csv('GCC non hydrocarbon exports.csv', stringsAsFactors = T)
totalexports<- read.csv('GCC Total exports.csv', stringsAsFactors = T)


# User Interface---------------------------------------------------------------


shinyApp(

# Main Panel

  ui = navbarPage("Exploring Non-Hydrocarbon Exports of the GCC from 2014-2019", theme = shinytheme("darkly"),
    tabPanel("Main",
        sidebarLayout(
          sidebarPanel(
            selectInput("country", 
                        label = "Countries:",
                        choices = c(unique(unfactor(totalexports$Country))),
                        selected = c(unique(unfactor(totalexports$Country)))[1],
                        width = 400),
                               
            width = 3
             ),
              mainPanel(
                fluidRow(
                  tags$img(src='crude-oil-price-history-chart.png',height="100%", width="100%")
                  ),
                               
                    hr(),
                            
                fluidRow(
                  plotlyOutput("graph")
                                 
                 )
               )
             )
   ),
  # Bahrain's panel -------------------------------------------------------------
  tabPanel("Bahrain",
    sidebarLayout(
      sidebarPanel(
                               
                               
              sliderInput("years1",
                          label="Choose years",
                          min = min(gccdata$Year),
                          max = max(gccdata$Year),
                          step=1,
                          sep ='',
                          value = c(min(gccdata$Year), max(gccdata$Year))),
                width = 3
              ),
              mainPanel(
                fluidRow(
                  plotlyOutput("chart1")
                )
                               
              )
             )
  ),
                  
                  
  # Kuwait's panel -----------------------------------------------------
  
  tabPanel("Kuwait",
    sidebarLayout(
     sidebarPanel(
                               
                               
            sliderInput("years2",
                        label="Choose years",
                        min = min(gccdata$Year),
                        max = max(gccdata$Year),
                        step=1,
                        sep ='',
                        value = c(min(gccdata$Year), max(gccdata$Year))),
            width = 3
           ),
            mainPanel(
              fluidRow(
                plotlyOutput("chart2")
              )
                               
            )
          )
  ),
                  
  # Saudi Arabia's panel -------------------------------------------------
  
  tabPanel("Saudi Arabia",
   sidebarLayout(
    sidebarPanel(
                               
                               
          sliderInput("years3",
                      label="Choose years",
                      min = min(gccdata$Year),
                      max = max(gccdata$Year),
                      step=1,
                      sep ='',
                      value = c(min(gccdata$Year), max(gccdata$Year))),
          width = 3
          ),
            mainPanel(
              fluidRow(
                plotlyOutput("chart3")
              )
                               
            )
          )
  ),
                  
  # Qatar's panel ----------------------------------------------
  
  tabPanel("Qatar",
    sidebarLayout(
      sidebarPanel(
                               
                               
            sliderInput("years4",
                        label="Choose years",
                        min = min(gccdata$Year),
                        max = max(gccdata$Year),
                        step=1,
                        sep ='',
                        value = c(min(gccdata$Year), max(gccdata$Year))),
             width = 3
             ),
                mainPanel(
                  fluidRow(
                    plotlyOutput("chart4")
                  )
                               
                )
              )
  ),
                  
  # UAE's panel
  
  tabPanel("UAE",
    sidebarLayout(
      sidebarPanel(
                               
                               
            sliderInput("years5",
                         label="Choose years",
                         min = min(gccdata$Year),
                         max = max(gccdata$Year),
                         step=1,
                         sep ='',
                         value = c(min(gccdata$Year), max(gccdata$Year))),
             width = 3
             ),
               mainPanel(
                fluidRow(
                  plotlyOutput("chart5")
                 )
                               
                )
              )
  ),
                  
  #Oman's panel
  
  tabPanel("Oman",
    sidebarLayout(
      sidebarPanel(
                               
                               
            sliderInput("years6",
                        label="Choose years",
                        min = min(gccdata$Year),
                        max = max(gccdata$Year),
                        step=1,
                        sep ='',
                        value = c(min(gccdata$Year), max(gccdata$Year))),
             width = 3
             ),
               mainPanel(
                fluidRow(
                  plotlyOutput("chart6")
                 )
                               
               )
             )
           )
 ),
 
  #SERVER ----------------------------------------------------------------------
 
  server <- function(input, output) {
    
    output$graph <- renderPlotly({
      
      
      df<- totalexports[totalexports$Country==input$country,]
      
      
      fig <- plot_ly(x = ~df$Year, 
                     y = ~df$Hydrocarbon.Exports, 
                     type = 'scatter', 
                     mode = 'lines', 
                     name = 'Hydrocarbon Exports', 
                     fillcolor = 'blue',
                     fill = 'tozeroy')
      
      fig <- fig %>% add_trace(x =~df$Year, 
                               y = ~df$Total.Exports, 
                               name = 'Non Hydrocarbon Exports + Hydrocarbon Exports', 
                               fillcolor = 'red',
                               fill = 'tonexty')
      
      fig <- fig %>% layout(xaxis = list(title = 'Year'),
                            yaxis = list(title = 'Export'),
                            title=input$country)
      
      
      fig
    })
    
    # setting the charts 
    
    output$chart1 <- renderPlotly({
      
      country='Bahrain'
      df<- gccdata[gccdata$Country==country,]
      from<-input$years1[1]
      to<-input$years1[2]
      
      df<- df[df$Year>=from & df$Year<=to,]
      
      
      sbc<-ggplot(data=df, aes(fill=Section, y=Trade.Value, x=Year)) 
      
      # stacked bar chart 
      
      p<-sbc+geom_bar(position="stack", stat="identity")+
        scale_y_continuous(labels = comma)+
        scale_fill_manual(values = as.vector(alphabet(26)))
      
      # make the charts interactive with ggplotly
      
      p <- ggplotly(p)
      p
    })
    
    
    output$chart2 <- renderPlotly({
      
      country='Kuwait'
      df<- gccdata[gccdata$Country==country,]
      from<-input$years2[1]
      to<-input$years2[2]
      
      df<- df[df$Year>=from & df$Year<=to,]
      
      
      sbc<-ggplot(data=df, aes(fill=Section, y=Trade.Value, x=Year)) 
      
      # stacked bar chart 
      
      p<-sbc+geom_bar(position="stack", stat="identity")+
        scale_y_continuous(labels = comma)+
        scale_fill_manual(values = as.vector(alphabet(26)))
      
      # make the charts interactive with ggplotly
      
      p <- ggplotly(p)
      p
    })
    
    output$chart3 <- renderPlotly({
      
      country='Saudi Arabia'
      df<- gccdata[gccdata$Country==country,]
      from<-input$years3[1]
      to<-input$years3[2]
      
      df<- df[df$Year>=from & df$Year<=to,]
      
      
      sbc<-ggplot(data=df, aes(fill=Section, y=Trade.Value, x=Year)) 
      
      #stacked bar chart 
      
      p<-sbc+geom_bar(position="stack", stat="identity")+
        scale_y_continuous(labels = comma)+
        scale_fill_manual(values = as.vector(alphabet(26)))
      
      # make the charts interactive with ggplotly
      
      p <- ggplotly(p)
      p
    })
    
    output$chart4 <- renderPlotly({
      
      country='Qatar'
      df<- gccdata[gccdata$Country==country,]
      from<-input$years4[1]
      to<-input$years4[2]
      
      df<- df[df$Year>=from & df$Year<=to,]
      
      
      sbc<-ggplot(data=df, aes(fill=Section, y=Trade.Value, x=Year)) 
      
      # stacked bar chart
      
      p<-sbc+geom_bar(position="stack", stat="identity")+
        scale_y_continuous(labels = comma)+
        scale_fill_manual(values = as.vector(alphabet(26)))
      
      # make the charts interactive with ggplotly
      
      p <- ggplotly(p)
      p
    })
    
    output$chart5 <- renderPlotly({
      
      country='UAE'
      df<- gccdata[gccdata$Country==country,]
      from<-input$years5[1]
      to<-input$years5[2]
      
      df<- df[df$Year>=from & df$Year<=to,]
      
      
      sbc<-ggplot(data=df, aes(fill=Section, y=Trade.Value, x=Year)) 
      
      # stacked bar chart 
      
      p<-sbc+geom_bar(position="stack", stat="identity")+
        scale_y_continuous(labels = comma)+
        scale_fill_manual(values = as.vector(alphabet(26)))
      
      # make the charts interactive with ggplotly
      
      p <- ggplotly(p)
      p
    })
    
    output$chart6 <- renderPlotly({
      
      country='Oman'
      df<- gccdata[gccdata$Country==country,]
      from<-input$years6[1]
      to<-input$years6[2]
      
      df<- df[df$Year>=from & df$Year<=to,]
      
      
      sbc<-ggplot(data=df, aes(fill=Section, y=Trade.Value, x=Year)) 
      
      #stacked bar chart 
      
      p<-sbc+geom_bar(position="stack", stat="identity")+
        scale_y_continuous(labels = comma)+
        scale_fill_manual(values = as.vector(alphabet(26)))
      
      # make the charts interactive with ggplotly
      
      p <- ggplotly(p)
      p
    })
    
  }
)
