library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(rgdal) 
library(DT)
shinyUI(
  dashboardPage(
    dashboardHeader(title = "COVID-19 Tracker",dropdownMenuOutput("messageMenu")),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Region Wise Analysis", tabName = "cases" ,icon = icon("chart-bar")),
        menuItem("Comparision", tabName = "comparisions", icon = icon("chart-line")),
        menuItem("Map Analysis", tabName = "map_analysis", icon = icon("globe")),
        menuItem("Raw Data", tabName = "RawData", icon = icon("question-circle"))
      )
    ),
    dashboardBody(
      tags$head(tags$style(HTML(".small-box {height: 124px}"))),
      tabItems(
        # First tab content
        tabItem(tabName = "cases",
                fluidRow(
                  box(width=12,
                    
                    column(width=6,h3("Select Region"),
                   # title = "Select Region",
                    selectInput("Regions", "", c(regions),selected = "World")
                  ),
                  column(width=6,
                    h3("Select Time Period"),
                    #title = "Select Date",
                    sliderInput("minimum_date",
                                "",
                                min = as.Date(min(corona$date),"%Y-%m-%d"),
                                max = as.Date(max(corona$date),"%Y-%m-%d"),
                                value=as.Date("2020-04-21"),
                                timeFormat="%d %b"),
                  )
                  )
                  
                ),
                fluidRow(
                  box(plotOutput("linePlot" )),
                  valueBox(subtitle = (h2("Number of New Cases ")),value=textOutput("value_of_new_cases"),color="purple",icon=icon("arrow-up"),width=6),
                  valueBox(textOutput("value_of_total_cases"),h2("Number of Total Cases "),icon=icon("globe"),color="yellow",width=6),
                  valueBox(textOutput("value_of_death"),h2("Number of Total Deaths "),icon=icon("ambulance"),color="red",width=6),
       
                 
                ),
                
                
                
                
                
                
                
                
                
                
                
        ),
        
        # Second tab content
        tabItem(tabName = "comparisions",
                fluidRow(
                  box(
                    h3("Select 3 regions for comparision"),
                    column(width = 3,selectInput("reg1", "",choices =regions ,selected = "India")),
                      column(width = 3,selectInput("reg2", "",choices =regions,selected = "Israel")),
                      column(width = 3,selectInput("reg3", "",choices =regions,selected = "Brazil" )),
                    
                    
                    
                    #selectInput("myselect",h4("Select regions for comparison"),choices = regions,multiple =TRUE,selected="India"),
                    #  verbatimTextOutput("selected"),
                      #"select atmost 3 regions for better visualization"
                      
                      ),
                  #box(h2("select comparison parameter"))
                  box(h3("Select the parameter for the comparison"),
                    #radioButtons("parameter","Select the parameter for the comparison",choices=c("Total cases","Total Death","New cases"))
                    selectInput("parameter", "",choices =parameter ,selected = "Total cases")
                  )
                ),
                fluidRow(
                  box(plotOutput("bar")),
                  box(plotOutput("comparisonPlot" )),
                  
                  
                  
                )
                
                
        ),
        
        
        
        
        
        tabItem(tabName = "map_analysis",
                
                fluidRow(
                  valueBox(textOutput("country_for_maps"),("Country "),color="light-blue",width=3),
                  valueBox(subtitle = ("Number of New Cases "),value=textOutput("new_case_for_map"),color="light-blue",width=3),
                  valueBox(textOutput("total_case_for_map"),("Number of Total Cases "),color="light-blue",width=3),
                  valueBox(textOutput("total_deat_for_maps"),("Number of Total Deaths "),color="light-blue",width=3)
                  
                  
                  
                ),
                
                
                
                fluidRow(
  
                  leafletOutput("mymap")
                )
                
        ),
        
        
        
        tabItem(tabName = "RawData",
                
                fluidRow(
                  h3("Raw Data of Covid-19"),
                  "source: https://ourworldindata.org/coronavirus-source-data\n",
                  DTOutput("raw")))
        
        
        
        
    )
      
  )
)
)



