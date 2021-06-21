library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(rgdal)
library(DT)
options(scipen=999)#to disable scientific notation
#load the data initially 
#load_data<-function(){
corona<-read.csv("covid_data.csv")
df<-data.frame(corona)

cv_min_date = as.Date(min(corona$date),"%Y-%m-%d")
current_date = as.Date(max(corona$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")
regions=unique(corona["location"])

parameter=c("Total cases","Total Death","New cases")

myspdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")
#}



shinyServer(function(input,output){
  #load_data()
  
  
  
  
  
  # line plot for cases
  if (!is.null(corona)) {
    
    
    #first plot
    output$linePlot<-renderPlot({
      temp<-filter(df, location ==input$Regions)
      date<-temp[,"date"]
      date<-as.Date(date)
      minimum_date_of_particular_country<-min(date)
      maximum_date_of_particular_country<-max(date)
      date<-date[date<=as.Date(input$minimum_date)]
      number_of_rows=length(date)
      #print(input$minimum_date)
      
      name<- paste(input$Regions,"Covid-19 Visualization",sep=" ")
      
      
      if(length(date)==0){
        date=0
      }
      #print(date)
      new_cases=temp[,"new_cases"]
      #print(number_of_rows)
      new_cases=new_cases[0:number_of_rows]
      
      
      total_cases=temp[,"total_cases"]
      total_cases=total_cases[0:number_of_rows]
      death=temp[,"total_deaths"]
      death=death[0:number_of_rows]
      
      
      
      if(length(new_cases)==0){
        new_cases=0
      } 
      if(length(total_cases)==0){
        total_cases=0
      } 
      if(length(death)==0){
        death=0
      }
      
      #print(new_cases)
      plot(new_cases~date,type='l',col='blue',xlab='days',ylab='cases',main =name)
      lines(total_cases~date, type = "l", col = "orange")
      lines(death~date, type = "l", col = "red")
      if(date!=0){  # used because in case of countries like yiemene ,for date below 12 april it is giving errot due to zero length of date
        axis(1, date, format(date, "%b %d"), cex.axis = .7,labels = FALSE)
      }
      
      legend("topleft", legend=c("new cases", "deaths","total cases"),
             col=c("blue", "red","orange"), lty = 1:2, cex=0.8)
    })
  }
  
  
  output$value_of_death<-renderText({
    temp<-filter(df, location ==input$Regions)
    temp<-filter(temp, date ==as.Date(input$minimum_date))
    paste0(temp[,"total_deaths"])
    
  })
  
  
  output$value_of_total_cases<-renderText({
    temp<-filter(df, location ==input$Regions)
    temp<-filter(temp, date ==as.Date(input$minimum_date))
    paste0(temp[,"total_cases"])
    
  })
  
  
  output$value_of_new_cases<-renderText({
    temp<-filter(df, location ==input$Regions)
    temp<-filter(temp, date ==as.Date(input$minimum_date))
    paste0(temp[,"new_cases"])
    
  })
  
  
  
  #output of map
  
  
  output$mymap <- renderLeaflet({
    # Create the map data and add ploygons to it
    leaflet(data=myspdf) %>% 
      #addProviderTiles(providers$Stamen.Toner)%>%
      addTiles()%>%
      setView(lat = 1.6596,lng  = 28.0339,zoom=2)%>%  #seted view for algeria
      addPolygons(fillColor = "red",
                  color = "#660000",
                  smoothFactor = 0.5,
                  highlight = highlightOptions(weight = 5,
                                               color = "red",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  
                  label = ~NAME,
                  layerId = ~NAME) # add a layer ID to each shape. This will be used to identify the shape clicked
    
  })
  
  
  observe(
    {  click = input$mymap_shape_click
    #  subset the spdf object to get the lat, lng and country name of the selected shape (Country in this case)
    sub = myspdf[myspdf$NAME==input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
    lat = sub$LAT
    lng = sub$LON
    nm=sub$NAME
    if(is.null(click))
      return()
    else
      leafletProxy("mymap") %>%
      setView(lng = lng , lat = lat, zoom = 2) %>%
      clearMarkers() %>% 
      addMarkers(lng =lng , lat = lat, popup = nm) 
    # using lat long from spdf will not change the view on multiple clicks on the same shape
    
    }
  )
  
  # Observe to display click data when a shape (country in this case) is clicked
  observe(
    {click = input$mymap_shape_click
    sub = myspdf[myspdf$NAME==input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
    lat = sub$LAT
    lng = sub$LON
    nm = sub$NAME
    if(is.null(click))
      return()
    else{
      mydf<-filter(df,as.Date(df$date)==as.Date(current_date)&df$location==nm)
      total_cases=mydf[,"total_cases"]
      output$new_case_for_map <- renderText({paste0(mydf[,"new_cases"])})
      output$country_for_maps <- renderText({paste0(nm)})
      output$total_case_for_map <- renderText({paste0(total_cases)})
      output$total_deat_for_maps <- renderText({paste0(mydf[,"total_deaths"])})
      
      
      
    }}
    
  )
  
  
  
  
  # When map is clicked, show a popup with city info
  
  
  
  
  
  
  
  
  
  
  #output for comparison
  output$comparisonPlot<-renderPlot({
    
    comparaision_parameter=input$parameter
    name<- paste(comparaision_parameter,"Bifurcation with Progressive Period" ,sep=" ")
    
    
    if(comparaision_parameter=="Total cases"){
      comparaision_parameter<-"total_cases"
      
    }
    else if(comparaision_parameter=="Total Death"){
      comparaision_parameter<-"total_deaths"
      
    }
    
    else if(comparaision_parameter=="New cases"){
      comparaision_parameter<-"new_cases"
      
    }
    loc1<-input$reg1
    loc2<-input$reg2
    loc3<-input$reg3
    
    if(!is.null(loc1)){
      plot(filter(df, location ==loc1)[,comparaision_parameter],type='l',col='blue',xlab='days',ylab='cases',main =name)
    }
    if(!is.null(loc2)){
      lines(filter(df, location ==loc2)[,comparaision_parameter],type='l',col='green')
    }
    if(!is.null(loc3)){
      lines(filter(df, location ==loc3)[,comparaision_parameter],type='l',col='red')
    }
    
    legend("topleft", legend=c(loc1, loc2,loc3),
           col=c("blue","green","red"), lty = 1:2, cex=0.8)
    
  })
  
  output$bar<-renderPlot({
    loc1<-input$reg1
    loc2<-input$reg2
    loc3<-input$reg3
    temp_data1= filter(df, location ==input$reg1)
    temp_data2= filter(df, location ==input$reg2)
    temp_data3= filter(df, location ==input$reg3)
    loc1_temp_data<-temp_data1[nrow(temp_data1),]
    loc2_temp_data<-temp_data2[nrow(temp_data2),]
    loc3_temp_data<-temp_data3[nrow(temp_data3),]
    
    countries<-c(rep(loc1,2),rep(loc2,2),rep(loc3,2))
    condition<-rep(c("Total cases","Total deaths"),3)
    values<-c(loc1_temp_data[,"total_cases"],loc1_temp_data[,"total_deaths"],
              loc2_temp_data[,"total_cases"],loc2_temp_data[,"total_deaths"],
              loc3_temp_data[,"total_cases"],loc3_temp_data[,"total_deaths"])
    
    
    data <- data.frame(countries,condition,values)
    #point <- format(big.mark = " ", decimal.mark = ",", scientific = FALSE)
    
    # Grouped
    ggplot(data, aes(fill=condition, y=values, x=countries)) + 
      geom_bar(position="dodge", stat="identity")+
      ggtitle("        \n Overall Region-wise Comparsion \n")+
      theme(
        plot.title = element_text( size=15, face="bold")
        #axis.title.x = element_text(color="blue", size=14, face="bold"),
        #axis.title.y = element_text(color="#993333", size=14, face="bold")
      )#+
    #scale_y_continuous(labels = point)
  })
  
  
  
  
  
  
  
  #info box
  
  # Add points to plot
  output$raw<-renderDT({
    temp_data_for_table<-filter(df,df$location!="World")
    temp_data_for_table%>%
      select(location,date,new_cases,total_cases,total_deaths)%>%
      arrange(desc(total_cases))%>%
      arrange(desc(date))
  })
  
  
  
  
})



























