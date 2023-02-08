library("tidyverse")#for data tidying etc.
library('wesanderson') #nicer visualization
library('googlesheets4') # to import data
library("forecast")
library('shiny')
library('rsconnect')
#library(rsconnect)
#rsconnect::deployApp("path_to_app")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Scotland Housing App"),

  p("This app visualises information from the Scottish Government open Housing dataset."),
  p("It shows the recorded median house prices per region between 1993-2018 and makes a prediction for a desired amount of years"),
  p("This prediction is made using the forecast() R package"),
  p("It also shows a range of other information about the properties across the Council Areas"),
  p('Note that in the Properties per Construction period and Proprerty Type tabs, there are Council Areas for which there is no information'),
  # Sidebar with a slider input for number  of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("which_area", "Which Council Area",
                  c("Scotland",
                    "Aberdeen City",
                                                "Aberdeenshire",
                                                "Angus",
                                                "Argyll and Bute",
                                                "City of Edinburgh",
                                                "Clackmannanshire",
                                                "Dumfries and Galloway",
                                                "Dundee City",
                                                "East Ayrshire",
                                                "East Dunbartonshire",
                                                "East Lothian",
                                                "East Renfrewshire",
                                                "Falkirk",
                                                "Fife",
                                                "Glasgow City",
                                                "Highland",
                                                "Inverclyde",
                                                "Midlothian",
                                                "Moray",
                                                "Na h-Eileanan Siar",
                                                "North Ayrshire",
                                                "North Lanarkshire",
                                                "Orkney Islands",
                                                "Perth and Kinross",
                                                "Renfrewshire",
                                                "Scottish Borders",
                                                "Shetland Islands",
                                                "South Ayrshire",
                                                "South Lanarkshire",
                                                "Stirling",
                                                "West Dunbartonshire", 
                                                "West Lothian"
                                                ), selected = 'Scotland'), 
      numericInput("how_long", "Predict for X years", min = 1, value = 5)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Median Property Price", plotOutput("distPlot")), 
        tabPanel("Properties per Council Tax Band", plotOutput("CouncilTaxPlot")), 
        tabPanel("Properties per Number of Rooms", plotOutput("RoomNumberPlot")),
        tabPanel("Properties per Occupation Type", plotOutput("OccupationTypePlot")),
        tabPanel("Properties per Construction period", plotOutput("PropertyAgePlot")),
        tabPanel("Properties per Property type", plotOutput("PropertyTypePlot"))
      )
    )
  )
)

# Define server logic required to draw plots
server <- function(input, output) {
  #allows to access google sheets - those were taken from the scottish gouvernemnt website
  gs4_deauth()
  
  sheet_url <- "https://docs.google.com/spreadsheets/d/1WBf-MsrwcBfDt6lN4fmqEfMiK1Yse8mVWSEOYXYlU_s/edit?usp=sharing"
  sheet_id <- as_sheets_id(sheet_url)
  HousePrices <- read_sheet(sheet_id, 1, range = "house-sales-prices!B7:AB40")
  
  
  sheet_url <- "https://docs.google.com/spreadsheets/d/1lzzZg1IJ7_rfzIX8jflXrdOW2TSyh22g3RihGUD6pas/edit#gid=0"
  sheet_id <- as_sheets_id(sheet_url)
  CouncilTax <- read_sheet(sheet_id, 1, range = "Sheet1!B8:J41")

  sheet_url <- "https://docs.google.com/spreadsheets/d/1ycLXcAzlHVgoiZ-yJYxjCsPdcxHG5c_CIuKxnuUAAx8/edit?usp=sharing"
  sheet_id <- as_sheets_id(sheet_url)
  RoomNumber <- read_sheet(sheet_id, 1, range = "Sheet1!B8:M41")
  
  sheet_url <- "https://docs.google.com/spreadsheets/d/1nuv60tB_z4KVBibR-MSjrKvo1RdXXCXSGr2W8Vdp1nU/edit?usp=sharing"
  sheet_id <- as_sheets_id(sheet_url)
  OccupationType <- read_sheet(sheet_id, 1, range = "Sheet1!B8:I41")
  
  sheet_url <- "https://docs.google.com/spreadsheets/d/1-nN1645eaKfNsvNte8GKphJ6-4wZA1O_ajVQCca93ME/edit?usp=sharing"
  sheet_id <- as_sheets_id(sheet_url)
  PropertyAge <- read_sheet(sheet_id, 1, range = "Sheet1!B8:H35")
  
  sheet_url <- "https://docs.google.com/spreadsheets/d/1ZWxh_xWYZMpaQ3plS4TCUo8lJG2jFgMMOnsCFLQzvaM/edit?usp=sharing"
  sheet_id <- as_sheets_id(sheet_url)
  PropertyType <- read_sheet(sheet_id, 1, range = "Sheet1!B8:E35")
  
  #convert into a time series
    ts_HousePrices <- ts(HousePrices)

    output$distPlot <- renderPlot({
      fit  <- auto.arima(ts_HousePrices[which(HousePrices[,1]== input$which_area),])
      predictions <- forecast(fit, input$how_long)
      temp <- as.data.frame(predictions$x)%>%filter(between(row_number(), 2, 27))
      observed <- tibble::rownames_to_column(temp, "time")%>%mutate(wh_dat = "observations", time = as.integer(time))%>%dplyr::rename(val = 2)
      temp <- as.data.frame(predictions$mean)
      predicted_mean <- tibble::rownames_to_column(temp, "time")%>%mutate(time = as.integer(time) + as.integer(observed$time[26]))%>%mutate(wh_dat = "my_forecast")%>%dplyr::rename(val = 2)
      temp <- as.data.frame(predictions$upper)
      upper_bound <- tibble::rownames_to_column(temp, "time")%>%mutate(time = as.integer(time) + as.integer(observed$time[26]))%>%mutate(wh_dat = "upper_bound")%>%dplyr::rename(val = 2)%>%select(-3)
      temp <- as.data.frame(predictions$lower)
      lower_bound <- tibble::rownames_to_column(temp, "time")%>%mutate(time = as.integer(time) + as.integer(observed$time[26]))%>%mutate(wh_dat = "lower_bound")%>%dplyr::rename(val = 2)%>%select(-3)
      
      plot(
      ggplot(observed, aes(x = time, y = val/1000)) + geom_line(colour='deepskyblue', size = 2, alpha = 0.7) +
        geom_smooth(aes(x=time, y=val/1000, ymax=upper_bound$val/1000, ymin=lower_bound$val/1000), 
                    colour='red', data=predicted_mean, stat='identity', size = 2)+
        ggtitle(as.character(input$which_area))+
        xlab('Year') + ylab('Median Price in 1000 pounds')+ theme_bw()+ 
        theme(text = element_text(size = 20)) 
   ) 
  })
    output$CouncilTaxPlot <- renderPlot({
      plot(
        CouncilTax %>% filter(`Reference Area` == input$which_area)%>%
          gather('Tax Band', 'Proportion', -`Reference Area`)%>%
          separate(`Tax Band`, c("B", "Tax Band"), " ")%>%select(-B)%>%
          ggplot(aes(x=`Tax Band`, y= Proportion))+ 
          geom_bar(stat = "identity", fill = 'deepskyblue', alpha = 0.7)+ theme_bw()+ 
          theme(text = element_text(size = 20)) + ylab('Proportion of properties in 2021')+
          ggtitle(as.character(input$which_area))
      )
    })
    output$RoomNumberPlot <- renderPlot({
      plot(
        RoomNumber %>% filter(`Reference Area` == input$which_area)%>%
          gather('Number of Rooms per Property', 'Proportion', -`Reference Area`)%>%
          ggplot(aes(x=factor(`Number of Rooms per Property`), y= Proportion))+ 
          geom_bar(stat = "identity", fill = 'deepskyblue', alpha = 0.7)+ theme_bw()+ 
          theme(text = element_text(size = 20)) + ylab('Proportion of properties in 2017') + 
          xlab('Number of Habitable Rooms per Property \n (i.e. bedrooms or living rooms)') + 
          scale_x_discrete(limits=c("1","2","3", "4", "5", "6", "7", "8", "9", "10+", "Unknown"))+
          ggtitle(as.character(input$which_area))
      )
    })
    output$OccupationTypePlot <- renderPlot({
      #we need to clean the columns
      OccupationTypeClean <- OccupationType %>% filter(`Reference Area` == input$which_area)%>%
        gather('Occ', 'Proportion', -`Reference Area`)
      #they have extra text
      OccupationTypeClean$Occ <- gsub("Which Are ", "",OccupationTypeClean$Occ)
      OccupationTypeClean$Occ <- gsub("With ", "",OccupationTypeClean$Occ)
      plot(
        OccupationTypeClean %>%
          ggplot(aes(x=factor(Occ), y= Proportion))+ 
          geom_bar(stat = "identity", fill = 'deepskyblue', alpha = 0.7)+ theme_bw()+ 
          theme(text = element_text(size = 20)) + ylab('Proportion of \nproperties in 2021') + 
          xlab('Types of Occupancy') + theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9))+
          ggtitle(as.character(input$which_area))
      )
    })
    output$PropertyAgePlot <- renderPlot({
      PropertyAge[, 2:7] <- round(PropertyAge[, 2:7]/c(PropertyAge[, 7])*100, 1)
      plot(
        PropertyAge %>% filter(`Reference Area` == input$which_area)%>%select(-All)%>%
          gather('Ages', 'Proportion', -`Reference Area`)%>%
          ggplot(aes(x=factor(Ages), y= Proportion))+ 
          geom_bar(stat = "identity", fill = 'deepskyblue', alpha = 0.7)+ theme_bw()+ 
          theme(text = element_text(size = 20)) + ylab('Proportion of properties in 2021') + 
          xlab('Property construction') +
          ggtitle(as.character(input$which_area))
      )
    })
    output$PropertyTypePlot <- renderPlot({
      PropertyType[, 2:4] <- round(PropertyType[, 2:4]/c(PropertyType[, 2])*100, 1)
      plot(
        PropertyType %>% filter(`Reference Area` == input$which_area)%>%select(-All)%>%
          gather('Type', 'Proportion', -`Reference Area`)%>%
          ggplot(aes(x=factor(Type), y= Proportion))+ 
          geom_bar(stat = "identity", fill = 'deepskyblue', alpha = 0.7)+ theme_bw()+ 
          theme(text = element_text(size = 20)) + ylab('Proportion of properties in 2021') + 
          xlab('Property Type') +
          ggtitle(as.character(input$which_area))
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
