#ALY6070 Final Project
#Yuhan Wang


library(shiny)
library(networkD3)
library(data.table)
library(dplyr)
library(leaflet)

#================================ DATA PREPARATION =================================
#part one
frd <- fread("friends.csv") #read the document in data table format
head(frd) 
frd$year <- substr(frd$date_followed, 1,4) #extract the date into year
frd$date_followed <- NULL #remove date_followed column

#part two
##address table
state <- fread("address.csv") #read csv
head(state)
length(state$State) #number of states
state$stateid <- 1:nrow(state) #add a column with consecutive numbers as state id

##user table
userid <- unique(frd$follower_user_id) #extract unique user id
user <- data.frame(userid) #save the integer into a data frame
count(user) #identify how many unique id exist
set.seed(123)
user$stateid <- sample(1:51, 116059, replace = TRUE) #randomly assign the number to each user

#table merge
user_state <- merge(user, state, by = "stateid") #merge user and state table
user_state1 <- merge(user, state, by = "stateid") #create a new user_state table for merging

df <- merge(frd, user_state, by.x = "follower_user_id", by.y = "userid") #merge tables based on follower
df <- merge(df, user_state1, by.x = "followed_user_id", by.y = "userid") #merge tables based on followed
names(df) <- c("follower_user_id", "followed_user_id", "year", 
               "stateid_follower", "state_follower", "Latitude_follower", "Longitude_follower",
               "stateid_followed", "state_followed", "Latitude_followed", "Longitude_followed") #rename

#randomize the order of df
df_random <- df[sample(nrow(df)),] #for part two only

#ui.R
ui <- fluidPage(
  
  # Application title
  titlePanel("ALY6070 Final Project - Friends Network"),
  
  #navbar page design
  navbarPage("Friends & Followers",
    navbarMenu("Part One",
      #tabpanel 1 for random rows selection
      tabPanel("Random Selection",
          h2("User Network"),
          sidebarLayout(
            sidebarPanel(
              #sidebar of assignment part one -- random selection
              sliderInput(inputId = "randomrows",
                          label = strong("Choose the number of user connections you would like to check out: "),
                          min = 1,
                          max = 300,
                          value = 10), #select number of observations
              selectInput(inputId = "year1",
                          label = strong("Select the Year: "),
                          choices = c("2007", "2008", "2009", "2010", "2011", "2012"),
                          selected = 2007), #select the year
                     
              textOutput("selected_var1"),
              br(),
              strong("Notes: "),
              br(),
              "The network plot is showing the relationships between users and their followers.",
              br(),
              "With the increasing number of user connections that have been randomly selected, we can see multiple types of relationship including one-to one, one-to-multiple and multiple-to-multiple."
              
            ),
            # Show the network outputs
            mainPanel(
              tabsetPanel(
                tabPanel("Network Plot",simpleNetworkOutput("networkPlot1")), #network plot
                tabPanel("Data Table",dataTableOutput("networkPlot_table1")) #data table output
              )
            )
          )
        ),
        #tabpanel 2 for top n rows selection
        tabPanel("Order Selection",
          h2("User Network"),
          sidebarLayout(
            sidebarPanel(
              #sidebar of assignment part one -- top n rows selection
              sliderInput(inputId = "toprows",
                          label = strong("Choose the number of user connections you would like to check out: "),
                          min = 1,
                          max = 300,
                          value = 10), #select number of observations
              selectInput(inputId = "year2",
                          label = strong("Select the Year: "),
                          choices = c("2007", "2008", "2009", "2010", "2011", "2012"),
                          selected = 2007), #select the year
                     
                     textOutput("selected_var2"),
              br(),
              strong("Notes: "),
              br(),
              "The network plot is showing the relationships between users and their followers.",
              br(),
              "Compare with the random selection, we are more frequently to see multiple-to-multple relationships among users as the dataset is sorted by user id.",
              br(),
              "It allows us to identify the users' friend network easier than the random selection method."
                   ),
              # Show the network outputs
              mainPanel(
              tabsetPanel(
                tabPanel("Network Plot",simpleNetworkOutput("networkPlot2")), #network plot
                tabPanel("Data Table",dataTableOutput("networkPlot_table2")) #data table output
              )
            )
          )
        )
    ),
        tabPanel("Part Two",
          h2("Geographical User Network"),
          sidebarLayout(
            #sidebar for selecting year-basis number of connections
            sidebarPanel(
              sliderInput(inputId = "randomnum",
                          label = strong("Choose the number of user connections you would like to check out: "),
                          min = 100, max = 300, value = 30),
              #sidebar for year basis results
              selectInput(inputId = "year",
                          label = strong("Select the Year: "),
                          choices = c("2007", "2008", "2009", "2010", "2011", "2012"),
                          selected = 2009), 
              #sidebar for count type
              radioButtons(inputId = "count_type",
                          label = strong("Select the Follow Status"),
                          choices = list("Follower", "Followed", "Both"),
                          selected = "Both"),
              #output the selected year
              textOutput("selected_year"),
              br(),
              strong("Notes: "), 
              br(),
              "The red circle refers to the number of users have been followed by others in the state.", br(),
              "The blue circle refers to the number of users make the following action in the state.", br()
              ),
          mainPanel(
            tabsetPanel(
              tabPanel("Map",leafletOutput("map", height = 550)), #network plot
              tabPanel("Data Table",dataTableOutput("map_table")) #data table output
            )
          )
        )
      )

  )
)


#server.R
server <- function(input, output) {
  
  #==================================== PART ONE =====================================
  #------------------------------ RANDOM ROWS SELECTION ------------------------------
  #reactive
  RandomInput <- reactive({
    switch(input$year1,
           "2007" = subset(frd, year == '2007') %>% sample_n(input$randomrows), #random rows selection for each year
           "2008" = subset(frd, year == '2008') %>% sample_n(input$randomrows),
           "2009" = subset(frd, year == '2009') %>% sample_n(input$randomrows),
           "2010" = subset(frd, year == '2010') %>% sample_n(input$randomrows),
           "2011" = subset(frd, year == '2011') %>% sample_n(input$randomrows),
           "2012" = subset(frd, year == '2012') %>% sample_n(input$randomrows))
  })
  
  #output the text
  output$selected_var1 <- renderText({
    paste("You have randomly chose ", input$randomrows, "connections in", input$year1, ".")
  })

  #output network plot
  output$networkPlot1 <- renderSimpleNetwork({
    networkdataset1 <- data.frame(RandomInput()$follower_user_id, RandomInput()$followed_user_id)
    simpleNetwork(networkdataset1)
  })
  
  #output data table
  output$networkPlot_table1 <- renderDataTable({
    RandomInput()
  })
  
  #------------------------------ TOP N ROWS SELECTION ------------------------------
  #reactive
  TopInput <- reactive({
    switch(input$year2,
           "2007" = subset(frd, year == '2007') [1:input$toprows], #select top n rows for each year
           "2008" = subset(frd, year == '2008') [1:input$toprows],
           "2009" = subset(frd, year == '2009') [1:input$toprows],
           "2010" = subset(frd, year == '2010') [1:input$toprows],
           "2011" = subset(frd, year == '2011') [1:input$toprows],
           "2012" = subset(frd, year == '2012') [1:input$toprows])
  })
  
  #output the text
  output$selected_var2 <- renderText({
    paste("You have chose the first", input$toprows, "connections in", input$year2, ".")
  })
  
  #output network plot
  output$networkPlot2 <- renderSimpleNetwork({
    networkdataset2 <- data.frame(TopInput()$follower_user_id, TopInput()$followed_user_id)
    simpleNetwork(networkdataset2)
  })
  
  #output data table
  output$networkPlot_table2 <- renderDataTable({
    TopInput()
  })
  
  #==================================== PART TWO =====================================
  #----------------------------------- YEAR BASIS ------------------------------------
  #reactive
  YearInput <- reactive({
    if(input$count_type == "Follower"){ #controls the radio button
      switch(input$year,
             "2007" = ((subset(df_random, year == '2007')[1:input$randomnum]) #select the first n rows
                       [, Count_follower:=.N, by = state_follower]), #add a new frequency column, to show the number of followers in each state
             "2008" = ((subset(df_random, year == '2008')[1:input$randomnum])[, Count_follower:=.N, by = state_follower]),
             "2009" = ((subset(df_random, year == '2009')[1:input$randomnum])[, Count_follower:=.N, by = state_follower]),
             "2010" = ((subset(df_random, year == '2010')[1:input$randomnum])[, Count_follower:=.N, by = state_follower]),
             "2011" = ((subset(df_random, year == '2011')[1:input$randomnum])[, Count_follower:=.N, by = state_follower]),
             "2012" = ((subset(df_random, year == '2012')[1:input$randomnum])[, Count_follower:=.N, by = state_follower]))
    }
    else if(input$count_type == "Followed"){
      switch(input$year,
             "2007" = ((subset(df_random, year == '2007')[1:input$randomnum]) #select the first n rows
                       [, Count_followed:=.N, by = state_followed]), #add a new frequency column, to show the number of followers in each state
             "2008" = ((subset(df_random, year == '2008')[1:input$randomnum])[, Count_followed:=.N, by = state_followed]),
             "2009" = ((subset(df_random, year == '2009')[1:input$randomnum])[, Count_followed:=.N, by = state_followed]),
             "2010" = ((subset(df_random, year == '2010')[1:input$randomnum])[, Count_followed:=.N, by = state_followed]),
             "2011" = ((subset(df_random, year == '2011')[1:input$randomnum])[, Count_followed:=.N, by = state_followed]),
             "2012" = ((subset(df_random, year == '2012')[1:input$randomnum])[, Count_followed:=.N, by = state_followed]))
    }
    else if(input$count_type == "Both"){
      switch(input$year,
             "2007" = ((subset(df_random, year == '2007')[1:input$randomnum]) #select the first n rows
                      [, Count_follower:=.N, by = state_follower]) #add a new frequency column, to show the number of followers in each state
                      [, Count_followed:=.N, by = state_followed], #add a new frequency column, to show the number of users have been followerd in each state
             "2008" = ((subset(df_random, year == '2008')[1:input$randomnum])[, Count_follower:=.N, by = state_follower])[, Count_followed:=.N, by = state_followed],
             "2009" = ((subset(df_random, year == '2009')[1:input$randomnum])[, Count_follower:=.N, by = state_follower])[, Count_followed:=.N, by = state_followed],
             "2010" = ((subset(df_random, year == '2010')[1:input$randomnum])[, Count_follower:=.N, by = state_follower])[, Count_followed:=.N, by = state_followed],
             "2011" = ((subset(df_random, year == '2011')[1:input$randomnum])[, Count_follower:=.N, by = state_follower])[, Count_followed:=.N, by = state_followed],
             "2012" = ((subset(df_random, year == '2012')[1:input$randomnum])[, Count_follower:=.N, by = state_follower])[, Count_followed:=.N, by = state_followed])
    }
  })
  
  
  #output the text
  output$selected_year <- renderText({
    paste("You have randomly selected ", input$randomnum, "user connections in", input$year, "." )
  })
  
  
  #output the map
  output$map <- renderLeaflet({
    leaflet(data = YearInput()) %>% #create map widget
      #addTiles() %>% #default openstreet map tiles
      addProviderTiles(providers$Stamen.Terrain) %>% #adds the openstreet map tiles
      
      #lines
      addPolylines(data = data.frame(lon = c(YearInput()$Longitude_follower, YearInput()$Longitude_followed),
                                     lat = c(YearInput()$Latitude_follower, YearInput()$Latitude_followed)),
                   lng = ~lon, lat = ~lat, color = 'black',
                    weight = 0.5, opacity = 0.5) %>% 
      
      #circles based on size
      addCircles(data = YearInput(),
                 lng = ~YearInput()$Longitude_follower,
                 lat = ~YearInput()$Latitude_follower,
                 weight = 0.2, fillOpacity = 0.08,
                 popup = paste(as.character(YearInput()$state_follower), "<br>",
                               "Number of Followers: ", as.character(YearInput()$Count_follower), "<br>"),
                 radius = ~('^'(YearInput()$Count_follower,2)*1500)) %>% ##circles based on the number of followers in each state
      
      addCircles(data = YearInput(),
                 lng = ~YearInput()$Longitude_followed,
                 lat = ~YearInput()$Latitude_followed,
                 weight = 1, fillOpacity = 0.08, color = 'red',
                 popup = paste(as.character(YearInput()$state_followed), "<br>",
                               "Number of Users been Followed: ", as.character(YearInput()$Count_followed), "<br>"),
                 radius = ~('^'(YearInput()$Count_followed,2)*1500)) %>% ##circles based on the number of followered in each state
      
      setView(lng = -98.79113, lat = 39.80667, zoom = 4.4) #locate the map
  })

  #output data table
  output$map_table <- renderDataTable({
    YearInput()[, c(1,2,3,5,9)]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

