
#You can run the application by clicking the 'Run App' button above.

library(shiny)
library(leaflet)
library(tidyverse)

kc_house_data <- read.csv("kc_house_data.csv")

#Define UI for application that displays the visualization
ui <- bootstrapPage(
  
  #Additional styling of the webpage
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  tags$style(HTML(".control-label {color: #6a6a6a}")),
  tags$style(HTML("h2 {align-items: center; display: flex; justify-content: center;
                  background: #2a0707; color: white; height: 5%}")),
  tags$style(HTML("#waterfront + span {color: #6a6a6a; font-weight: bold}")),
  
  #Application title
  titlePanel("House sales in the area of Seattle, WS in the years 2014 - 2015"),
  
  #Main page layout
  mainPanel(
    #Output the map and plots
    leafletOutput("map", width = "1920px", height = "1200px"),
    plotOutput("plot"),
  ),
  #Output the different slides and dropdowns for filtering the house markers
  absolutePanel(top = 100, right = 250, width = "200px",
                sliderInput("price",
                            "Total Price:",
                            min = min(kc_house_data$price),
                            max = max(kc_house_data$price),
                            value = c(1000000, max(kc_house_data$price))),
                #value = range(kc_house_data$price)),
                sliderInput("living_area",
                            "Living area:",
                            min = min(kc_house_data$sqft_living),
                            max = max(kc_house_data$sqft_living),
                            value = range(kc_house_data$sqft_living)),
                sliderInput("bedrooms",
                            "Number of bedrooms:",
                            min = min(kc_house_data$bedrooms),
                            max = max(kc_house_data$bedrooms),
                            value = range(kc_house_data$bedrooms)),
                sliderInput("bathrooms",
                            "Number of bathrooms:",
                            min = min(kc_house_data$bathrooms),
                            max = max(kc_house_data$bathrooms),
                            value = range(kc_house_data$bathrooms)),
                sliderInput("floors",
                            "Number of floors:",
                            min = min(kc_house_data$floors),
                            max = max(kc_house_data$floors),
                            value = range(kc_house_data$floors))
  ),
  absolutePanel(top = 100, right = 30, width = "200px",
                sliderInput("view",
                            "View of the property:",
                            min = min(kc_house_data$view),
                            max = max(kc_house_data$view),
                            value = range(kc_house_data$view)),
                sliderInput("grade",
                            "Grade of the property:",
                            min = min(kc_house_data$grade),
                            max = max(kc_house_data$grade),
                            value = range(kc_house_data$grade)),
                sliderInput("condition",
                            "Condition of the property:",
                            min = min(kc_house_data$condition),
                            max = max(kc_house_data$condition),
                            value = range(kc_house_data$condition)),
                checkboxInput("waterfront",
                            "Has waterfront",
                            value = FALSE),
                selectInput("zipcodes",
                            "Select zip code",
                            unique(kc_house_data$zipcode),
                            multiple = TRUE),
                textInput("id",
                          "Type an ID")
  )
)

# Define server logic required to draw the map and plots
server <- function(input, output) {
  
  #Add calculated column square foot living
  mutatedHouseData <- kc_house_data %>%
    mutate(pricePerSqft = price / sqft_living)
  
  #Add a placeholder row, which is being used when the filters are set in a way that there is no data to be displayed
  mutatedHouseData[nrow(mutatedHouseData) + 1,] <- c(1:22)
  
  #Calculate the difference between the min and max price per square for creating the color column
  pricePerSqftMin <- min(mutatedHouseData$pricePerSqft)
  pricePerSqftMax <- max(mutatedHouseData$pricePerSqft)
  differencePricePerSqft <- pricePerSqftMax - pricePerSqftMin
  
  #Add a column for the color code, based on the price per square foot of the house
  mutatedHouseData <- mutatedHouseData %>%
    mutate(rgb = paste0("rgb(", (pricePerSqft - pricePerSqftMin) / differencePricePerSqft * 255, ", ", abs((pricePerSqft - pricePerSqftMin) / differencePricePerSqft - 1)  * 255, ", ", 0, ")"))

  #Add a column for the custom description of the houses, that pop up when the respective marker is clicked
  mutatedHouseData <- mutatedHouseData %>%
    mutate(popupDescription = paste(sep = "</br>", 
                                    "ID:", id, "",
                                    "Price (Dollars):", format(price, big.mark = ' '), "",
                                    "Living area (square feet):", format(sqft_living, big.mark = ' '), "",
                                    "Number of bedrooms:", bedrooms, "",
                                    "Number of bathrooms:", bathrooms, "",
                                    "Number of Floors:", floors, "",
                                    "View:", paste(view, "/", max(kc_house_data$view)), "",
                                    "Grade", paste(grade, "/", max(kc_house_data$grade)), "",
                                    "Condition", paste(condition, "/", max(kc_house_data$condition)), "",
                                    "Price per square foot:", round(pricePerSqft, 2), ""
    ))
  
  #Add a column for the custom description of the houses, that are shown when the respective marker is hovered over
  mutatedHouseData <- mutatedHouseData %>%
    mutate(labelDescription = paste("House ID:", id))
  
  #Filter markers based on filters that are selected by the user
  filteredData <- reactive({
    filteredHouseData <- filter(mutatedHouseData,
                                price >= input$price[1],
                                price <= input$price[2],
                                sqft_living >= input$living_area[1],
                                sqft_living <= input$living_area[2],
                                bedrooms >= input$bedrooms[1],
                                bedrooms <= input$bedrooms[2],
                                bathrooms >= input$bathrooms[1],
                                bathrooms <= input$bathrooms[2],
                                floors >= input$floors[1],
                                floors <= input$floors[2],
                                view >= input$view[1],
                                view <= input$view[2],
                                grade >= input$grade[1],
                                grade <= input$grade[2],
                                condition >= input$condition[1],
                                condition <= input$condition[2],
                                waterfront >= as.integer(as.logical(input$waterfront)))
    #Additional filter for specifying zip codes
    if (!is.null(input$zipcodes)) {
      filteredHouseData <- filter(filteredHouseData,
             zipcode %in% input$zipcodes)
    }
    #Additional filter for specifying an ID
    if (input$id != "") {
      filteredHouseData <- filter(filteredHouseData,
                                 grepl(input$id, id, fixed=TRUE))
    }
    #Return the filtered dataset, or if empty, the placeholder row
    if (nrow(filteredHouseData) > 0) {
      return(filteredHouseData)
    }
    else {
      return(tail(mutatedHouseData, n = 1))
      
    }
  })
  
  #Render the map that is being displayed on the webbrowser
  output$map <- renderLeaflet({
    leaflet(data = mutatedHouseData) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      setView(lng = -122.2, lat = 47.5, zoom = 11) %>%
      addTiles(group="Dark") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
      addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
      addProviderTiles(providers$CartoDB.Voyager, group="OSM") %>%
      addLayersControl(baseGroups=c('Dark','Light','OSM'))
  })
  
  #Add functionality for dynamically displaying markers based on the user's input
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      addCircleMarkers(opacity = 0.4, 
                       radius = ~sqft_living / 1500 * input$map_zoom / 5,
                       color = ~rgb,
                       ~long, 
                       ~lat, 
                       weight = 1,
                       popup = ~popupDescription,
                       label = ~labelDescription
      )
  })
  
  #Render the plot
  output$plot <- renderPlot({
    ggplot(data = mutatedHouseData) +
      geom_line(mapping = aes(x = date,
                              y = price, group=zipcode,
                              color="grey")) +
      labs(x = "Date", y = "Price")
  })
}

#Run the application 
shinyApp(ui = ui, server = server)
