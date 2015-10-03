# Copyright (c) 2015 Ben Zimmer. All rights reserved.

# Web app for visualizing financial goals.

# 2015-09-19: Created.
# 2015-10-03: View date control for viewing changes from month to month.

library(shiny)

source("dsbudget.R")
source("conf.R")      # provides a configuration list used throughout

endDates <- seq.Date(as.Date(conf$endDateStart), by = "month", length.out = 24)
endDates <- setNames(as.integer(endDates), as.character(endDates))

pages <- loadPages(conf$inputFile, conf$inputDir)
spends <- do.call(rbind, lapply(pages, pageToDataFrame))
categories <- sort(unique(spends$categoryName))

app <- shinyApp(
  
  ui = fluidPage(
    
    titlePanel("Financial Goals"),
    
    sidebarLayout(position = "right",
      
      sidebarPanel(
        selectInput("category", "Category", categories,
                    multiple = TRUE, selected = conf$categoryDefault),
        textInput("amount", "Amount", value=conf$amountDefault),
        selectInput(
          "enddate", "End Date", endDates,
          selected = conf$endDateDefault),
        selectInput("type", "Goal Type", c("save", "spend")),
        selectInput(
          "viewdate", "View Date", endDates,
          selected = conf$endDateDefault),
        width = 3
      ),
      
      mainPanel(
        plotOutput("plot")
      )
    )
  ),
  
  
  server = function(input, output) {
    
    output$plot <- renderPlot({
      
      if (length(input$category) > 0) {
        
        curDate <- as.Date(as.integer(input$enddate), origin = "1970-01-01")
        viewDate <- as.Date(as.integer(input$viewdate), origin = "1970-01-01")
        
        goalAmount <- as.numeric(input$amount) * (if (input$type == "save") 1 else -1)
        
        curSpends <- spends %>%
          filter(categoryName %in% input$category) %>%
          filter(spendDate <= viewDate) %>%
          arrange(spendDate)
        
        plotGoals(
          curSpends,
          goalAmount,
          curDate,
          input$type)
      
      } else {
        
        plot(0, type = "n", xaxt = "n", yaxt = "n",
             ann = FALSE)
        
      }
      
    }, height = 720)
  }
)

browseURL("http://localhost:8100")
runApp(app, port = 8100)
