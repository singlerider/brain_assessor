
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library (ggplot2)

refreshRate <- 100
dataWindow <- 20
numDataPoints <- 0
dataVector <- rep (NA, dataWindow)

con  <- socketConnection(host="", port = 6011, blocking=TRUE,
                         server=TRUE, open="r+")

getDataFromSocket <- function() {
                
        data <- tryCatch ({
                                readLines (con, 1)
                        },
                        error = function (e) {
                                warning (e)
                                return (NA)
                        })
        writeLines ("1", con, sep="")
        if (length (data)==0) data <- NA
        return (as.numeric (data))
}

getData <- function (...) {
        data <- runif (1, 0, 1)
        return (data)
}

shinyServer(function(input, output, session) {
        
        data <- 0.5
        output$liveFeedbackPlot <- renderPlotly({
        
                invalidateLater (refreshRate, session)
                
                data <- getDataFromSocket()
                
                numDataPoints <<- numDataPoints + 1
                
                if (numDataPoints <= dataWindow)
                        dataVector[numDataPoints] <<- data
                else {
                        dataVector <<- dataVector[2:dataWindow]
                        dataVector[dataWindow] <<- data
                }
                                
                fillColor <- ifelse (data < 0.5, "rgb(233, 102, 44)", "rgb(0, 172, 101)")
                p <- plot_ly(x = 0, y=data, type = "bar", marker=list (color=fillColor))
                
                layout(p, 
                       xaxis = list(range = c(-1, 1), autorange = FALSE,
                                       showticklabels=FALSE, title=""),
                       yaxis = list(title="Score", range = c(0, 1), autorange=FALSE))
        
        })
        
        output$timeFeedbackPlot <- renderPlotly({
                invalidateLater (refreshRate, session)
                gg <- qplot (1:dataWindow, dataVector, geom="line") + 
                        xlab ("Time (s)") +
                        ylab ("Score")
                
                ggplotly (gg)
        })

        session$onSessionEnded (function() {
                close (con)
        })
})
