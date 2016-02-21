
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library (ggplot2)
library (forecast)

refreshRate <- 300
dataWindow <- 20
numDataPoints <- 0
dataVector <- rep (NA, dataWindow)
movingAvgWindow <- 5
movingAvgLength <- dataWindow - movingAvgWindow + 1
movingAvg <- rep (NA, movingAvgLength)

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
        writeLines ("1", con, sep="\n")
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
                
                if (numDataPoints <= dataWindow) {
                        dataVector[numDataPoints] <<- data
                        p <- plot_ly(x = 0, y=data, type = "bar", 
                                     marker=list (color=fillColor))
                }
                else {
                        dataVector <<- dataVector[2:dataWindow]
                        dataVector[dataWindow] <<- data
                        movingAvg <<- na.omit (ma (dataVector, movingAvgWindow))
                        print (movingAvg)
                        p <- plot_ly(x = 0, y=movingAvg[movingAvgLength], type = "bar", 
                                     marker=list (color=fillColor))
                }
                                
                fillColor <- ifelse (movingAvg[movingAvgLength] < 0.5, 
                                     "rgb(233, 102, 44)", "rgb(0, 172, 101)")
                
                
                layout(p, 
                       xaxis = list(range = c(-1, 1), autorange = FALSE,
                                       showticklabels=FALSE, title=""),
                       yaxis = list(title="Score", range = c(0, 1), showticklabels=FALSE,
                                    autorange=FALSE))
        
        })
        
        output$timeFeedbackPlot <- renderPlotly({
                invalidateLater (refreshRate, session)
                if (numDataPoints > dataWindow) {
                        gg <- qplot (1:movingAvgLength, movingAvg, geom="line") + 
                                xlab ("Time (s)") +
                                ylab ("Score") +
                                ylim (0, 1)
                        
                        ggplotly (gg)
                }
        })

        session$onSessionEnded (function() {
                close (con)
        })
})
