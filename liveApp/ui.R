
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library (shiny)
library (plotly)

shinyUI(fluidPage(

                fluidRow (
                        column(2,
                               h4 ("Live Feedback"),
                               plotlyOutput("liveFeedbackPlot")
                        ),
                        column(10,
                               tags$div (align="center", 
                                        h4 ("Feedback Over Time")
                                ),
                               plotlyOutput("timeFeedbackPlot")
                       )
                )
        )
)

