#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
source('../multiple_test_correction.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$populationDistribtionPlot <- renderPlot({
        # provide a dummy dataset
        p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
        wt = function(x) { dnorm(x,input$wildtype_mean,input$wildtype_sd)}
        p + stat_function(fun = wt) + xlim(-20,20)
    })
    output$fiftyStudentsPlot <- renderPlot({
        print(fiftyStudents(input$wildtype_mean,input$wildtype_sd))
    })

})
