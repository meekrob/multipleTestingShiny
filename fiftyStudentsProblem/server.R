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
    cat("sourcing multiple_test_correction")
    source('../multiple_test_correction.R')
    resample <- eventReactive(input$resample,{})
    
    output$populationDistribtionPlot <- renderPlot({
        # We're plotting the dnorm function, not data,
        # But we have to provide a dummy value for data just so ggplot will run.
        p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
        wt = function(x) { dnorm(x,input$wildtype_mean,input$wildtype_sd)}
        p + stat_function(fun = wt) + xlim( max(0,qnorm(.001, input$wildtype_mean,input$wildtype_sd)),
                                            qnorm(.999, input$wildtype_mean,input$wildtype_sd))
    },height=250, width=400)
    output$fiftyStudentsPlot <- renderPlot({
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Pipetting...", detail="and PCR, and growing worms...", value = 0)
        nothing=resample() # to initiate the graph (I don't think this works)
        
        print(
            fiftyStudents(input$wildtype_mean,
                          input$wildtype_sd, 
                          10, #input$samples_nres,
                          10, #input$samples_wt,
                          as.numeric(input$alpha_reject)))
        progress$inc(.5,"measuring data.")
        
    },height=500)

})
