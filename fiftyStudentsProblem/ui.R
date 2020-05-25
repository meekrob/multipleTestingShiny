#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Fifty Students Problem"),
    fluidRow(column(
        br(),
        p(
            "There are 50 students in labs across the world whose advisors read the
same paper and gave them a gene to test for effect on worm metabolic function."
        ),
        br(),
        p(
            "What we know, that they don't, is that the gene,",
            em('nres-1,'),
            "has
          no effect on metabolism, measured by body length.",
            em("Nevertheless,"), "the fifty fledgling scientists proceed with their investigation."
        ),
        hr(),
        width = 12 # column width
    ) # end column
    ), # end fluidRow
    fluidRow(column(
        h3("We'll simulate wildtype worm length using a normal distribution."),
        sliderInput(
            "wildtype_mean",
            "mean worm length (wildtype)",
            min = 5,
            max = 10,
            value = 7
        ),
        sliderInput(
            "wildtype_sd",
            "standard deviation of worm length (wildtype)",
            min = 1,
            max = 5,
            value = 2
        ),
        width = 6
    ),
    column(
        h3("Population distribution of wildtype worm lengths."),
        plotOutput("populationDistribtionPlot"),
        width = 6
    )),
    fluidRow(
        column(
        h3("The fifty students each measure body lengths of 5", em('nres-1'), 
           "knockout worms against 10 wildtype worms."),
        p("They do a two-sample t-test on the two groups. Since", em('nres-1'), 
          "has no effect, we'll draw its sample from the same population that wildtype 
          is drawn from. Shown the graph above."), width=12)
    ),
    fluidRow(
        column(plotOutput("fiftyStudentsPlot"), width=12)
    ),
    fluidRow(
        column(p("aftermath"),width=12)
    )
))