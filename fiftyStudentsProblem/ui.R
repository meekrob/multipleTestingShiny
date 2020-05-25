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
            value = 7, step = .5
        ),
        sliderInput(
            "wildtype_sd",
            "standard deviation of worm length (wildtype)",
            min = 1,
            max = 5,
            value = 2, step = .5
        ),
        width = 6
    ),
    column(
        h3("Population distribution of wildtype worm lengths."),
        plotOutput("populationDistribtionPlot", height="250px"),
        width = 6
    )),
    fluidRow(
        column(
        h3(strong("False positives:"), "The fifty students each measure body lengths of", em('nres-1'), 
           "knockout worms versus wildtype worms."),
        p("They do a two-sample t-test on the two groups. Since", em('nres-1'), 
          "has no effect, we'll draw its sample from the same population that wildtype 
          is drawn from. Shown the graph above."), 
        width=12)
    ),
    fluidRow(
        column(2,actionButton("resample", "Make them WORK", icon("microscope")
        )),
        column(4,sliderInput("samples_nres", "Number of nres-1 KO worms measured",min=5, value=5, max=10)),
        column(4,sliderInput("samples_wt", "Number of wild-type worms measured",min=5, value=5, max=10)),
        column(2,selectInput("alpha_reject", "p-value threshold",selected=.05,choices=c(".01"=.01, ".05"=.05, ".1"=.1),
                             width="75px"))
        
    ),
    fluidRow(
        column(plotOutput("fiftyStudentsPlot", height="500px"), width=12)
    ),
    fluidRow(
        column(p("aftermath"),width=12)
    )
))
