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
same paper and gave them a gene to test."
        ),
        p(
            "It is",
            em('nres-1,'),
            "and despite indications,", em("it has no effect on worm length."),
        ),
        p(
            "Nevertheless, the fifty fledgling scientists proceed with their investigation, each unaware of the other 49."
        ),
        hr(),
        width = 8 # column width
    ), # end column
    column(img(src="advisor_flipped.png",height="100"),p('"Test', em("nres-1"),'"'), width=4),
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
        p("These controls determine the (theoretical) population that wildtype samples are drawn from."), 
        width = 6
    ),
    column(
        h3("Wildtype worm lengths."),
        plotOutput("populationDistribtionPlot", height="250px"),
        
        h4("Main idea: experiments on non-effect genes will also product samples from this distribution."),
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
        #column(4,sliderInput("samples_nres", "Number of nres-1 KO worms measured",min=5, value=5, max=10)),
        #column(4,sliderInput("samples_wt", "Number of wild-type worms measured",min=5, value=5, max=10)),
        column(4,selectInput("alpha_reject", "p-value threshold",selected=.05,choices=c(".01"=.01, ".05"=.05, ".1"=.1),
                             width="75px")),
        column(4,actionButton("resample", "RUN experiments", icon("microscope")
        ))
        
    ),
    fluidRow(
        column(plotOutput("fiftyStudentsPlot", height="500px"), width=12)
    ),
    fluidRow(
        column(p("aftermath"),width=12)
    )
))
