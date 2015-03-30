# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 3/27/2015

###############################################################################
## SCRIPT OVERVIEW

# goal: This is a core Shiny project file. It defines the user interface
#       for the Sim Tool prototype - including which data objects are visible,
#       where they are visible, and how they can be interacted with (if
#       at all.)

# sketch of script
# - the shinyUI loop makes up the entirety of the script - it defines the
#   the following paired interface/object sections
#       - gator prototype

###############################################################################
## STEP

shinyUI(fluidPage(
    titlePanel("SimTool Demo"),
    
    column(3,
           wellPanel(
               helpText("Adjust predictors to explore how likelihoods change."),
               
               radioButtons("predictor_choice", label = h3("Select X-Axis"), 
                            choices = list("Age", "Income", "IQ"), 
                            selected = "Age"),
               
               radioButtons("facet_choice", 
                            label = h3("Facet Choice"),
                            choices = list("None", "Sex"),
                            selected = "None"),
               
               sliderInput("ci", 
                           label = "Confidence Interval",
                           min = 0, max = 100, value = 95)
           ),
           
           conditionalPanel(
               condition = "input.predictor_choice == 'Age'",
               sliderInput("income_set", label = h3("What income?"),
                           min = 0, max = 100000, value = 20000),
               sliderInput("IQ_set", label = h3("What IQ?"),
                           min = 0, max = 200, value = 100)
           ),
           
           conditionalPanel(
               condition = "input.predictor_choice == 'Income'",
               sliderInput("age_set", label = h3("What age?"),
                           min = 0, max = 17, value = 9),
               sliderInput("IQ_set", label = h3("What IQ?"),
                           min = 0, max = 200, value = 100)
           ),
           
           conditionalPanel(
               condition = "input.predictor_choice == 'IQ'",
               sliderInput("age_set", label = h3("What age?"),
                           min = 0, max = 17, value = 9),
               sliderInput("income_set", label = h3("What income?"),
                           min = 0, max = 100000, value = 20000)
           )
    ),
    
    column(9, 
           plotOutput("demo_plot")
    )
))

###############################################################################
## END OF SCRIPT
###############################################################################