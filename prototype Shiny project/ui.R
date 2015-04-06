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


## Configuration
x_axis_options <- list("Age", "Income", "IQ")
facet_options <- list("None", "Sex")

###############################################################################
## STEP

shinyUI(fluidPage(
    titlePanel("SimTool Demo"),
    
    column(3,
           wellPanel(
               helpText("Adjust predictors to explore how likelihoods change."),
               
               radioButtons("x_axis_choice", label = h3("Select X-Axis"), 
                            choices = x_axis_options, 
                            selected = x_axis_options[[1]]),
               
               radioButtons("facet_choice", 
                            label = h3("Facet Choice"),
                            choices = facet_options,
                            selected = facet_options[[1]]),
               
               sliderInput("ci_choice", 
                           label = "Confidence Interval",
                           min = 0, max = 100, value = 95)
           ),
           
           wellPanel(
               helpText("Adjust fixed (non-x-axis) predictors."),
               
               uiOutput("fixed_predictors")
               )
    ),
    
    column(9, 
           plotOutput("demo_plot")
    )
))

###############################################################################
## END OF SCRIPT
###############################################################################