# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 

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
    
    sidebarLayout(
        sidebarPanel(
            helpText("Adjust predictors to explore how likelihoods change."),
            
            radioButtons("facet_choice", 
                         label = h3("Facet Choice"),
                         choices = list("None", "Sex"),
                         selected = "None"),
            
            selectInput("predictor_choice", label = h3("Select X-Axis"), 
                        choices = list("Age", "Income", "IQ"), 
                        selected = "Age"),
            
            sliderInput("ci", 
                        label = "Confidence Interval",
                        min = 0, max = 100, value = 95)
        ),
        
        mainPanel(
            plotOutput("demo_plot")
        )
    )
))

###############################################################################
## END OF SCRIPT
###############################################################################