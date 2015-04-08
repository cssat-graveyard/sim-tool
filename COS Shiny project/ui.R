# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 4/7/2015

###############################################################################
## SCRIPT OVERVIEW

# goal: This is a core Shiny project file. It defines the user interface
#       for the Sim Tool prototype - including which data objects are visible,
#       where they are visible, and how they can be interacted with (if
#       at all.)

# sketch of script
# - Configuration Settings
#   - for defining the options in the various UI features and, if needed,
#     for defining the relationship between the options and the actual
#     data objects
# - Shiny UI Loop
#   - page settings
#   - define the user tools
#   - define the visualization

###############################################################################
## CONFIGURATION SETTINGS
# what you want the user to see on the UI selection features and visualization
# x_axis_options <<- list("mist_scores",
#                         "wrkg_scores",
#                         "recep_scores",
#                         "buyn_scores",
#                         "log_age_eps_begin",
#                         "housing_hs_cnt")
  facet_options <<- list("None", 
                         "Region")

# the relationship between the UI names and the data objects
x_axis_conversions <<- list("Parent mistrust" = "mist_scores",
                            "Parent working score" = "wrkg_scores",
                            "Parent receptivity" = "recep_scores",
                            "Parent buy-in" = "buyn_scores",
                            "Age at removal (log)" = "log_age_eps_begin",
                            "Count of housing hardships" = "housing_hs_cnt")
facet_conversions <<- list("None" = NULL, 
                           "Region" = "REG")

x_axis_options <<- as.list(names(unlist(x_axis_conversions)))

###############################################################################
## SHINY UI LOOP

shinyUI(fluidPage(
    # settings for the entire page
    titlePanel("SimTool Demo"),
    
    # define user tools in the first column
    # width = 3 of 12 (Shiny divides the horizontal space up into 12 sections)
    column(3, 
           wellPanel(
               helpText("Adjust predictors to explore how likelihoods change."),
               
               radioButtons("x_axis_choice", label = h3("Select X-Axis"), 
                            choices = x_axis_options),
               
               radioButtons("facet_choice", 
                            label = h3("Facet Choice"),
                            choices = facet_options)
           ),
           
           wellPanel(
               helpText("Adjust fixed (non-x-axis) predictors."),
               
               uiOutput("fixed_predictors")
           )
    ),
    
    # define the visualization in the second column
    # width = 9 of 12
    column(9, 
           plotOutput("ribbon_plot")
    )
))

###############################################################################
## END OF SCRIPT
###############################################################################