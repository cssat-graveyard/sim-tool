# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 4/8/2015

###############################################################################
## SCRIPT OVERVIEW

# goal: This is a core Shiny project file. It defines the user interface
#       for the Sim Tool prototype - including which data objects are visible,
#       where they are visible, and how they can be interacted with (if
#       at all.)

# sketch of script
# - Configuration Settings
#   - where the admin defines the roles of the model variables and some other
#     basic information
# - Configuration Processing
#   - the provided settings are processed so they are ready to be used with
#     the Shiny UI Loop and the simulation/visualization functions
# - Shiny UI Loop
#   - page settings
#   - define the user tools
#   - define the visualization

###############################################################################
## CONFIGURATION SETTINGS
# provide key variable information (in this order, as strings)
# - the user-friendly name of the variable
# - the raw name of the variable (dataset column name)
# - what role the variable plays in the model (outcome or predictor)
# - what role the variable should play in the visualization (x-axis, x-axis + 
#   slider, facet)
variable_configuration <-list(
    c("Parent Mistrust", "mist_scores", "predictor", "x-axis + slider"),
    c("Parent Working Score", "wrkg_scores", "predictor", "x-axis + slider"),
    c("Parent Receptivity", "recep_scores", "predictor", "x-axis + slider"),
    c("Parent Buy-in", "buyn_scores", "predictor", "x-axis + slider"),
    c("Age at Removal (log)", "log_age_eps_begin", "predictor", "x-axis + slider"),
    c("Count of Housing Hardships", "housing_hs_cnt", "predictor", "x-axis + slider"),
    c("Region", "REG", "predictor", "facet")
)

###############################################################################
## CONFIGURATION PROCESSING

# reformat the variable information to make it easy to work with
variable_configuration <- do.call(rbind, variable_configuration)
variable_configuration <- data.frame(variable_configuration, 
                                     stringsAsFactors = FALSE)
names(variable_configuration) <- c("pretty_name", "raw_name", 
                                   "model_role", "ui_role")

# capture key subsets and features
x_axis_options <- filter(variable_configuration, ui_role == "x-axis + slider" | 
                               ui_role == "x-axis"
                         )[["pretty_name"]]
facet_options <- filter(variable_configuration, ui_role == "facet"
                        )[["pretty_name"]]

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
                            choices = c("None", facet_options))
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