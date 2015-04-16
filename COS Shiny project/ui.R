# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 4/14/2015

###############################################################################
## SCRIPT OVERVIEW

# goal: This is a core Shiny project file. It defines the user interface
#       for the Sim Tool prototype - including which data objects are visible,
#       where they are visible, and how they can be interacted with (if
#       at all.)

# sketch of script
# - Load Supporting Libraries
#   - anything that might fail if the script needs to wait for server.R to load
#
# - Configuration Settings
#   - where the admin defines the roles of the model variables and some other
#     basic information
#   - also provide the POC color scheme to be called during plot creation
#
# - Configuration Processing
#   - the provided settings are processed so they are ready to be used with
#     the Shiny UI Loop and the simulation/visualization functions
#
# - Shiny UI Loop
#   - page settings
#   - define the user tools
#   - define the visualization

###############################################################################
## LOAD SUPPORTING LIBRARIES
library(dplyr)      # serves various formatting needs

###############################################################################
## CONFIGURATION SETTINGS

# provide key variable information (in this order, as strings)
# - the user-friendly name of the variable
# - the raw name of the variable (dataset column name)
# - what role the variable plays in the model (outcome or predictor)
# - what role the variable should play in the visualization (x-axis, x-axis + 
#   slider, facet)
variable_configuration <<- list(
    c("Parent Mistrust", "mist_scores", 
      "predictor", "x-axis + slider"),
    c("Parent Working Score", "wrkg_scores", 
      "predictor", "x-axis + slider"),
    c("Parent Receptivity", "recep_scores", 
      "predictor", "x-axis + slider"),
    c("Parent Buy-in", "buyn_scores", 
      "predictor", "x-axis + slider"),
    c("Age at Removal (log)", "log_age_eps_begin", 
      "predictor", "x-axis + slider"),
    c("Count of Housing Hardships", "housing_hs_cnt", 
      "predictor", "x-axis + slider"),
    c("Region", "REG", 
      "predictor", "facet")
)


# provide the POC colors
poc_colors <<- c("#3B6E8F", "#A2B69A", "#A3DCE6", "#A784B4")
portal_colors <<- c("#D9BB32", "#6DB33F", "#6E9CAE", "#B1662B", "#5B8067", 
                    "#444D3E", "#994D3E", "#10475B", "#7D6E86", "#D47079", 
                    "#262F1D", "#B0B0B0")
rage_colors <<- portal_colors[c(1, 2, 3, 4)]

###############################################################################
## CONFIGURATION PROCESSING

# reformat the variable information to make it easy to work with
variable_configuration <<- do.call(rbind, variable_configuration)
variable_configuration <<- data.frame(variable_configuration, 
                                      stringsAsFactors = FALSE)
names(variable_configuration) <<- c("pretty_name", "raw_name", 
                                    "model_role", "ui_role")

# capture key subsets and features
x_axis_options <<- filter(variable_configuration, ui_role == "x-axis + slider" | 
                              ui_role == "x-axis"
)[["pretty_name"]]
facet_options <<- filter(variable_configuration, ui_role == "facet"
)[["pretty_name"]]
slider_options <<- filter(variable_configuration, ui_role == "x-axis + slider")

###############################################################################
## SHINY UI LOOP

shinyUI(navbarPage(
    "The Case Outcome Simulator",
    
    # using COS to explore across a range ("Explore Mode")
    tabPanel("Explore Mode", fluidPage(
        # define user tools in the first column
        # width = 3 of 12 (Shiny divides the horizontal space up into 12 sections)
        column(3, 
               wellPanel(               
                   radioButtons("x_axis_choice", label = h3("Select X-Axis"), 
                                choices = x_axis_options),
                   
                   radioButtons("facet_choice", 
                                label = h3("Facet Choice"),
                                choices = c("None", facet_options))
               ),
               
               wellPanel(
                   helpText("Adjust fixed (non-x-axis) predictors."), 
                   
                   checkboxInput("slider_show", 
                                 label = "Show sliders?",
                                 FALSE),
                   
                   conditionalPanel(
                       condition = "input.slider_show == true",
                       
                       actionButton("update_explore_data",
                                    "Update"),
                       
                       uiOutput("explore_slider_set")
                   )
               )
        ),
        
        # define the visualization in the second column
        # width = 9 of 12
        column(9, 
               plotOutput("ribbon_plot")
        )
    )),
    
    tabPanel("Single Case Mode", fluidPage(
        column(3, 
               wellPanel(
                   actionButton("update_sc_data",
                                "Update"),
                   
                   helpText("Adjust predictors."),
                   
                   uiOutput("sc_slider_set")
               )
        ),
        
        column(9,
            plotOutput("error_bar_plot"),
            
            plotOutput("dot_cloud_plot")
        )
    ))
))

###############################################################################
## END OF SCRIPT
###############################################################################