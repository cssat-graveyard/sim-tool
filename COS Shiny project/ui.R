# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 5/12/2015

###############################################################################
## SCRIPT OVERVIEW

# GOAL: ui.R is a core Shiny project file that defines and arranges the 
#       interface features for interacting with Shiny data objects (defined
#       in server.R).
#
#       For the Multinomial Outcome Simulator (MOS) application, the ui.R file
#       also handles loading any R resources needed by ui.R and server.R, along
#       with the relevant config file (e.g., NAME_config.R) to load the base 
#       data, base formula, and variable configuration list needed for the MOS 
#       application to function correctly.

# SCRIPT OUTLINE:
# - Load Supporting Packages and Scripts
#   - ui.R is run before server.R, so it makes sense to load any resources 
#     needed for either script here.
#
# - Load MOS Configuration Script
#   - This it is the place where the application adminstrator defines:
#     (a) what data will be used by the MOS
#     (b) the formula relating predictor attributes to the outcome attribute
#     (c) which variables will be visible in the UI, how they can be 
#         interacted with (slider or facet), and how they should be
#         understood by users
#     (d) (optional) custom visualization colors
#     (e) (optional) custom bootstrap.css file to format the application
#
# - Configuration Processing
#   - Key MOS configuration features are processed so they are ready to be used 
#     with the Shiny UI Loop and the simulation/visualization functions.
#
# - Shiny UI Loop
#   - Global Application Settings
#   - Ribbon Plot UI and Visualization
#   - Dot Cloud Plot UI and Visualization

###############################################################################
## LOAD SUPPORTING LIBRARIES AND SET ANY DEFAULT OPTIONS
library(dplyr)      # serves various formatting needs
library(shinyBS)    # expands base Shiny features (e.g., popovers)
library(Cairo)      # supports plot quality across devices
library(ggplot2)    # for specifying cos theme

options(shiny.usecairo=T)

source("COS sim and vis functions.R")
source("COS custom mlogitsimev.R")

###############################################################################
## CONFIGURATION SETTINGS

source("MOS_config.R")

# define visualization theme
cos_theme <<- theme_bw(16) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          strip.text = element_text(color = "white"),
          axis.text = element_text(size = 12),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 3),
          plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
          aspect.ratio = 2 / (1 + sqrt(5))
    )

###############################################################################
## CONFIGURATION PROCESSING (AUTO-GENERATE ADDITIONAL USEFUL FEATURES)

# extract the fixed ui options (the levels for any ui features that are
# generated statically - such as the x-axis choices - rather than dynamically -
# such as the sliders)
get_fixed_ui_options <- function(variable_config_list) {
    # collect the x-axis options names and definitions
    x_axis_options <- c()
    x_axis_definitions <- c()
    for(index in 1:length(variable_config_list)) {
        if(variable_config_list[[index]]$x_axis_candidate) {
            current_name       <- variable_config_list[[index]]$pretty_name
            current_def        <- variable_config_list[[index]]$definition
            x_axis_options     <- c(x_axis_options, current_name)
            x_axis_definitions <- c(x_axis_definitions, current_def)
        }
    }
    
    # collect the facet options names and definitions
    facet_options <- c()
    facet_definitions <- c()
    for(index in 1:length(variable_config_list)) {
        if(variable_config_list[[index]]$facet_candidate) {
            current_name      <- variable_config_list[[index]]$pretty_name
            current_def       <- variable_config_list[[index]]$definition
            facet_options     <- c(facet_options, current_name)
            facet_definitions <- c(facet_definitions, current_def)
        }
    }
    
    # return all option collections
    list(x_axis_options     = x_axis_options, 
         x_axis_definitions = x_axis_definitions,
         facet_options      = facet_options,
         facet_definitions  = facet_definitions)
}

fixed_ui_options <<- get_fixed_ui_options(variable_configuration)

# create a simple collection of raw = pretty name pairs (to make it easy to
# associate the pairs)
raw_pretty_pairs <<- do.call(rbind, variable_configuration)
raw_pretty_pairs <<- as.data.frame(raw_pretty_pairs)$pretty_name

###############################################################################
## SHINY UI LOOP

shinyUI(navbarPage(
    # title of the MOS application instance
    MOS_instance_name,
    
    # set custom bootstrap.css if desired/available
    theme = custom_css,
    
    # using COS to explore trends per predictor ("Explore Mode")
    tabPanel("Explore Mode", fluidPage(
        # define user tools in the first column
        # width = 3 of 12 (Shiny divides the horizontal space up into 12 sections)
        column(3, 
               wellPanel(               
                   radioButtons("x_axis_choice", label = h4("Select X-Axis"), 
                                choices = fixed_ui_options$x_axis_options),
                   
                   radioButtons("facet_choice", 
                                label = h4("Compare By..."),
                                choices = c("None", 
                                            fixed_ui_options$facet_options))
               ),
               
               wellPanel( 
                   helpText(h4("Advanced Options")), 
                   
                   checkboxInput("slider_show", 
                                 label = "Show?",
                                 FALSE),
                   
                   conditionalPanel(
                       # only show if the "Advanced Options" box is ticked
                       condition = "input.slider_show == true",
                       
                       actionButton("update_explore_data",
                                    "Update Plot"),
                       br(),
                       br(),
                       
                       uiOutput("explore_input_set")
                   )
               )
        ),
        
        # define the visualization in the second column
        # width = 9 of 12
        column(9, 
               plotOutput("ribbon_plot"),
               
               wellPanel(
                   uiOutput("ribbon_text"),
                   HTML(ribbon_addendum)
               )
        )
    )),
    
    # using COS to simulate outcomes for fixed predictor values ("Explore Mode")
    tabPanel("Single Case Mode", fluidPage(
        # define user tools in the first column
        column(3, 
               wellPanel(
                   helpText(h4("Case Values")),
                   
                   actionButton("update_sc_data",
                                "Simulate"),
                   br(),
                   br(),
                   
                   uiOutput("sc_input_set")
               )
        ),
        
        # define the visualization in the second column
        column(9,
               plotOutput("dot_cloud_plot"),
               
               wellPanel(
                   HTML(dot_cloud_addendum)
               )
        )
    ))
))

###############################################################################
## END OF SCRIPT
###############################################################################