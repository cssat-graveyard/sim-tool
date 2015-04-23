# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 4/21/2015

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

# the following features must be specified for every model variable that will
# be visible to the user (as an outcome, slider, or facet)
# variable_configuration <<- list(
#     RAW_NAME = list(
#         pretty_name         = UI_FRIENDLY_NAME,
#         x_axis_candiate     = TRUE OR FALSE (WHEN APPROPRIATE, MAKE ALLOW
#                               VARIABLE TO BE PREPARED FOR AND PRESENTED ON
#                               PLOT X-AXIS),
#         slider_candidate    = TRUE OR FALSE (WHEN APPROPRIATE, MAKE A SLIDER)
#         slider_rounding     = NUMBER (E.G., 1 WILL FORCE THE SLIDER TO SNAP TO
#                               WHOLE NUMBERS AS IT IS BEING MOVED); DEFAULTS TO
#                               0.1 IF NA,
#         facet_candidate     = TRUE OR FALSE (WHEN APPROPRIATE, ALLOW FACET - 
#                               WILL FORCE VARIABLE TO FACTOR),
#         transform_for_ui    = USE "identity" AS DEFAULT; BUT IF VARIABLE NEEDS
#                               TO BE TRANSFORMED FOR USER PRESENTATION, DEFINE
#                               THE TRANSFORM FUNCTION HERE,
#         transform_for_model = USE "identity" AS DEFAULT; SPECIFY FUNCTION
#                               IF UI INPUT NEEDS TO BE TRANSFORMED BACK BEFORE
#                               USED IN THE MODEL,
#     ),
#     ...
# )
variable_configuration <<- list(   
    mist_scores = list(
        pretty_name         = "Parent Mistrust",
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = NA,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),    
    wrkg_scores = list(
        pretty_name         = "Parent/SW Relationship",
        x_axis_candidate    = TRUE,    
        slider_candidate    = TRUE,
        slider_rounding     = NA,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),   
    recep_scores = list(
        pretty_name         = "Parent Receptivity",
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = NA,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),    
    buyn_scores = list(
        pretty_name         = "Parent Buy-in",
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = NA,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),    
    log_age_eps_begin = list(
        pretty_name         = "Age at Removal",
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) exp(x) - 1,
        transform_for_model = log1p
    ),  
    housing_hs_cnt = list(
        pretty_name         = "Count of Housing Hardships",
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),   
    REG = list(
        pretty_name         = "Region",
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),
    employ = list(
        pretty_name         = "Employment Status",
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),
    sm_coll = list(
        pretty_name         = "Educational Level",
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),
    high_in = list(
        pretty_name         = "Income",
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    )
)

# provide the POC colors for use in plots, UI, etc.
poc_colors    <<- c("#3B6E8F", "#A2B69A", "#A3DCE6", "#A784B4")
portal_colors <<- c("#D9BB32", "#6DB33F", "#6E9CAE", "#B1662B", "#5B8067", 
                    "#444D3E", "#994D3E", "#10475B", "#7D6E86", "#D47079", 
                    "#262F1D", "#B0B0B0")

###############################################################################
## CONFIGURATION PROCESSING (AUTO-GENERATE ADDITIONAL USEFUL FEATURES)

# extract the fixed ui options (the levels for any ui features that are
# generated statically - such as the x-axis choices - rather than dynamically -
# such as the sliders)
get_fixed_ui_options <- function(variable_config_list) {
    # collect the x-axis options names
    x_axis_options <- c()
    for(index in 1:length(variable_config_list)) {
        if(variable_config_list[[index]]$x_axis_candidate) {
            current_name <- variable_config_list[[index]]$pretty_name
            x_axis_options <- c(x_axis_options, current_name)
        }
    }
    
    # collect the facet options names
    facet_options <- c()
    for(index in 1:length(variable_config_list)) {
        if(variable_config_list[[index]]$facet_candidate) {
            current_name <- variable_config_list[[index]]$pretty_name
            facet_options <- c(facet_options, current_name)
        }
    }
    
    # return all option collections
    list(x_axis_options = x_axis_options, 
         facet_options  = facet_options)
}

fixed_ui_options <<- get_fixed_ui_options(variable_configuration)

# create a simple collection of raw = pretty name pairs (to make it easy to
# associate the pairs)
raw_pretty_pairs <<- do.call(rbind, variable_configuration)
raw_pretty_pairs <<- as.data.frame(raw_pretty_pairs)$pretty_name

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
                                choices = fixed_ui_options$x_axis_options),
                   
                   radioButtons("facet_choice", 
                                label = h3("Facet Choice"),
                                choices = c("None", 
                                            fixed_ui_options$facet_options))
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
#                plotOutput("error_bar_plot"),
               
               plotOutput("dot_cloud_plot")
        )
    ))
))

###############################################################################
## END OF SCRIPT
###############################################################################