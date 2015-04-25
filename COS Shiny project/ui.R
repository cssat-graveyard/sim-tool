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
library(shinyBS)    # expands base Shiny features (e.g., popovers)

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
        pretty_name         = "Engagement: Mistrust",
        definition          = "A parent's belief that the agency or worker is manipulative, malicious, or capricious, with intent to harm the client.",
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),    
    wrkg_scores = list(
        pretty_name         = "Engagement: Working Relationship",
        definition          = "A parent's perception of the interpersonal relationship with worker characterized by a sense of reciprocity or mutuality and good communication.",
        x_axis_candidate    = TRUE,    
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),   
    recep_scores = list(
        pretty_name         = "Engagement: Receptivity",
        definition          = "A parent's openness to receiving help, characterized by recognition of problems or circumstances that resulted in agency intervention and by a perceived need for help",
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),    
    buyn_scores = list(
        pretty_name         = "Engagement: Buy-In",
        definition          = "A parent's perception of benefit; a sense of being helped or the expectation ofreceiving help through the agency's involvement; a feeling that things are changing (or will change) for the better. Also includes a commitment to the helping process, characterized by active participation in planning or services, goal ownership, and initiative in seeking and using help.",
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),    
    log_age_eps_begin = list(
        pretty_name         = "Age at Episode Begin",
        definition          = "The age of the child (in years) as of the start of their placement in out-of-home care.",
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) exp(x) - 1,
        transform_for_model = log1p
    ),  
    housing_hs_cnt = list(
        pretty_name         = "Count of Housing Hardships",
        definition          = "The count of affirmative responses to survey questions concerning housing hardships (e.g. difficulty paying rent, couch-surfing, etc.).",
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),   
    REG = list(
        pretty_name         = "Administrative Region",
        definition          = "An indicator of the administrative region of the parent's child welfare case.",
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),
    employ = list(
        pretty_name         = "Parental Employment Status",
        definition          = "An indicator as to whether or not the parent reported full or part-time employment.",
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),
    sm_coll = list(
        pretty_name         = "Parental Education Level",
        definition          = "An indicator as to whether or not the parent reported any education beyond high-school.",
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),
    high_in = list(
        pretty_name         = "Parental Income Status",
        definition          = "An indicator as to whether or not the parent's reported income is less than (or equal to) 10,000 dollars.",
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
    
    # using COS to explore trends per predictor ("Explore Mode")
    tabPanel("Explore Mode", fluidPage(
        # define user tools in the first column
        # width = 3 of 12 (Shiny divides the horizontal space up into 12 sections)
        column(3, 
               wellPanel(               
                   radioButtons("x_axis_choice", label = h4("Select X-Axis"), 
                                choices = fixed_ui_options$x_axis_options),
                   
                   bsPopover("x_axis_choice",
                             title = "What are these?",
                             content = paste0("<p>Things you should know.",
                                              "Seriously - know them. ",
                                              "Or else.... you know. </p>"
                             ),
                             trigger = "click",
                             placement = "bottom"),
                   
                   radioButtons("facet_choice", 
                                label = h4("Compare By..."),
                                choices = c("None", 
                                            fixed_ui_options$facet_options))
               ),
               
               wellPanel( 
                   # popout text for "Advanced Options" object: "The x-axis and - if selected - facet predictors are visible. All other predictors are set to their mean value. Adjust a slider to explore how changes in that predictor impact the relationship between the x-axis variable and the likelihood of particular outcomes."
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
                   
                   uiOutput("sc_slider_set")
               )
        ),
        
        # define the visualization in the second column
        column(9,
#                plotOutput("error_bar_plot"),
               
               plotOutput("dot_cloud_plot")
        )
    ))
))

###############################################################################
## END OF SCRIPT
###############################################################################