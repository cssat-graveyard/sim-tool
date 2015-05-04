# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 4/29/2015

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
## LOAD SUPPORTING LIBRARIES AND SET ANY DEFAULT OPTIONS
library(dplyr)      # serves various formatting needs
library(shinyBS)    # expands base Shiny features (e.g., popovers)
library(Cairo)      # supports plot quality across devices
library(ggplot2)    # for specifying cos theme

options(shiny.usecairo=T)

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
        definition          = "Parental belief that the agency or worker is manipulative, malicious, or capricious, with intent to harm the client.",
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),    
    wrkg_scores = list(
        pretty_name         = "Engagement: Working Relationship",
        definition          = "Parental perception of the interpersonal relationship with worker characterized by a sense of reciprocity or mutuality and good communication.",
        x_axis_candidate    = TRUE,    
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),   
    recep_scores = list(
        pretty_name         = "Engagement: Receptivity",
        definition          = "Parental openness to receiving help, characterized by recognition of problems or circumstances that resulted in agency intervention and by a perceived need for help",
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),    
    buyn_scores = list(
        pretty_name         = "Engagement: Buy-In",
        definition          = "Parental perception of benefit; a sense of being helped or the expectation of receiving help through the agency involvement; a feeling that things are changing (or will change) for the better. Also includes a commitment to the helping process, characterized by active participation in planning or services, goal ownership, and initiative in seeking and using help.",
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
        definition          = "An indicator of the administrative region of the child welfare case.",
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
        definition          = "An indicator as to whether or not the reported parental income is less than (or equal to) 10,000 dollars.",
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
    # title of entire proejct
    "The Case Outcome Simulator",
    # CSS theme for entire project (current theme from here:
    # https://bootswatch.com/sandstone/)
    theme = "bootstrap.css",
    
    # using COS to explore trends per predictor ("Explore Mode")
    tabPanel("Explore Mode", fluidPage(
        # define user tools in the first column
        # width = 3 of 12 (Shiny divides the horizontal space up into 12 sections)
        column(3, 
               wellPanel(               
                   radioButtons("x_axis_choice", label = h4("Select X-Axis"), 
                                choices = fixed_ui_options$x_axis_options),
                   
                   bsPopover("x_axis_choice",
                             title = "Variable Definitions.",
                             content = paste(
                                 "<strong>",
                                 fixed_ui_options$x_axis_options,
                                 "</strong>",
                                 "<br>",
                                 fixed_ui_options$x_axis_definitions,
                                 "<br><br>"),
                             trigger = "click",
                             placement = "right",
                             options = list(container = "body")),
                   
                   radioButtons("facet_choice", 
                                label = h4("Compare By..."),
                                choices = c("None", 
                                            fixed_ui_options$facet_options)),
                   
                   bsPopover("facet_choice", 
                             title = "Variable Definitions.",
                             content = paste(
                                 "<strong>",
                                 fixed_ui_options$facet_options,
                                 "</strong>",
                                 "<br>",
                                 fixed_ui_options$facet_definitions,
                                 "<br><br>"),
                             trigger = "click",
                             placement = "right",
                             options = list(container = "body"))
                   
               ),
               
               wellPanel( 
                   popify(helpText(h4("Advanced Options")), 
                          title = "What is This?",
                          content = "This section allows you to explore how changes in unselected predictors impact the relationship between the selected x-axis predictor and simulated outcome likelihoods.",
                          trigger = "click",
                          placement = "bottom"
                   ),
                   
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
               plotOutput("ribbon_plot"),
               
               wellPanel(
                   strong("What does this graph show us?"),
                   p("This graph shows the relationship between a selected ",
                     "predictor (X-Axis choice) and the simulated likelihood ",
                     "of RAGE outcomes at different levels of that predictor."),
                   p("The simulation is modeled on real data collected from ",
                     "a limited selection of Washington State welfare data.")
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
                   
                   uiOutput("sc_slider_set")
               )
        ),
        
        # define the visualization in the second column
        column(9,
               #                plotOutput("error_bar_plot"),
               
               plotOutput("dot_cloud_plot"),
               
               wellPanel(
                   strong("What does this graph show us?"),
                   p("Each time the 'SIMULATE' button is clicked, 1000 child ",
                     "welfare cases are simulated for the values you set the ",
                     "sliders to."),
                   p("Then we plot how likely each RAGE outcome is for each ",
                     "case."),
                   p("The final graph gives us a sense of which outcomes tend ",
                     "to be more likely and how much uncertainty there is in ",
                     "the simulation."),
                   p("The simulation is modeled on real data collected from ",
                     "a limited selection of Washington State welfare data.")
               )
        )
    ))
))

###############################################################################
## END OF SCRIPT
###############################################################################