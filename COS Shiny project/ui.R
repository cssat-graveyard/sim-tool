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
        pretty_name         = "Engagement: Parent Trusts Case Worker",
        definition          = paste0("Parental belief that the agency or ", 
                                     "worker is sincere, honest, or ",
                                     "well-intentioned, with intent to help ",
                                     "the client."),
        ribbon_plot_summary = paste0("There is a positive association between ",
                                     "this index of parental trust and ",
                                     "Reunification: the likelihood that ",
                                     "simulated cases end in Reunification ",
                                     "increases as the trust index increases.",
                                     "<br><br>The likelihood of both Adoption ",
                                     "and Guardianship declines as the trust ",
                                     "index increases. The likelihood of ",
                                     "Emancipation (very unlikely) remains ",
                                     "stable at all index levels."),
        annotation          = c("<- less trust", "more trust ->"),
        annotation1         = c("very low", "low", "moderate", "high", "very high"),
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) abs(x - 3),
        transform_for_model = function(x) -(x) + 3
    ),    
    wrkg_scores = list(
        pretty_name         = paste0("Engagement: Working Relationship ",
                                     "Between Parent and Case Worker"),
        definition          = paste0("Parental perception of the ",
                                     "interpersonal relationship with worker ",
                                     "characterized by a sense of reciprocity ",
                                     "or mutuality and good communication."),
        ribbon_plot_summary = paste0("There is a positive, but weak, ",
                                     "association between this index of the ",
                                     "parent--agent relationship and ",
                                     "Reunification: the likelihood that ",
                                     "simulated cases end in Reunification ",
                                     "slightly increases as the relationshp ",
                                     "index increases.<br><br>The likelihood ",
                                     "of Adoption declines as the ",
                                     "relationship index increases. The ",
                                     "likelihood of Guardianship (fairly ",
                                     "unlikely) and Emancipation (very ",
                                     "unlikely) remain stable all index ",
                                     "levels."),
        annotation          = c("<- worse relationship", "better relationship ->"),
        x_axis_candidate    = TRUE,    
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),   
    recep_scores = list(
        pretty_name         = "Engagement: Parent Receptivity",
        definition          = paste0("Parental openness to receiving help, ",
                                     "characterized by recognition of ", 
                                     "problems or circumstances that resulted ",
                                     "in agency intervention and by a ",
                                     "perceived need for help."),
        ribbon_plot_summary = paste0("The association between this index of ",
                                     "parent receptivity and the case ",
                                     "outcomes is very weak. In other words, ",
                                     "this index - at least by itself - is ",
                                     "has little effect on the likelihood of ",
                                     "simulated case outcomes."),
        annotation          = c("<- less receptivity", "more receptivity ->"),
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),    
    buyn_scores = list(
        pretty_name         = "Engagement: Parent Buy-In",
        definition          = paste0("Parental perception of benefit; a sense ",
                                     "of being helped or the expectation of ",
                                     "receiving help through the agency ",
                                     "involvement; a feeling that things are ",
                                     "changing (or will change) for the ",
                                     "better. Also includes a commitment to ",
                                     "the helping process, characterized by ",
                                     "active participation in planning or ",
                                     "services, goal ownership, and ",
                                     "initiative in seeking and using help."),
        ribbon_plot_summary = paste0("There is a positive association between ",
                                     "this index of parental ",
                                     "commitment/participation and ",
                                     "Reunification: the likelihood that ",
                                     "simulated cases end in Reunification ",
                                     "increases as the buy-in index increases.",
                                     "<br><br>The likelihood of Guardianship ",
                                     "decreases as the buy-in index ",
                                     "increases.The likelihood of Adoption ",
                                     "(moderately likely) and Emancipation ",
                                     "(very unlikely) remain stable."),
        annotation          = c("<- less buy-in", "more buy-in ->"),
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) x + 3,
        transform_for_model = function(x) x - 3
    ),    
    log_age_eps_begin = list(
        pretty_name         = "Child Age at Episode Begin",
        definition          = paste0("The age of the child (in years) as of ",
                                     "the start of their placement in ",
                                     "out-of-home care."),
        ribbon_plot_summary = paste0("There is a high likelihood that ",
                                     "simulated cases end in Reunification if ",
                                     "the case starts when the child about 2 ",
                                     "to 12 years of age.<br><br>Prior to the ",
                                     "second year, Adoption is also fairly ",
                                     "likely - but it declines steeply from 0 ",
                                     "to 5 years and then stabilizes until ",
                                     "about 10 to 12 years.<br><br>The ",
                                     "likelihood that simulated cases end in ",
                                     "Guardianship slowly increases ",
                                     "(complimenting the decline in Adoption) ",
                                     "until about 12 years.<br><br>At 10 to ",
                                     "12 years, Reunification, Adoption, and ",
                                     "Guardianship become rapidly less likely ",
                                     "as child age increases. Instead, ",
                                     "Emancipation becomes increasingly ",
                                     "likely. By 13 to 15 years of age, it is ",
                                     "the most likely outcome for simulated ",
                                     "cases."),
        annotation          = NULL,
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = function(x) exp(x) - 1,
        transform_for_model = log1p
    ),  
    housing_hs_cnt = list(
        pretty_name         = "Count of Housing Hardships",
        definition          = paste0("The count of affirmative responses to ",
                                     "survey questions concerning housing ",
                                     "hardships (e.g. difficulty paying rent, ",
                                     "couch-surfing, etc.)."),
        ribbon_plot_summary = paste0("There is a strong negative assocation ",
                                     "between this index of housing hardships ",
                                     "and Reunification: the likelihood that ",
                                     "simulated cases end in Reunification ",
                                     "decreases as the housing hardship index ",
                                     "increases.<br><br>The likelihood of ",
                                     "Adoption increases as the housing ",
                                     "hardship index increases. Guardianship ",
                                     "(unlikely) and Emancipation (very ",
                                     "unlikely) remain stable at all index ",
                                     "levels."),
        annotation          = NULL,
        x_axis_candidate    = TRUE,
        slider_candidate    = TRUE,
        slider_rounding     = 1,
        facet_candidate     = FALSE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),   
    REG = list(
        pretty_name         = "Administrative Region",
        definition          = paste0("An indicator of the administrative ",
                                     "region of the child welfare case."),
        ribbon_plot_summary = paste0(""),
        annotation          = NULL,
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),
    employ = list(
        pretty_name         = "Parental Employment Status",
        definition          = paste0("An indicator as to whether or not the ",
                                     "parent reported full or part-time ",
                                     "employment."),
        ribbon_plot_summary = paste0(""),
        annotation          = NULL,
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),
    sm_coll = list(
        pretty_name         = "Parental Education Level",
        definition          = paste0("An indicator as to whether or not the ",
                                     "parent reported any education beyond ",
                                     "high-school."),
        ribbon_plot_summary = paste0(""),
        annotation          = NULL,
        x_axis_candidate    = FALSE,
        slider_candidate    = FALSE,
        slider_rounding     = NA,
        facet_candidate     = TRUE,
        transform_for_ui    = identity,
        transform_for_model = identity
    ),
    high_in = list(
        pretty_name         = "Parental Income Status",
        definition          = paste0("An indicator as to whether or not the ",
                                     "reported parental income is less than ",
                                     "(or equal to) 10,000 dollars."),
        ribbon_plot_summary = paste0(""),
        annotation          = NULL,
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
                   
#                    bsPopover("x_axis_choice",
#                              title = "Variable Definitions",
#                              content = paste(
#                                  "<strong>",
#                                  fixed_ui_options$x_axis_options,
#                                  "</strong>",
#                                  "<br>",
#                                  fixed_ui_options$x_axis_definitions,
#                                  "<br><br>"),
#                              trigger = "hover",
#                              placement = "right",
#                              options = list(container = "body")),
                   
                   radioButtons("facet_choice", 
                                label = h4("Compare By..."),
                                choices = c("None", 
                                            fixed_ui_options$facet_options))
                   
#                    bsPopover("facet_choice", 
#                              title = "Variable Definitions",
#                              content = paste(
#                                  "<strong>",
#                                  fixed_ui_options$facet_options,
#                                  "</strong>",
#                                  "<br>",
#                                  fixed_ui_options$facet_definitions,
#                                  "<br><br>"),
#                              trigger = "hover",
#                              placement = "right",
#                              options = list(container = "body"))
                   
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
                   br(),
                   strong("Please Keep In Mind"),
                   p("Our simulation cannot tell if the observed ",
                     "relationships are causal or correlational."),
                   strong("What Is This Simulation Based On?"),
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
                   
                   uiOutput("sc_input_set")
               )
        ),
        
        # define the visualization in the second column
        column(9,
               #                plotOutput("error_bar_plot"),
               
               plotOutput("dot_cloud_plot"),
               
               wellPanel(
                   strong("What Does This Graph Show Us?"),
                   p("Each time the 'SIMULATE' button is clicked, 1000 child ",
                     "welfare cases are simulated for the values you set the ",
                     "sliders to."),
                   p("For each case, we get an estimate of how likely the ",
                     "four outcomes are. We plot every estimate by its ",
                     "outcome."),
                   p("So, there are 1000 points by each outcome - one for ",
                     "every simulated case."),
                   p("The final graph gives us a sense of which outcomes tend ",
                     "to be more likely and how much uncertainty there is in ",
                     "the simulation."),
                   p("The simulation is modeled on real data collected from ",
                     "a limited selection of Washington State welfare data."),
                   strong("What Is This Simulation Based On?"),
                   p("The simulation is modeled on real data collected from ",
                     "a limited selection of Washington State welfare data.")
               )
        )
    ))
))

###############################################################################
## END OF SCRIPT
###############################################################################