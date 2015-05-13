# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 5/12/2015

###############################################################################
## SCRIPT OVERVIEW

# goal: This is a core Shiny project file. It defines the creation of the
#       data objects that the user interface will display and allow interaction
#       with.
#
#       Supporting libraries, data imports, etc., all happen here. For the 
#       SimTool prototype, the majority of the work is handled by custom 
#       functions (loaded, along with supporting libraries, by R scripts 
#       named below).

# sketch of script
# - source the scripts providing the functions to create the visualization
#   from the model and data

# - user specification required: source and select base data object, define
#   the base data model
#
# - build any static objects needed for simulation/visualization
#   - we need to "spread" the factors and interactions in a second data frame  
#     so they are handled properly; this also results in an expanded model
#     formula matching the "spread" data frame
#   - the first data frame is retained for factor/level information, the second
#     is the data frame we will use for the model fitting and simulation
#   - create all simulation objects except the new data, the predictions
#     summary object, and the visualization
#
# - define the shinyServer loop to turn the data objects into Shiny's output
#   objects (and define any desired interactivty)
#   - collect user inputs
#   - generate the new data based on user input
#   - feed the new data to the simulated coefficients (get prediction object)
#   - generate the visualization based on user input and the prediction object

###############################################################################
## SOURCE THE SUPPORTING SIMULATION/VISUALIZATION SCRIPTS

source("COS sim and vis functions.R")
source("COS custom mlogitsimev.R")

###############################################################################
## USER DEFINED INPUTS

# source the base data and base model
load("data_model_V4.RData")

# explicitly choose the data object we will be working with
# NOTE: incomplete cases will be dropped to avoid modeling/plotting issues
base_data <<- data[which(complete.cases(data)), ]

# ensure the levels in the outcome variable are in RAGE order
base_data$outcome <- factor(base_data$outcome, c("Reunification", 
                                                 "Adoption",
                                                 "Guardianship", 
                                                 "Emancipation"))

# specify the formula for the base data object
base_formula <<- outcome ~ 
    # additive terms
    mist_scores + wrkg_scores + recep_scores + buyn_scores + log_age_eps_begin + 
    non_min + male + log_par_age + married + hhnum_c + rel_plc + log_eps_rank + 
    housing_hs_cnt + high_in + sm_coll + employ + REG + 
    # interaction terms
    high_in : housing_hs_cnt + 
    housing_hs_cnt : employ


###############################################################################
## PREPARE BASE DATA FOR SIMULATION/VISUALIZATION (EXPANSION, STATIC FEATURES)

# TESTING SETTINGS TO INCREASE STABILITY ## TEMP ##
options(warn = -1)

# use base data to expand variable_configuration to have all values needed to 
# define sliders
variable_configuration <<- add_input_features(variable_configuration,
                                              base_data)

# snag the outcome variable from the formula (simplifies later calls)
outcome_variable <<- as.character(base_formula[[2]])

# expand the factors in the data object, re-add the outcome, drop the intercept
exp_data <<- model.matrix(base_formula, base_data)
exp_data <<- data.frame(base_data[outcome_variable],  
                        exp_data)
exp_data[, "X.Intercept."] <<- NULL

# fit the model to the expanded dataset (using the expanded model)
# multinom: "fit a multinomial log-linear model via neural networks"
exp_model <<- multinom(formula(exp_data), data = exp_data, Hess = T)

# extract the interaction column names from the expanded data
interaction_cols <<- get_interaction_col_names(base_formula, exp_data)

# get the static simulation/visualization features
point_estimates <<- get_point_estimates(exp_model)
cov_matrix <<- get_covariance_matrix(exp_model)
coeff_estimates <<- get_coefficient_estimates(1000, point_estimates, 
                                              cov_matrix, exp_model)

###############################################################################
## DEFINE THE OBJECTS FOR THE UI TO DISPLAY

shinyServer(function(input, output, session) {
    # create fixed user inputs
    # all are reactive objects so that updates to these can be relied on to
    # update any dependencies
    x_axis_raw_name <- reactive({
        # explore raw_pretty_pairs to find the correct raw name
        index <- match(input$x_axis_choice, raw_pretty_pairs)
        raw_name <- names(raw_pretty_pairs)[index]
        return(raw_name)
    })
    
    facet_raw_name <- reactive({
        # filter the variable_configuration dataframe for the row with the
        # matching UI name, extract the column name
        # UNLESS special variable "None" is selected, in which case return NULL
        if(input$facet_choice == "None") {
            return(NULL)
        } else {
            # explore raw_pretty_pairs to find the correct raw name
            index <- match(input$facet_choice, raw_pretty_pairs)
            raw_name <- names(raw_pretty_pairs)[index]
            return(raw_name)
        }
    })
    
    # generate the sliders for the "Explore Mode"
    output$explore_input_set <- renderUI({
        make_inputs(variable_config_list = variable_configuration,
                    variables_to_drop = x_axis_raw_name(),
                    append_name = "explore")
    })
    
    # generate the sliders for the "Single Case Mode"
    output$sc_input_set <- renderUI({
        make_inputs(variable_config_list = variable_configuration,
                    variables_to_drop = NA,
                    append_name = "sc",
                    facet_as_dropdown = TRUE)
    })
    
    # generate representative data to feed coefficients
    base_new_data <- reactive({
        get_new_data(exp_data,
                     base_data,
                     exp_model, 
                     x_axis_raw_name(), 
                     facet_selected = facet_raw_name(),
                     interaction_col_names = interaction_cols)
    })
    
    # generate counterfactual data for the "Explore Mode" visualizations
    explore_new_data <- reactive({
        # establish the reactive link to the "Update" button
        input$update_explore_data
        
        # this next section only gets evaluated if the "Show sliders?" option is
        # set to TRUE - in that case, the slider values should be used to 
        # create an updated data object
        # NOTE: the "Update Plot" button will only be visible if the "Show
        #       sliders?" option is set to true, so we are restricting our
        #       update pathways to just TWO possibilities:
        #       1. the "Update Plot" button is visible and is pressed, updating 
        #          the data with slider values
        #       2. a new x-axis is selected (refreshes the plots that use
        #          explore_new_data() and resets the sliders if they are 
        #          visible)
        if(isolate(input$slider_show)) {
            # note that the update_target here is allowed to be reactive
            # to create a reactive link when the sliders are visible
            apply_input_values(update_target = base_new_data(), 
                               interaction_col_names = interaction_cols,
                               variable_config_list = variable_configuration,
                               input_call = isolate(input),               
                               append_name = "explore",
                               base_data = base_data,
                               use_slider_values = TRUE,
                               use_dropdown_values = FALSE,
                               variables_to_drop = isolate(x_axis_raw_name()))
        } else {
            # reactive link for when the sliders are hidden
            return(base_new_data())
        }
    })
    
    # generate counterfactual data for the "Single Case" visualizations
    sc_new_data <- reactive({        
        # establish the reactive link to the "Update" button
        #input$update_sc_data
        input$update_sc_data
        
        # make the sliders
        # NOTE: there is only a single reactive pathway here - the "update_sc_
        #       data" input must be triggered - this insures that the
        #       visualization chain is only triggered on user request        
        apply_input_values(update_target = isolate(base_new_data()), 
                           interaction_col_names = interaction_cols,
                           variable_config_list = variable_configuration,
                           input_call = isolate(input),               
                           append_name = "sc",
                           base_data = base_data,
                           use_slider_values = TRUE,
                           use_dropdown_values = TRUE,
                           variables_to_drop = NA)
    })
    
    # feed the representative data to the sampled coefficients to generate
    # our final simulated outcome likelihoods
    explore_likelihoods <- reactive({
        # get the unformatted summary likelihoods
        likelihoods_raw <- mlogitsimev_med(explore_new_data(), 
                                           coeff_estimates, 
                                           ci = c(0.95, 0.50))
        
        # format the summary likelihoods for visualization
        ribbon_ready <- format_for_ribbon_plot(likelihoods_raw,
                                               exp_model,
                                               base_data,
                                               isolate(explore_new_data()),
                                               isolate(x_axis_raw_name()),
                                               facet_selected = isolate(facet_raw_name())
        )
        
        # return the formatted object
        return(ribbon_ready)
    })
    
    # single case: feed the representative data to the sampled coefficients to 
    # generate our final simulated outcome likelihoods
    sc_likelihoods <- reactive({
        # get single point estimates for the dot plot cloud
        likelihoods_cloud <- mlogitsimev_med(sc_new_data(), 
                                             coeff_estimates, 
                                             ci = c(0.95, 0.50),
                                             return_cloud = TRUE)
        
        # format the single point estimates for visualization
        likelihoods_cloud <- data.frame(likelihoods_cloud)
        names(likelihoods_cloud) <- exp_model$lab
        likelihoods_cloud$index <- 1:nrow(likelihoods_cloud)
        dotplot_ready <- gather(likelihoods_cloud, outcome, single_pe, 
                                -index)
        
        # return the collection
        return(dotplot_ready)
    })
    
    # visualize the outcome likelihoods (ribbon plot)
    output$ribbon_plot <- renderPlot({
        # isolate the x_axis_variable name and its associated transform_for_ui
        x_axis_var <- isolate(x_axis_raw_name())
        ui_transform <- variable_configuration[[x_axis_var]]$transform_for_ui
        
        # apply the transform to the predictor column in explore_likelihoods to 
        # make it ui friendly
        ribbon_likelihoods <- explore_likelihoods()
        ribbon_likelihoods$predictor <- ui_transform(ribbon_likelihoods$predictor)
        
        # draw the plot
        get_ribbon_plot(ribbon_likelihoods, 
                        facet_selected = isolate(facet_raw_name()),
                        y_lab = "Simulated Probability", 
                        x_lab = isolate(input$x_axis_choice),
                        custom_colors = portal_colors,
                        isolate(variable_configuration[[x_axis_var]]$annotation),
                        isolate(variable_configuration[[x_axis_var]]$annotation1)
        )
    })
    
    # construct the summary text for constructed ribbon plot
    output$ribbon_text <- renderText({
        build_ribbon_summary(x_axis_raw_name(), variable_configuration)
    })
    
    # visualize the outcome likelihoods (dot cloud plot)
    output$dot_cloud_plot <- renderPlot({
        # draw the plot
        get_dot_cloud_plot(sc_likelihoods(),
                           y_lab = "Simulated Outcome Probability",
                           x_lab = "",
                           custom_colors = portal_colors)
        
    })
})



###############################################################################
## END OF SCRIPT
###############################################################################