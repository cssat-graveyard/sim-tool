# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 4/20/2015

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
load("data_model.RData")

# explicitly choose the data object we will be working with
# NOTE: incomplete cases will be dropped to avoid modeling/plotting issues
base_data <<- data[which(complete.cases(data)), ]

# specify the formula for the base data object
base_formula <<- outcome ~ mist_scores + wrkg_scores + recep_scores + buyn_scores + 
    log_age_eps_begin + non_min + male + log_par_age + married + 
    hhnum_c + rel_plc + log_eps_rank + housing_hs_cnt + high_in + 
    sm_coll + employ + REG + male * log_par_age + mist_scores * wrkg_scores

###############################################################################
## PREPARE BASE DATA FOR SIMULATION/VISUALIZATION (EXPANSION, STATIC FEATURES)

# use base data to expand variable_configuration to have all values needed to 
# define sliders
variable_configuration <<- add_slider_features(variable_configuration,
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
    output$explore_slider_set <- renderUI({
        # index which variables are slider candidates
        slider_index <- unlist(lapply(variable_configuration, 
                                      function(x) x$slider_candidate == TRUE))
        
        # set the selected x-axis variable to FALSE in the index
        slider_index[x_axis_raw_name()] <- FALSE
        
        # subset variable_configuration to just get the slider candidates
        # (excluding the x-axis variabe)
        selected_sliders <- variable_configuration[slider_index]
        
        # generate the sliders
        lapply(1:length(selected_sliders), function(i) {
            sliderInput(inputId = paste0("explore_",
                                         names(selected_sliders)[i]), 
                        label = selected_sliders[[i]]$pretty_name,
                        min = selected_sliders[[i]]$ui_min, 
                        max = selected_sliders[[i]]$ui_max,
                        value = selected_sliders[[i]]$ui_median,
                        step = ifelse(is.na(selected_sliders[[i]]$slider_rounding),
                                      0.01,
                                      selected_sliders[[i]]$slider_rounding)
            )
        })
    })
    
    # generate the sliders for the "Single Case Mode"
    output$sc_slider_set <- renderUI({
        # index which variables are slider candidates
        slider_index <- unlist(lapply(variable_configuration, 
                                      function(x) x$slider_candidate == TRUE))
        
        # subset variable_configuration to just get the slider candidates
        selected_sliders <- variable_configuration[slider_index]
        
        # generate the sliders
        lapply(1:length(selected_sliders), function(i) {
            sliderInput(inputId = paste0("sc_",
                                         names(selected_sliders)[i]), 
                        label = selected_sliders[[i]]$pretty_name,
                        min = selected_sliders[[i]]$ui_min, 
                        max = selected_sliders[[i]]$ui_max,
                        value = selected_sliders[[i]]$ui_median,
                        step = ifelse(is.na(selected_sliders[[i]]$slider_rounding),
                                      0.01,
                                      selected_sliders[[i]]$slider_rounding)
            )
        })
    })
    
    # generate representative data to feed coefficients
    base_new_data <- reactive({
        get_new_data(exp_data,
                     base_data,
                     exp_model, 
                     x_axis_raw_name(), 
                     facet_selected = facet_raw_name())
    })
    
    # generate counterfactual data for the "Explore Mode" visualizations
    explore_new_data <- reactive({
        # get the current representative data from base_new_data()
        # NOTE: this also establishes the first reactive link - this function
        #       will re-run if the base_new_data object updates
        new_data_updated <- base_new_data()
        
        # establish the reactive link to the "Update" button
        input$update_explore_data
        
        # this next section only gets evaluated IF the "Show sliders?" option is
        # set to true - in that case, the slider values should be used to 
        # update the data
        # NOTE: the "Update Plot" button will only be visible if the "Show
        #       sliders?" option is set to true, so we are restricting our
        #       update pathways to just TWO possibilities:
        #       1. a new x-axis is selected (refreshing all the data/sliders)
        #       2. the "Update Plot" button is pressed, updating the data with
        #          slider values
        if(isolate(input$slider_show)) {
            # index which variables are slider candidates
            slider_index <- unlist(lapply(variable_configuration, 
                                          function(x) x$slider_candidate == TRUE))
            
            # set the selected x-axis variable to FALSE in the index
            slider_index[x_axis_raw_name()] <- FALSE
            
            # subset variable_configuration to just get the slider candidates
            # (excluding the x-axis variabe)
            selected_sliders <- variable_configuration[slider_index]
            
            # update the values based on current slider inputs
            for(i in 1:length(selected_sliders)) {
                # grab the key details
                current_var <- names(selected_sliders)[[i]]
                slider_name <- paste0("explore_", current_var)
                slider_value <- isolate(input[[slider_name]])
                
                # all the input variables initialize as "NULL" - we want to avoid
                # working with them until they've been assigned a value
                # NOTE: input[[current_var]] IS a reactive link (actually a flexible 
                #       set of reactive links) - it links to the sliders dynamically 
                #       generated by the output$slider_set observer
                if(!is.null(isolate(input[[slider_name]]))) {
                    # apply the relevant transformation to convert slider values
                    # to model-appropriate values
                    model_value <- selected_sliders[[current_var]]$transform_for_model(slider_value)
                    
                    # update the matching new_data_updated column with the 
                    # current slider value
                    new_data_updated[current_var] <- model_value
                    
                    # now test to see if the slider variable is part of any
                    # interactions
                    interaction_index <- grepl(paste(paste0(".", current_var), 
                                                     paste0(current_var, "."), 
                                                     sep = "|"),
                                               names(new_data_updated))
                    if(any(interaction_index)) {
                        # if we find interactions, we pull those terms out
                        interaction_vars <- names(new_data_updated)[interaction_index]
                        # create a list with the items in each term split
                        interaction_list <- strsplit(interaction_vars, ".", fixed = T)
                        # update the interaction variables by multiplying their
                        # source columns together
                        for(current_set in 1:length(interaction_list)) {
                            matching_cols <- new_data_updated[interaction_list[[current_set]]]
                            updated_col <- apply(matching_cols, 1, prod)
                            new_data_updated[interaction_vars[[current_set]]] <- updated_col
                        }
                    }
                }
            }
        }
        
        return(new_data_updated)
    })
    
    # generate counterfactual data for the "Single Case" visualizations
    sc_new_data <- reactive({
        # get the current representative data from base_new_data()
        new_data_updated <- isolate(base_new_data())
        
        # establish the reactive link to the "Update" button
        input$update_sc_data
        
        # index which variables are slider candidates
        slider_index <- unlist(lapply(variable_configuration, 
                                      function(x) x$slider_candidate == TRUE))
        
        # subset variable_configuration to just get the slider candidates
        # (excluding the x-axis variabe)
        selected_sliders <- variable_configuration[slider_index]
        
        # update the values based on current slider inputs
        for(i in 1:length(selected_sliders)) {
            # grab the key details
            current_var <- names(selected_sliders)[[i]]
            slider_name <- paste0("sc_", current_var)
            slider_value <- isolate(input[[slider_name]])
            
            # all the input variables initialize as "NULL" - we want to avoid
            # working with them until they've been assigned a value
            # NOTE: input[[current_var]] IS a reactive link (actually a flexible 
            #       set of reactive links) - it links to the sliders dynamically 
            #       generated by the output$slider_set observer
            if(!is.null(isolate(input[[slider_name]]))) {
                # apply the relevant transformation to convert slider values
                # to model-appropriate values
                model_value <- selected_sliders[[current_var]]$transform_for_model(slider_value)
                
                # update the matching new_data_updated column with the 
                # current slider value
                new_data_updated[current_var] <- model_value
                
                # now test to see if the slider variable is part of any
                # interactions
                interaction_index <- grepl(paste(paste0(".", current_var), 
                                                 paste0(current_var, "."), 
                                                 sep = "|"),
                                           names(new_data_updated))
                if(any(interaction_index)) {
                    # if we find interactions, we pull those terms out
                    interaction_vars <- names(new_data_updated)[interaction_index]
                    # create a list with the items in each term split
                    interaction_list <- strsplit(interaction_vars, ".", fixed = T)
                    # update the interaction variables by multiplying their
                    # source columns together
                    for(current_set in 1:length(interaction_list)) {
                        matching_cols <- new_data_updated[interaction_list[[current_set]]]
                        updated_col <- apply(matching_cols, 1, prod)
                        new_data_updated[interaction_vars[[current_set]]] <- updated_col
                    }
                }
            }
        }
        
        
        return(new_data_updated)
    })
    
    # feed the representative data to the sampled coefficients to generate
    # our final simulated outcome likelihoods
    explore_likelihoods <- reactive({
        likelihoods_raw <- mlogitsimev_med(explore_new_data(), 
                                           coeff_estimates, 
                                           ci = c(0.95, 0.50))
        # format the simulated outcome likelihoods for visualization
        ribbon_ready <- format_for_visualization(likelihoods_raw,
                                                 exp_model,
                                                 base_data,
                                                 isolate(explore_new_data()),
                                                 isolate(x_axis_raw_name()),
                                                 facet_selected = isolate(facet_raw_name()))
        
        return(ribbon_ready)
    })
    
    # single case: feed the representative data to the sampled coefficients to generate
    # our final simulated outcome likelihoods
    sc_likelihoods <- reactive({
        likelihoods_raw <- mlogitsimev_med(sc_new_data(), 
                                           coeff_estimates, 
                                           ci = c(0.95, 0.50))
        # format the simulated outcome likelihoods for visualization
        errorbar_ready <- format_for_visualization(likelihoods_raw,
                                                   exp_model,
                                                   base_data,
                                                   isolate(explore_new_data()),
                                                   isolate(x_axis_raw_name()),
                                                   facet_selected = isolate(facet_raw_name()))
        
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
        
        # gather into a single object
        collection <- list("eb" = errorbar_ready, 
                           "dp" = dotplot_ready)
        
        return(collection)
    })
    
    # visualize the outcome likelihoods
    output$ribbon_plot <- renderPlot({
        # update the "predictor" in explore_likelihoods to make it ui friendly
        x_axis_var <- isolate(x_axis_raw_name())
        ui_transform <- variable_configuration[[x_axis_var]]$transform_for_ui
        
        ribbon_likelihoods <- explore_likelihoods()
        ribbon_likelihoods$predictor <- ui_transform(ribbon_likelihoods$predictor)
        
        # draw the plot
        get_ribbon_plot(ribbon_likelihoods, 
                        facet_selected = isolate(facet_raw_name()),
                        y_lab = "Probability", 
                        x_lab = isolate(input$x_axis_choice),
                        custom_colors = portal_colors
        )
    })
    
    output$error_bar_plot <- renderPlot({
        if(input$update_sc_data > 0) {
            get_error_bar_plot(sc_likelihoods()$eb,
                               y_lab = "Probability",
                               x_lab = "Outcomes",
                               custom_colors = portal_colors)
        }
    })
    
    output$dot_cloud_plot <- renderPlot({
        if(input$update_sc_data > 0) {
            get_dot_cloud_plot(sc_likelihoods()$dp,
                               y_lab = "Probability",
                               x_lab = "Outcomes",
                               custom_colors = portal_colors)
        }
    })
})



###############################################################################
## END OF SCRIPT
###############################################################################