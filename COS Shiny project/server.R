# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 4/14/2015

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
# - source the model and data
#
# - source the scripts providing the functions to create the visualization
#   from the model and data
#
# - build any static objects needed for simulation/visualization
#   - select the source data
#   - define the model formula
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
## SOURCE THE DATA, MODEL, AND SIM/VIS SCRIPTS

load("data_model.RData")
source("COS sim and vis functions.R")
source("COS custom mlogitsimev.R")

###############################################################################
## STATIC OBJECTS FOR SIMULATION/VISUALIZATION

# choose the data object we will be working with and specify the formula
# (with respect to this data object)
base_data <<- data[which(complete.cases(data)), ]
base_formula <<- outcome ~ mist_scores + wrkg_scores + recep_scores + buyn_scores + 
    log_age_eps_begin + non_min + male + log_par_age + married + 
    hhnum_c + rel_plc + log_eps_rank + housing_hs_cnt + high_in + 
    sm_coll + employ + REG + male * log_par_age + mist_scores * wrkg_scores
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
        #browser()
        # filter the variable_configuration dataframe for the row with the
        # matching UI name, extract the column name
        variable_configuration[which(variable_configuration$pretty_name == 
                                         input$x_axis_choice),
                               ]$raw_name
    })
    
    facet_raw_name <- reactive({
        #browser()
        # filter the variable_configuration dataframe for the row with the
        # matching UI name, extract the column name
        # UNLESS special variable "None" is selected, in which case return NULL
        if(input$facet_choice == "None") {
            return(NULL)
        } else {
            variable_configuration[which(variable_configuration$pretty_name == 
                                             input$facet_choice),
                                   ]$raw_name
        }
    })
    
    # generate dynamic UI features for the "Explore Mode"
    slider_set_definition <- reactive({
        #browser()
        define_sliders(x_axis_selected = x_axis_raw_name(), 
                       slider_raw_names = slider_options$raw_name, 
                       slider_pretty_names = slider_options$pretty_name, 
                       base_data = base_data, 
                       auto = FALSE, 
                       outcome_variable = outcome_variable)
    })
    
    output$slider_set <- renderUI({
        #browser()
        # check if pretty names are available - use them if they are or 
        # simply use the raw column names
        if("var_pretty_name" %in% names(slider_set_definition())) {
            label_name <- slider_set_definition()$var_pretty_name
        } else {
            label_name <- toupper(slider_set_definition()$var_raw_name)
        }
        # collect the initial values for the sliders from the new_data_initial object
        start_values <- isolate(new_data_initial())[slider_set_definition()$var_raw_name][1,]
        
        # generate the sliders
        lapply(1:nrow(slider_set_definition()), function(i) {
            sliderInput(inputId = slider_set_definition()$var_raw_name[i], 
                        label = label_name[i],
                        min = slider_set_definition()$var_min[i], 
                        max = slider_set_definition()$var_max[i],
                        value = start_values[[i]],
                        step = 0.0001)
        })
    })
    
    # generate dynamic UI features for the "Single Case Mode"
    sc_slider_set_definition <- reactive({
        #browser()
        define_sliders(x_axis_selected = NA, 
                       slider_raw_names = slider_options$raw_name, 
                       slider_pretty_names = slider_options$pretty_name, 
                       base_data = base_data, 
                       auto = FALSE, 
                       outcome_variable = outcome_variable)
    })
    
    output$sc_slider_set <- renderUI({
        #browser()
        # check if pretty names are available - use them if they are or 
        # simply use the raw column names
        if("var_pretty_name" %in% names(sc_slider_set_definition())) {
            label_name <- sc_slider_set_definition()$var_pretty_name
        } else {
            label_name <- toupper(sc_slider_set_definition()$var_raw_name)
        }
        
        # generate the sliders
        lapply(1:nrow(sc_slider_set_definition()), function(i) {
            sliderInput(inputId = sc_slider_set_definition()$var_raw_name[i], 
                        label = label_name[i],
                        min = sc_slider_set_definition()$var_min[i], 
                        max = sc_slider_set_definition()$var_max[i],
                        # NOTE: this behavior is distinct from the "Explore
                        #       Mode" sliders, which use the new_data_initial
                        #       object to get their start values
                        value = sc_slider_set_definition()$var_median[i],
                        step = 0.0001)
        })
    })
    
    # generate representative data to feed coefficients
    new_data_initial <- reactive({
        #browser()
        get_new_data(exp_data,
                     base_data,
                     exp_model, 
                     x_axis_raw_name(), 
                     facet_selected = facet_raw_name())
    })
    
    # update the new data with inputs from the sliders
    new_data_updated <- reactive({
        #browser()
        # get the current representative data from new_data_initial()
        # NOTE: this also establishes the first reactive link - this function
        #       will re-run if the new_data_initial object updates
        new_data_updated <- new_data_initial()
        # establish the reactive link to the "Update Plot" button
        input$use_slider_values
        
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
            # update the values based on current slider inputs
            for(i in 1:nrow(isolate(slider_set_definition()))) {
                current_var <- isolate(slider_set_definition()$var_raw_name[i])
                # all the input variables initialize as "NULL" - we want to avoid
                # working with them until they've been assigned a value
                # NOTE: input[[current_var]] IS a reactive link (actually a flexible 
                #       set of reactive links) - it links to the sliders dynamically 
                #       generated by the output$slider_set observer
                if(!is.null(isolate(input[[current_var]]))) {
                    # update the matching new_data_updated column with the 
                    # current slider value
                    new_data_updated[current_var] <- isolate(input[[current_var]])
                    
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
    
    # single case: update the new data with inputs from the sliders
    sc_new_data <- reactive({
        #browser()
        # get the current representative data from new_data_initial()
        # NOTE: this also establishes the first reactive link - this function
        #       will re-run if the new_data_initial object updates
        new_data_updated <- new_data_initial()
        # establish the reactive link to the "Get Simulated Outcome" button
        input$update_sc_new_data
        
        # update the values based on current slider inputs
        for(i in 1:nrow(isolate(sc_slider_set_definition()))) {
            current_var <- isolate(sc_slider_set_definition()$var_raw_name[i])
            # all the input variables initialize as "NULL" - we want to avoid
            # working with them until they've been assigned a value
            # NOTE: input[[current_var]] IS a reactive link (actually a flexible 
            #       set of reactive links) - it links to the sliders dynamically 
            #       generated by the output$slider_set observer
            if(!is.null(isolate(input[[current_var]]))) {
                # update the matching new_data_updated column with the 
                # current slider value
                new_data_updated[current_var] <- isolate(input[[current_var]])
                
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
    likelihoods <- reactive({
        #browser()
        likelihoods_raw <- mlogitsimev_med(new_data_updated(), 
                                           coeff_estimates, 
                                           ci = c(0.95, 0.50))
        # format the simulated outcome likelihoods for visualization
        format_for_visualization(likelihoods_raw,
                                 exp_model,
                                 base_data,
                                 isolate(new_data_updated()),
                                 isolate(x_axis_raw_name()),
                                 facet_selected = isolate(facet_raw_name()))
    })
    
    # single case: feed the representative data to the sampled coefficients to generate
    # our final simulated outcome likelihoods
    sc_likelihoods <- reactive({
        #browser()
        likelihoods_raw <- mlogitsimev_med(sc_new_data(), 
                                           coeff_estimates, 
                                           ci = c(0.95, 0.50))
        # format the simulated outcome likelihoods for visualization
        format_for_visualization(likelihoods_raw,
                                 exp_model,
                                 base_data,
                                 isolate(sc_new_data()),
                                 x_axis_selected = NA,
                                 facet_selected = NULL)
    })
    
    # visualize the outcome likelihoods
    output$ribbon_plot <- renderPlot({
        #browser()
        get_ribbon_plot(likelihoods(), 
                        facet_selected = isolate(facet_raw_name()),
                        y_lab = "Probability", 
                        x_lab = isolate(input$x_axis_choice),
                        custom_colors = portal_colors
        )
    })
    
    output$single_case_plot <- renderPlot({
        get_single_case_plot(sc_likelihoods(),
                             y_lab = "Probability",
                             x_lab = "Outcomes",
                             custom_colors = portal_colors)
    })
})



###############################################################################
## END OF SCRIPT
###############################################################################