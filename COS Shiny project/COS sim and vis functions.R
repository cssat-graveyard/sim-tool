# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/25/2015
# Date updated: 4/22/2015

# NOTE: Functions were largely developed in the "gator model example V3.R"
#       script. That script was archived to allow us to seperate the model
#       from the functions working with the model.

###############################################################################
## SCRIPT OVERVIEW

# goal: Provide the suite of functions needed to create the visualization for
#       the Partners for Our Children COS (Case Outcome Simulator) application.
#
#       The functions:
#       - take a given multinomial model, an x-axis selection, and an (optional)
#         facet selection
#       - sample a range of values for each model coefficient with respect to 
#         the uncertainty associated with each coeffecient
#       - generate a range of inputs for the predictor associated with the 
#         x-axis selection (and facet selection, if present) and combine it with 
#         reasonable fixed values for other model coefficients
#       - generate simulated predicted likelihoods for each model outcome by
#         combining the input combinations with the sampled coefficients and
#         summarizing the results (upper quartile, lower quartile, mean pe)
#       - create a plot object visualizing the simulated likelihoods along the
#         given x-axis selection (facetting if appropriate)
#
#       Generating the predicted likelihoods is handled by the simcf package.
#       The code developed here is loosely based on an example provided by
#       that package.
#
#       Where to find the original example code:
#       library(simcf)
#       help(mlogitsimev)
#
#       The original example code was vague, limited and somewhat broken. It has
#       been corrected, extended, and made into a function set (performance 
#       testable) for the COS application.
#
#       The COS application itself is a an R Shiny application that uses the
#       functions in this script to allow users to interact with and visualize
#       a given multinomial logit model.

# sketch of script
# - LOAD SUPPORTING LIBRARIES
#
# - LOAD SOURCE DATASET
#   - tidy, properly formatted and labelled dataset to fit the multinom model to
#
# - FUNCTIONS TO GENERATE OUTCOME PREDICTIONS
#   - create an expanded version of the source dataset (using same model as
#     will be used in the multinom fitting)
#   - fit the multinom model
#   - extract point estimates from the model object
#   - solve the model object Hessian matrix for the covariance matrix
#   - get coefficient estimates via simulation
#   - generate data to feed to coefficient estimates
#   - get the outcome predictions (feed the estimates!)
#
# - FUNCTIONS TO VISUALIZE OUTCOME PREDICTIONS (USER SELECTED X-AXIS/FACETTING)
#   - rearrange the outcome predictions to interface with ggplot (tidy 
#     dataframe)
#   - visualize with ggplot
#   - may be multiple visualizations for development purposes
#
# - ADDITIONAL HELPER FUNCTIONS (TO CONSOLIDATE CERTAIN SHINY SERVER TASKS)
#   - expanding the variable configuration object to complete the values
#     needed to define the sliders
#   - generate a slider set
#   - determine which columns in the expanded data represent interactions

###############################################################################
## LOAD SUPPORTING LIBRARIES
#   - to get simcf (a custom package on github), use the install_github 
#     function from the devtools package:
#     install_github("chrisadolph/tile-simcf", subdir = "simcf")

library(nnet)       # supports interactions with the multinomial logit object
library(MASS)       # allows for multivariate sampling
library(simcf)      # creates counterfactual sets
library(dplyr)      # serves various formatting needs
library(tidyr)      # for reformatting data for visualization
library(ggplot2)    # for visualizing the data
library(combinat)   # for building interaction term permutations

###############################################################################
## FUNCTIONs TO GENERATE OUTCOME PREDICTIONS FOR GIVEN MODEL OBJECT
#   - create an expanded version of the source dataset (using same model as
#     will be used in the multinom fitting)
#   - fit the multinom model
#   - extract point estimates from the model object
#   - solve the model object Hessian matrix for the covariance matrix
#   - get coefficient estimates via simulation
#   - generate data to feed to coefficient estimates
#   - get the outcome predictions (feed the estimates!)

# create an expanded version of the source dataset
# simply use model.matrix(formula, dataset)

# fit the multinom model
# simply use multinom(formula, dataset, Hess = T)

# function to extract non-reference point estimates from the model object
get_point_estimates <- function(model_object) {
    # determine the number of coefficents (intercepts, predictors, interactions) 
    # and outcomes
    number_coefficients <- length(model_object$coefnames)
    number_outcomes <- length(model_object$lab)
    
    # the multinom function returns a lot of 0s - first we find identify where the
    # non-reference weights begin (one set of weights per outcome but we 
    # skip the initial reference set)
    index_starts <- NULL
    for(i in 1:(number_outcomes - 1)) {
        # go the end of the current chunk... add two to skip the placeholder 0...
        index_starts[i] <- i * (number_coefficients + 1) + 2
    }
    
    # then we use the start indices and the the number of coefficients to
    # define the index that will align with all the non-reference weights
    wts_index <- NULL
    for(i in index_starts) {
        wts_index <- c(wts_index, i:(i + number_coefficients - 1))
    }
    
    # finally we return the point estimates (weights) for the coefficients
    return(model_object$wts[wts_index])
}

# function to get the covariance matrix from the Hessian in the model object
get_covariance_matrix <- function(model_object) {
    cov_matrix <- chol2inv(chol(model_object$Hess))
    dimnames(cov_matrix) <- dimnames(model_object$Hess)
    
    return(cov_matrix)
}

# function to get coefficient estimates via simulation
get_coefficient_estimates <- function(sample_size, 
                                      point_estimates, 
                                      covariance_matrix,
                                      model_object) {
    # draw parameters, using MASS::mvrnorm
    sim_betas <- mvrnorm(sample_size, point_estimates, covariance_matrix)
    
    # data needs to be re-arranged into an array format
    # first determine array dimensions...
    # looks crazy, but we're essentially taking all the UNIQUE variables in
    # the model formula then subtract 1 for the outcome variable and 1 for the
    # reference variable
    number_arrays  <- length(model_object$lab) - 1
    number_columns <- length(point_estimates)/number_arrays
    number_rows    <- sample_size
    
    # then re-arrange simulates to array format for MNL simulation
    sim_beta_array <- array(NA, dim = c(number_rows, 
                                        number_columns, 
                                        number_arrays)
    )  
    index_starts <- seq(from = 1, to = number_columns * number_arrays, 
                        by = number_columns)
    for(i in 1:number_arrays) {
        sim_beta_array[, , i] <- sim_betas[, index_starts[i]:(index_starts[i] 
                                                              + number_columns -
                                                                  1)]
    }
    
    # return the re-arranged coefficient estimates
    return(sim_beta_array)
}

# generate data to feed the coefficient estimates
get_new_data <- function(exp_data, 
                         base_data, 
                         model_object,
                         x_axis_selected, 
                         x_range = NULL, 
                         x_range_density = 100,
                         facet_selected = NULL,
                         interaction_col_names = NA) {
    # check if an explicit range has been provided for the x-axis variable
    if(is.null(x_range)) {
        # if not provided, calculate the range from the dataset
        # floor and ceiling used to insure some space around the observed data
        x_range[1] <- floor(min(exp_data[x_axis_selected]))
        x_range[2] <- ceiling(max(exp_data[x_axis_selected]))
    }
    
    # initialize the minimum set of counterfactuals (the x-axis variable cuts)
    counterfactuals <- seq(x_range[1], x_range[2], length.out = x_range_density)
    # check if a facet variable has been set
    if(!is.null(facet_selected)) {
        # if a facet variable has been set, we expand the counterfactuals
        # to include all x-axis variable/facet variable combinations
        # first we get the levels from original dataset
        var_levels <- with(base_data, levels(get(facet_selected)))
        # get all combinations of the factor name combined with the level name  
        # (in the order that the levels are set)
        factor_var_combinations <- paste0(facet_selected, var_levels)
        # expand the counterfactual set to include appropriate combinations of the
        # factor/level columns - all having range(0, 1, 1)
        # first treat the initial counterfactual set explicitly as the x_axis
        # cuts
        x_axis_cuts <- counterfactuals
        # then create the long, factor format of all x_axis/factor combos
        # NOTE: sorts the facet_var column which messes up variable order
        counterfactuals <- data.frame(x_axis_cuts, facet_var = 
                                          rep(factor_var_combinations, 
                                              each = length(x_axis_cuts))
        )
        # create the wide format (x_axis_cuts column gets dropped)
        counterfactuals <- spread(counterfactuals, facet_var, x_axis_cuts)
        # correct the variable order
        counterfactuals <- counterfactuals[factor_var_combinations]
        # convert the results to 0s and 1s
        counterfactuals <- ifelse(is.na(counterfactuals), 0, 1)
        # add the x_axis_cuts back in as the first column
        counterfactuals <- data.frame(rep(x_axis_cuts), counterfactuals)
        # drop the reference level
        counterfactuals <- counterfactuals[, -2]
        # label the x_axis_cuts column properly
        names(counterfactuals)[1] <- x_axis_selected
    } else {
        # if no facet variable, simply expand the counterfactual vector to a
        # one-column dataframe and label the column properly
        counterfactuals <- expand.grid(counterfactuals)
        names(counterfactuals) <- x_axis_selected
    }
    # finally, we check if there are additional predictors...
    # (the "formula" call ensures we get the actual formula object rather
    # than a reference to the object)
    exp_formula <- formula(model_object$call[[2]])
    variable_names <- all.vars(exp_formula)
    predictor_names <- variable_names[-1]   # drop off the outcome variable
    # we compare the total number of predictors against the number of columns
    # in the counterfactual table...
    if(length(predictor_names) > ncol(counterfactuals)) {
        # if there are predictors not yet represented in the counterfactual set
        # we define a regex search term that will match the x-axis and (if used)
        # facet variables and we drop ALL partial and complete matches
        # (getting rid of any interaction terms as well)
        if(!is.null(facet_selected)) {
            retained_index <- !grepl(paste(x_axis_selected, 
                                           facet_selected, 
                                           sep = "|"), 
                                     predictor_names)
        } else {
            retained_index <- !grepl(x_axis_selected, 
                                     predictor_names)
        }
        # we drop all the matches, leaving just the (non-interaction) 
        # predictors that we need to fix to a single value
        extra_predictors <- predictor_names[retained_index]
        # we quickly capture the current number of columns in our 
        # countefactual table and add one to it (giving us the index
        # for where we are adding new columns)
        offset_amount <- ncol(counterfactuals) + 1
        
        # now we get the means for the fixed predictors...
        mean_set <- NULL
        for(i in 1:length(extra_predictors)) {
            mean_set[i] <- mean(as.numeric(exp_data[, extra_predictors[i]]), 
                                na.rm = T)
        }
        # and attach those means to the current counterfactual set
        for(i in 1:length(extra_predictors)) {
            counterfactuals <- cbind(counterfactuals, mean_set[i])
        }
        names(counterfactuals)[offset_amount:ncol(counterfactuals)] <- 
            extra_predictors
    }
    
    # now we check if we have interaction columns in our predictors
    if(!is.na(interaction_col_names)) {
        # if we find them, we pull those terms out
        interaction_index <- predictor_names %in% interaction_col_names
        interaction_vars <- predictor_names[interaction_index]
        # create a list with the items in each term split (looks crazy but
        # I'm simply replacing the first period with a unique phrase so that
        # we only split once)
        interaction_list <- sub(".", "---", interaction_vars, fixed = TRUE)
        interaction_list <- strsplit(interaction_list, "---", fixed = T)
        # we quickly capture the current number of columns in our 
        # countefactual table and add one to it (giving us the index
        # for where we are adding new columns)
        offset_amount2 <- ncol(counterfactuals) + 1
        # for each split term, we take the matching columns in the 
        # counterfactual table and multiply them together to create a new
        # column for the interaction term
        for(current_set in 1:length(interaction_list)) {
            matching_cols <- counterfactuals[interaction_list[[current_set]]]
            new_col <- apply(matching_cols, 1, prod)
            counterfactuals <- cbind(counterfactuals, new_col)
        }
        # we then give our interaction columns their proper names
        names(counterfactuals)[offset_amount2:ncol(counterfactuals)] <- 
            interaction_vars
    }
    
    # now we quickly reorder our new data object so that the columns match
    # the order of our simulated coefficients objects
    counterfactuals <- counterfactuals[all.vars(exp_formula)[2:length(all.vars(exp_formula))]]
    
    # we wrap up by returning the counterfactual set
    return(counterfactuals)
}

# simulate expected probabilities using the new data and the coefficients
# NO NEW FUNCTION NEEDED - THIS IS HANDLED BY mlogitsimev

###############################################################################
## FUNCTIONS TO VISUALIZE OUTCOME PREDICTIONS (USER SELECTED X-AXIS/FACETTING)

format_for_visualization <- function(raw_likelihoods, 
                                     model_object, 
                                     base_data,
                                     counterfactuals,
                                     x_axis_selected = NA, 
                                     facet_selected = NULL) {
    
    # the mlogit structure is a collection of arrays but ggplot wants dataframes
    # first we extract the arrays as matrices and bind them together
    # NOTE: the lower/upper arrays will have as many dimensions as there are
    #       confidence intervals (here we have 2 dimensions because we ask
    #       for 95 and 50 percent CI in our mlogitsimev call)
    num_col <- ncol(raw_likelihoods$lower)
    tidy_sim <- rbind(matrix(raw_likelihoods$lower[, , 1], ncol = num_col),
                      matrix(raw_likelihoods$lower[, , 2], ncol = num_col),
                      matrix(raw_likelihoods$upper[, , 1], ncol = num_col),
                      matrix(raw_likelihoods$upper[, , 2], ncol = num_col),
                      matrix(raw_likelihoods$pe, ncol = num_col)
    )
    
    # then we format the resulting collection to be properly grouped and
    # labelled for visualizing
    tidy_sim <- data.frame(tidy_sim)
    # the outcome names are retained in the model object - we take these and
    # label our prediction dataframe columns accordingly
    names(tidy_sim) <- model_object$lab
    # add a grouping variable for the three types of measures we get from
    # the prediction object
    tidy_sim$measure_type <- rep(c("lower95", "lower50", 
                                   "upper95", "upper50", 
                                   "pe"), 
                                 each = nrow(raw_likelihoods$upper))
    # if available,
    # we also add the predictor (x-axis) value that will link the unique sets
    # (lower, upper, pe) - this should naturally repeat to the appropriate
    # length
    if(!is.na(x_axis_selected)) {
        tidy_sim$predictor <- rep(counterfactuals[[x_axis_selected]])
    } else {
        # if no x-axis given, we just slap on a row-count
        tidy_sim$predictor <- rep(1:nrow(counterfactuals))
    }
    # finally, if there is a facet variable set, we also add it as a grouping 
    # variable (create a new summary variable rather than deal with the 
    # already existing columns)
    if(!is.null(facet_selected)) {
        # we get the levels from the original data object
        factor_levels <- with(base_data, levels(get(facet_selected)))
        # the number of repitions of the factor is determined by the length
        # of the x_axis variable / number of unique factor levels
        num_reps <- nrow(raw_likelihoods$upper) / length(factor_levels)
        # finally add the grouping variable
        tidy_sim$facet <- rep(factor_levels, each = num_reps)
    }    
    
    # collapsing and spreading variables to make visualizing easy
    # (this is a tad arbitrary - it is consisent with Brian's interpretation of
    # good ggplot practice)
    if(!is.null(facet_selected)) {
        # if a facet variable is set, respect it...
        tidy_sim <- gather(tidy_sim, outcome, likelihood, -measure_type, 
                           -predictor, -facet)    
    } else {
        # otherwise don't because it's not there
        tidy_sim <- gather(tidy_sim, outcome, likelihood, -measure_type, 
                           -predictor)
    }
    tidy_sim <- spread(tidy_sim, measure_type, likelihood)
    
    # returning our visualization-ready data
    return(tidy_sim)
}

get_ribbon_plot <- function(formatted_likelihoods,
                            facet_selected = NULL,
                            x_lab = "Predictor", 
                            y_lab = "p(Outcome)",
                            custom_colors = NULL) {
    
    # build the plot object
    plot_object <- ggplot(formatted_likelihoods, aes(x = predictor, y = pe, 
                                                     group = outcome, 
                                                     ymin = lower95, ymax = upper95)) + 
        # takes the ymin and ymax and draws a ribbon around the lines
        geom_ribbon(alpha = 0.5, aes(fill = outcome)) + 
        geom_ribbon(alpha = 0.5, aes(fill = outcome,
                                     ymin = lower50, ymax = upper50)) +
        #geom_line(aes(color = outcome)) +
        scale_y_continuous(limits = c(0, 1),
                           labels = scales::percent,
                           expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0)) +
        theme_bw(16) +
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              strip.text = element_text(color = "white")) +
        xlab(x_lab) +
        ylab(y_lab)
    
    # if custom colors are provided, adjust the color scale
    if(!is.null(custom_colors)) {
        plot_object <- plot_object + 
            scale_fill_manual(values = custom_colors) +
            theme(strip.background = element_rect(color = custom_colors[8], 
                                                  fill = custom_colors[8]),
                  panel.border = element_rect(color = custom_colors[8]),
                  axis.ticks = element_line(color = custom_colors[8]))
    }
    
    # if a facet variable is set, add the facet layer to the plot object
    if(!is.null(facet_selected)) {
        plot_object <- plot_object + facet_wrap(~ facet, ncol = 2)
    }
    
    # return the plot object
    return(plot_object)
}

get_error_bar_plot <- function(formatted_likelihoods,
                               x_lab = "Outcome", 
                               y_lab = "p(Outcome)",
                               custom_colors = NULL) {
    # build the plot object
    plot_object <- ggplot(formatted_likelihoods, 
                          aes(x = outcome, 
                              color = outcome,
                              group = outcome)) + 
        geom_errorbar(aes(ymin = lower50, ymax = upper50)) +
        geom_errorbar(aes(ymin = lower95, ymax = upper95,
                          width = 0.5)) +
        scale_y_continuous(limits = c(0, 1),
                           labels = scales::percent) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              strip.text = element_text(color = "white")) +
        theme(legend.position="none") +
        xlab(x_lab) +
        ylab(y_lab) +
        coord_flip()
    
    # if custom colors are provided, adjust the color scale
    if(!is.null(custom_colors)) {
        plot_object <- plot_object + 
            scale_color_manual(values = custom_colors) +
            theme(strip.background = element_rect(color = custom_colors[8], 
                                                  fill = custom_colors[8]),
                  panel.border = element_rect(color = custom_colors[8]),
                  axis.ticks = element_line(color = custom_colors[8]))
    }
    
    # return the plot object
    return(plot_object)
}

get_dot_cloud_plot <- function(formatted_likelihoods,
                               x_lab = "Outcome", 
                               y_lab = "p(Outcome)",
                               custom_colors = NULL) {
    # build the plot object
    plot_object <- ggplot(formatted_likelihoods, 
                          aes(x = outcome, y = single_pe,
                              color = outcome, alpha = 0.10)) + 
        geom_jitter(position = position_jitter(width = 0.25, height = 0)) +
        scale_y_continuous(limits = c(0, 1),
                           labels = scales::percent) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              strip.text = element_text(color = "white")) +
        theme(legend.position="none") +
        xlab(x_lab) +
        ylab(y_lab) +
        coord_flip()
    
    # if custom colors are provided, adjust the color scale
    if(!is.null(custom_colors)) {
        plot_object <- plot_object + 
            scale_color_manual(values = custom_colors) +
            theme(strip.background = element_rect(color = custom_colors[8], 
                                                  fill = custom_colors[8]),
                  panel.border = element_rect(color = custom_colors[8]),
                  axis.ticks = element_line(color = custom_colors[8]))
    }
    
    # return the plot object
    return(plot_object)
}

###############################################################################
## ADDITIONAL HELPER FUNCTIONS

# This function expands the variable configuration object to include some more
# features, specifically those needed to define the sliders. It calculates
# these from the base data object.
add_slider_features <- function(variable_config_object, base_data) {
    # loop over the variables specified the variable configuration object
    for(index in 1:length(variable_config_object)) {
        # adjust object name to be more manageable
        vc <- variable_config_object
        
        # grab the current variable raw name
        current_var <- names(vc)[[index]]
        
        # if it's numeric, calculate the relevant values, otherwise assign NA
        # to the values to the properties exist but are appropriate for a non-
        # numeric variable
        if(is.numeric(base_data[[current_var]])) {
            current_median <- median(base_data[[current_var]])
            current_range <- range(base_data[[current_var]])
            
            # apply the variable's transform_to_ui function (making the values
            # ui friendly)
            current_median <- vc[[index]]$transform_for_ui(current_median)
            current_range[1] <- vc[[index]]$transform_for_ui(current_range[1])
            current_range[2] <- vc[[index]]$transform_for_ui(current_range[2])
            
            # round the range values so that ugly values are more ui friendly
            current_range[1] <- floor(current_range[1])
            current_range[2] <- ceiling(current_range[2])
        } else {
            current_median <- NA
            current_range <- NA
        }
        
        # add the values to the variable configuration
        variable_config_object[[current_var]]$ui_median <- current_median
        variable_config_object[[current_var]]$ui_min <- current_range[1]
        variable_config_object[[current_var]]$ui_max <- current_range[2]
    }
    
    # return the update variable_config_object
    return(variable_config_object)
}

# This function generates the actual slider objects. It expects the variable
# configuration object to determine which variables to make sliders for, but
# will also accept a vector of raw variable names that overrides the variable 
# configuration file to exclude selected variables. A unique "append" name must
# also be provided to ensure that separate slider sets to not share name space.
make_sliders <- function(variable_config_list, 
                         variables_to_drop = NA,
                         append_name) {
    # index which variables are slider candidates
    slider_index <- unlist(lapply(variable_config_list, 
                                  function(x) x$slider_candidate == TRUE))
    
    # if a vector of variables to drop has been provided, adjust their indices 
    # to FALSE so sliders are not made for them
    if(!is.na(variables_to_drop)) {
        for(index in 1:length(variables_to_drop)) {
            slider_index[variables_to_drop[index]] <- FALSE
        }
    }
    
    # subset variable_config_list to just get the slider candidates
    # (excluding the x-axis variabe)
    selected_sliders <- variable_config_list[slider_index]
    
    # generate the sliders
    lapply(1:length(selected_sliders), function(i) {
        sliderInput(
            inputId = paste0(append_name,
                             "_",
                             names(selected_sliders)[i]), 
            label   = selected_sliders[[i]]$pretty_name,
            min     = selected_sliders[[i]]$ui_min, 
            max     = selected_sliders[[i]]$ui_max,
            value   = selected_sliders[[i]]$ui_median,
            step    = ifelse(is.na(selected_sliders[[i]]$slider_rounding),
                             0.01,
                             selected_sliders[[i]]$slider_rounding)
        )
    })
}

# This function updates the base data object to create a data object adjusted
# for appropriate slider values. The inputs to the function should echo the
# inputs used to make the slider set with the addition of specifying the 
# target object. To establish the reactive link, we also need to explicitly
# pass the input object.
apply_slider_values <- function(variable_config_list,
                                variables_to_drop = NA,
                                append_name,
                                update_target,
                                input_call,
                                interaction_col_names) {
    # index which variables are slider candidates
    slider_index <- unlist(lapply(variable_config_list, 
                                  function(x) x$slider_candidate == TRUE))
    
    # if a vector of variables to drop has been provided, adjust their indices 
    # to FALSE so sliders are not made for them
    if(!is.na(variables_to_drop)) {
        for(index in 1:length(variables_to_drop)) {
            slider_index[variables_to_drop[index]] <- FALSE
        }
    }
    
    # subset variable_config_list to just get the slider candidates
    # (excluding the x-axis variabe)
    selected_sliders <- variable_config_list[slider_index]
    
    # update the values based on current slider inputs
    for(i in 1:length(selected_sliders)) {
        # grab the key details
        current_var <- names(selected_sliders)[[i]]
        slider_name <- paste0(append_name, "_", current_var)
        slider_value <- isolate(input_call[[slider_name]])
        
        # all the input variables initialize as "NULL" - we want to avoid
        # working with them until they've been assigned a value
        # NOTE: input[[current_var]] IS a reactive link (actually a flexible 
        #       set of reactive links) - it links to the sliders dynamically 
        #       generated by the output$slider_set observer
        if(!is.null(isolate(input_call[[slider_name]]))) {
            # apply the relevant transformation to convert slider values
            # to model-appropriate values
            model_value <- selected_sliders[[current_var]]$transform_for_model(slider_value)
            
            # update the matching update_target column with the 
            # current slider value
            update_target[current_var] <- model_value
            
            # now we check if there any interactions...
            if(!is.na(interaction_col_names)) {
                interaction_index <- grepl(paste(paste0(".", current_var), 
                                                 paste0(current_var, "."), 
                                                 sep = "|"),
                                           interaction_col_names)
            } else {
                interaction_index <- FALSE
            }
            # if there are any interactions...
            if(any(interaction_index)) {
                # if we find interactions, we pull those column names out
                interaction_vars <- interaction_col_names[interaction_index]
                # create a list with the items in each term split (looks crazy but
                # I'm simply replacing the first period with a unique phrase so that
                # we only split once)
                interaction_list <- sub(".", "---", interaction_vars, fixed = TRUE)
                interaction_list <- strsplit(interaction_list, "---", fixed = T)
                # update the interaction variables by multiplying their
                # source columns together
                for(current_set in 1:length(interaction_list)) {
                    matching_cols <- update_target[interaction_list[[current_set]]]
                    updated_col <- apply(matching_cols, 1, prod)
                    update_target[interaction_vars[[current_set]]] <- updated_col
                }
            }
        }
    }
    
    return(update_target)
}

# We need to be able to identify columns that are the result of interactions
# between base variables. However, the base data expansion makes it difficult
# to recover these columns - it also expands factor levels and does not use
# a unique separator for interaction combinations v. factor levels. This
# function finds the possible name permutations that may result from
# interactions and identifies columns that have matches - which will be the
# interaction columns.
get_interaction_col_names <- function(base_formula, exp_data) {
    # convert the base formula into a single character string (ignoring the outcome
    # variable)
    formula_string <- as.character(base_formula)[[3]]
    
    # parse all the separate terms (not variables but rather anything that occurs
    # before or after a "+" symbol)
    formula_parsed <- strsplit(formula_string, "+", fixed = TRUE)
    
    # get rid of any spaces (note that strsplit returns a list - we unlist to make
    # sure we work with a character vector)
    formula_parsed <- gsub(" ", "", unlist(formula_parsed))
    
    # now we collect just those terms that have an interaction symbol "*"
    interaction_terms <- grep("*", formula_parsed, fixed = TRUE, value = TRUE)
    
    # quickly check to see if there are any interaction terms at all - if none
    # we want to return "NA" so that later steps in this function don't fail
    # and so that we can identify the lack of interaction terms correctly in
    # later functions
    if(length(interaction_terms) == 0) {
        return(NA)
    }
    
    # for each term, we extract the variable names
    interaction_terms <- strsplit(interaction_terms, "*", fixed = TRUE)
    
    # for each term, we need to construct all possible combinations of the variable
    # names (variable names separated by a ".") - these are what we will use to
    # identity the interaction data columns in the expanded data frame
    # 1. get the permutations of the variable name strings
    interaction_combos <- lapply(interaction_terms, permn)
    # 2. collapse into single strings separated by a "."
    interaction_combos <- lapply(interaction_combos, 
                                 function(x) lapply(x, paste0, collapse = "."))
    # 3. switch the ugly list to a clean character vector
    interaction_combos <- unlist(interaction_combos)
    
    # test the expanded data object names against the vector to determine which
    # columns have combinations of the variable names separated by a "." (thus
    # indicating an interaction column)
    # 1. test each permutation against the column names
    interaction_index <- lapply(interaction_combos, 
                                function(x) grepl(x, names(exp_data)))
    # 2. collapse the results - if a column name matches ANY permutation it is
    #    any interaction column
    interaction_index <- do.call(rbind, interaction_index)
    interaction_index <- apply(interaction_index, 2, any)
    # 3. get the subset of column names that match a permutation
    interaction_col_names <- names(exp_data)[interaction_index]
    
    # return the names of interaction columns
    return(interaction_col_names)
}

###############################################################################
## END OF SCRIPT
###############################################################################