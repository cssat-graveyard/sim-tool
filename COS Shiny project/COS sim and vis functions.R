# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/25/2015
# Date updated: 4/8/2015

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
# - WRAPPER FUNCTION FOR SIMULATION AND VISUALIZATION (FOR OFFLINE TESTING)
#   [PROBABLY OUTDATED - DON'T USE WITHOUT INSPECTING]
#   - simply lets you manually generate plot objects to test app behavior
#     "under the hood"
#
# - ADDITIONAL HELPER FUNCTIONS (TO CONSOLIDATE CERTAIN SHINY SERVER TASKS)
#   - generating the slider names, ranges, and starting values

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
get_new_data <- function(exp_data, base_data, model_object,
                         x_axis_variable, x_range = NULL, x_range_density = 100,
                         facet_variable = NULL) {
    # check if an explicit range has been provided for the x-axis variable
    if(is.null(x_range)) {
        # if not provided, calculate the range from the dataset
        # floor and ceiling used to insure some space around the observed data
        x_range[1] <- floor(min(exp_data[x_axis_variable]))
        x_range[2] <- ceiling(max(exp_data[x_axis_variable]))
    }
    
    # initialize the minimum set of counterfactuals (the x-axis variable cuts)
    counterfactuals <- seq(x_range[1], x_range[2], length.out = x_range_density)
    
    # check if a facet variable has been set
    if(!is.null(facet_variable)) {
        # if a facet variable has been set, we expand the counterfactuals
        # to include all x-axis variable/facet variable combinations
        # first we get the levels from original dataset
        var_levels <- with(base_data, levels(get(facet_variable)))
        # get all combinations of the factor name combined with the level name  
        # (in the order that the levels are set)
        factor_var_combinations <- paste0(facet_variable, var_levels)
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
        # drop the reference level
        counterfactuals <- counterfactuals[, -1]
        # add the x_axis_cuts back in as the first column
        counterfactuals <- data.frame(rep(x_axis_cuts), counterfactuals)
        # label the x_axis_cuts column properly
        names(counterfactuals)[1] <- x_axis_variable
    } else {
        # if no facet variable, simply expand the counterfactual vector to a
        # one-column dataframe and label the column properly
        counterfactuals <- expand.grid(counterfactuals)
        names(counterfactuals) <- x_axis_variable
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
        if(!is.null(facet_variable)) {
            retained_index <- !grepl(paste(x_axis_variable, 
                                           facet_variable, 
                                           sep = "|"), 
                                     predictor_names)
        } else {
            retained_index <- !grepl(x_axis_variable, 
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
    
    # now we explore our predictor set for any interaction terms
    interaction_index <- grepl(".", predictor_names, fixed = T)
    if(any(interaction_index)) {
        # if we find them, we pull those terms out
        interaction_vars <- predictor_names[interaction_index]
        # create a list with the items in each term split
        interaction_list <- strsplit(interaction_vars, ".", fixed = T)
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

format_for_visualization <- function(prediction_object, 
                                     model_object, 
                                     base_data,
                                     counterfactuals,
                                     x_axis_variable, 
                                     facet_variable = NULL) {
    
    # the mlogit structure is a collection of arrays but ggplot wants dataframes
    # first we extract the arrays as matrices and bind them together
    # NOTE: the lower/upper arrays will have as many dimensions as there are
    #       confidence intervals (here we have 2 dimensions because we ask
    #       for 95 and 50 percent CI in our mlogitsimev call)
    num_col <- ncol(prediction_object$lower)
    tidy_sim <- rbind(matrix(prediction_object$lower[, , 1], ncol = num_col),
                      matrix(prediction_object$lower[, , 2], ncol = num_col),
                      matrix(prediction_object$upper[, , 1], ncol = num_col),
                      matrix(prediction_object$upper[, , 2], ncol = num_col),
                      matrix(prediction_object$pe, ncol = num_col)
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
                                 each = nrow(prediction_object$upper))
    # we also add the predictor (x-axis) value that will link the unique sets
    # (lower, upper, pe) - this should naturally repeat to the appropriate
    # length
    tidy_sim$predictor <- rep(counterfactuals[[x_axis_variable]])
    # finally, if there is a facet variable set, we also add it as a grouping 
    # variable (create a new summary variable rather than deal with the 
    # already existing columns)
    if(!is.null(facet_variable)) {
        # we get the levels from the original data object
        factor_levels <- with(base_data, levels(get(facet_variable)))
        # the number of repitions of the factor is determined by the length
        # of the x_axis variable / number of unique factor levels
        num_reps <- nrow(prediction_object$upper) / length(factor_levels)
        # finally add the grouping variable
        tidy_sim$facet <- rep(factor_levels, each = num_reps)
    }    
    
    # collapsing and spreading variables to make visualizing easy
    # (this is a tad arbitrary - it is consisent with Brian's interpretation of
    # good ggplot practice)
    if(!is.null(facet_variable)) {
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

get_ribbon_plot <- function(formatted_data,
                            facet_variable = NULL,
                            x_lab = "Predictor", 
                            y_lab = "p(Outcome)",
                            custom_colors = NULL) {
    
    # build the plot object
    plot_object <- ggplot(formatted_data, aes(x = predictor, y = pe, 
                                              group = outcome, 
                                              ymin = lower95, ymax = upper95)) + 
        # takes the ymin and ymax and draws a ribbon around the lines
        geom_ribbon(alpha = 0.5, aes(fill = outcome)) + 
        geom_ribbon(alpha = 0.5, aes(fill = outcome,
                                     ymin = lower50, ymax = upper50)) +
        #geom_line(aes(color = outcome)) +
        scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank()) +
        xlab(x_lab) +
        ylab(y_lab)
    
    # if custom colors are provided, adjust the color scale
    if(!is.null(custom_colors)) {
        plot_object <- plot_object + scale_fill_manual(values = custom_colors)
    }
    
    # if a facet variable is set, add the facet layer to the plot object
    if(!is.null(facet_variable)) {
        plot_object <- plot_object + facet_wrap(~ facet, ncol = 2)
    }
    
    # return the plot object
    return(plot_object)
}

###############################################################################
## TEST WRAPPER
# OUT OF DATE - REVIEW BEFORE TRYING TO USE

test_wrapper <- function(dataset, model_object, 
                         x_axis_variable, facet_variable = NULL,
                         coeff_sample_size = 1000) {
    # create the supporting objects
    pe <- get_point_estimates(model_object)
    cvm <- get_covariance_matrix(model_object)
    ce <- get_coefficient_estimates(coeff_sample_size, pe, cvm, 
                                    model_object)
    nd <- get_new_data(dataset, model_object, 
                       x_axis_variable, facet_variable = facet_variable)
    
    # generate the predictions
    prediction_object <- mlogitsimev(nd, ce, ci = 0.67)
    
    # generate the plot object
    plot_object <- visualize_predictions(prediction_object, model_object, 
                                         nd, x_axis_variable, facet_variable)
    # return the plot object
    return(plot_object)
}

###############################################################################
## ADDITIONAL HELPER FUNCTIONS

# This function will generate the key features needed to create the sliders. By
# default it expects an explicit index of variables that are allowed to be
# given a slider. However, 'auto' mode can be turned on and - so long as 
# the outcome_variable is provided - the function will generate sliders
# intelligently (but inefficiently) from the data itself.
get_sliders <- function(x_axis_selected,
                        slider_raw_names,
                        slider_pretty_names = NA,
                        base_data,
                        auto = FALSE, 
                        outcome_variable = NULL) {
    
    if(auto) {
        # make index of variables in the base dataset which are NOT factors
        slider_index <- which(!sapply(base_data, is.factor))
        
        # if it was retained, set the outcome variable index to FALSE
        outcome_retained <- grepl(outcome_variable, names(slider_index))
        if(any(outcome_retained)) {
            slider_index[outcome_retained] <- FALSE
        }
    } else {
        # if 'auto' is FALSE, then expect raw names to be provided and use them
        slider_index <- names(base_data) %in% slider_raw_names
        # this approach loses the variable names, so we restore them
        names(slider_index) <- names(base_data)
    }
    
    # set the x-axis variable to FALSE in the index
    x_axis_variable <- grepl(x_axis_selected, names(slider_index))
    slider_index[x_axis_variable] <- FALSE
    
    # create a dataframe with just the retained slider variables
    slider_selections <- base_data[slider_index]
    
    # create dataframe with basic values for all predictors that need sliders
    slider_features <- c()
    for(index in 1:length(slider_selections)) {
        var_raw_name <- names(slider_selections)[index]
        # get a reasonable range from the base_data object (floor and ceiling
        # used to make sure we have round, inclusive numbers)
        var_min <- floor(range(with(slider_selections, get(var_raw_name)))[1])
        var_max <- ceiling(range(with(slider_selections, get(var_raw_name)))[2])
        # the starting value defaults to the base data median (matches the
        # initial behavior of the get_new_data function and thus the initial
        # new_data object)
        var_median <- median(with(slider_selections, get(var_raw_name)))
        slider_features <- rbind(slider_features, 
                                 data.frame(var_raw_name,
                                            var_min, var_max, var_median,
                                            stringsAsFactors = FALSE)
        )
    }
    
    # if pretty names were given, add these to the features...
    if(!is.na(slider_pretty_names)) {
        # but first have to drop out the x-axis variable again
        x_axis_variable <- grepl(x_axis_selected, slider_raw_names)
        slider_pretty_names <- slider_pretty_names[!x_axis_variable]
        # then assign the pretty names
        slider_features$var_pretty_name <- slider_pretty_names
    }
    
    # return the dataframe of slider features
    return(slider_features)
}

###############################################################################
## END OF SCRIPT
###############################################################################