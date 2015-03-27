# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/23/2015
# Date updated: 

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
# - source the model script and sim/vis functions script
#
# - build any static objects needed for simulation/visualization
#   - select the source data
#   - define the model formula
#   - create an expanded version of the data
#   - create all simulation objects except the new data and the visualization
#
# - define the shinyServer loop to turn the data objects into Shiny's output
#   objects (and define any desired interactivty)
#   - collect user inputs
#   - generate the new data based on user input
#   - feed the new data to the simulated coefficients
#   - generate the visualization based on user input

###############################################################################
## SOURCE THE MODEL AND SIM/VIS SCRIPTS

source("COS test models.R")
source("COS sim and vis functions.R")

###############################################################################
## STATIC OBJECTS FOR SIMULATION/VISUALIZATION

# choose the data object we will be working with and specify the formula
# (with respect to this data object)
base_data <<- base_data
model_formula <<- outcome ~ sex + age + income + iq

# expand the factors in the data object, re-add the outcome, drop the intercept
exp_data <<- model.matrix(model_formula, base_data)
outcome_variable <<- as.character(model_formula[[2]])
exp_data <<- data.frame(base_data[outcome_variable],  
                       exp_data)
exp_data[, "X.Intercept."] <<- NULL

# fit the model to the expanded dataset (using the expanded model)
exp_model <<- multinom(formula(exp_data), data = exp_data, Hess = T)

# get the static simulation/visualization features
point_estimates <<- get_point_estimates(exp_model)
cov_matrix <<- get_covariance_matrix(exp_model)
coeff_estimates <<- get_coefficient_estimates(1000, point_estimates, 
                                             cov_matrix, exp_model)

###############################################################################
## DEFINE THE OBJECTS FOR THE UI TO DISPLAY

shinyServer(function(input, output) {
    output$demo_plot <- renderPlot({
        x_axis_selected <- switch(input$predictor_choice,
                                  "Age" = "age",
                                  "Income" = "income",
                                  "IQ" = "iq")
        
        facet_selected <- switch(input$facet_choice,
                                 "None" = NULL,
                                 "Sex" = "sex")
        
        selected_ci <- input$ci/100
        
        x_label <- input$predictor_choice
        
        new_data <- get_new_data(exp_data,
                                 base_data,
                                 exp_model, 
                                 x_axis_selected, 
                                 facet_variable = facet_selected)
        
        prediction_object <- mlogitsimev(new_data, coeff_estimates, 
                                         ci = selected_ci)
        
        visualize_predictions(prediction_object, 
                              exp_model, 
                              base_data,
                              new_data, 
                              x_axis_selected, 
                              facet_variable = facet_selected,
                              y_lab = "Probability", x_lab = x_label)
    })
})



###############################################################################
## END OF SCRIPT
###############################################################################