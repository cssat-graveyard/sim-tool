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
#   - select the desired model 
#   - extract the dataframe object from it
#
# - build any static objects needed for simulation/visualization
#
# - define the shinyServer loop to turn the data objects into Shiny's output
#   objects (and define any desired interactivty)

###############################################################################
## SOURCE THE MODEL AND SIM/VIS SCRIPTS

source("COS test models.R")
source("COS sim and vis functions.R")

selected_model <- add_logit
model_data <- model.frame(selected_model)

###############################################################################
## STATIC OBJECTS FOR SIMULATION/VISUALIZATION

point_estimates <- get_point_estimates(selected_model)
cov_matrix <- get_covariance_matrix(selected_model)
coeff_estimates <- get_coefficient_estimates(1000, point_estimates, 
                                             cov_matrix, selected_model)

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
        
        x_label <- input$predictor_choice
        
        new_data <- get_new_data(model_data, 
                                 selected_model, 
                                 x_axis_selected, 
                                 facet_variable = facet_selected)
        prediction_object <- mlogitsimev(new_data, coeff_estimates, ci = 0.67)
        visualize_predictions(prediction_object, 
                              selected_model, 
                              new_data, 
                              x_axis_selected, 
                              facet_variable = facet_selected,
                              y_lab = "Probability", x_lab = x_label)
    })
})



###############################################################################
## END OF SCRIPT
###############################################################################