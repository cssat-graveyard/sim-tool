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
# - source the current draft of the "gator model example" script
#       - this loads an example dataset and model along with all the functions
#         needed to get simulated likelihood predictions from the model and
#         to visualize these predictions in a ribbon plot
#
# - build the necessary data objects from the given dataset and model
#       - 
#
# - define the shinyServer loop to turn the data objects into Shiny's output
#   objects (and define any desired interactivty)

###############################################################################
## SOURCE GATOR MODEL CURRENT DRAFT

source("gator model example V3.R")

point_estimates <- get_point_estimates(gator_logit)
cov_matrix <- get_covariance_matrix(gator_logit)
coeff_estimates <- get_coefficient_estimates(1000, point_estimates, 
                                             cov_matrix, gator_logit)

###############################################################################
## DEFINE THE OBJECTS FOR THE UI TO DISPLAY

shinyServer(function(input, output) {
    output$demo_plot <- renderPlot({
        x_axis_selected <- switch(input$predictor_choice,
                                  "Age" = "size",
                                  "Income" = "teeth")
        
        facet_selected <- switch(input$facet_choice,
                                 "None" = NULL,
                                 "Sex" = "sex")
        
        x_label <- input$predictor_choice
        
        new_data <- get_new_data(gator, gator_logit, x_axis_selected, facet_variable = facet_selected)
        prediction_object <- mlogitsimev(new_data, coeff_estimates, ci = 0.67)
        visualize_predictions(prediction_object, gator_logit, 
                              new_data, x_axis_selected, facet_variable = facet_selected,
                              y_lab = "Probability", x_lab = x_label)
    })
})



###############################################################################
## END OF SCRIPT
###############################################################################