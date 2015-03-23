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

source("gator model example V2.R")

###############################################################################
## DEFINE THE OBJECTS FOR THE UI TO DISPLAY

shinyServer(function(input, output) {
    output$demo1 <- renderPlot({
        demographics[[1]]
    })
    
    output$demo2 <- renderTable({
        demographics[[2]]
    })
    
    output$demo3 <- renderPlot({
        demographics[[3]]
    })
    
    output$demo4 <- renderTable({
        demographics[[4]]
    })
    
    output$demo5 <- renderPlot({
        demographics[[5]]
    })
    
    output$demo6 <- renderTable({
        demographics[[6]]
    })
    
    output$perf1 <- renderPlot({
        performance[[1]]
    })
    
    output$perf2 <- renderTable({
        performance[[2]]
    })
    
    output$perf3 <- renderPlot({
        performance[[3]]
    })
    
    output$perf4 <- renderTable({
        performance[[4]]
    })
})

###############################################################################
## END OF SCRIPT
###############################################################################