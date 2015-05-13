# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 5/12/2015
# Date updated: 5/13/2015

###############################################################################
## SCRIPT OVERVIEW

# GOAL: MOS_config.R is where a Multinomial Outcome Simulator (MOS) instance
#       is defined by an application administrator.
#
#       The MOS is a Shiny application that allows users to interact with
#       visualizations of a given multinomial logit model. Data and a model
#       formula are provided to the application, which then builds the 
#       visualizations by:
#       (a) getting a model fit
#       (b) simulating outcomes from that model based on user inputs
#       (c) plotting the outcomes in the context of the user inputs
#
#       The MOS should (theoretically) take any multinomial logit formula
#       and its accompanying, properly formatted R data frame.
#
#       However, certain features of the MOS needed to be specified for it
#       to work.
#
#       This config file is where those features are specified. It is sourced
#       by the MOS ui.R file when the application is initialized.
#
# SCRIPT OUTLINE:
# - Name the Application Instance
#   - This is the title that will be displayed in the instance navigation and
#     should be a very concise description (e.g., "The Case Outcome 
#     Simulator")
#
# - Import and Name the Data Object as Needed
#   - The multinomial logit model needs to be fit to a dataset. This dataset
#     needs to be be an R data frame named "base_data" (no quotes).
#   - This section is where the data frame is created (however that needs to 
#     be done) and assigned to "base_data".
#
# - Specify the Multinomial Logit Formula
#   - The multinomial logit formula needs to be provided explicitly and it
#     needs to appropriately reference the "base_data" data frame. The formula
#     needs to be assigned to "base_formula".
#
# - Variable Configuration
#   - We need to specify which variables will be visible to the user (as a 
#     slider, facet, or outcome). Key information must be provided for each
#     of these variables. See the section for details.
#
# - Custom Visualization Colors (Optional)
#   - If custom colors are desired, set "use_custom_colors" to "TRUE".
#   - Assign the custom colors (the same number as there are outcomes) to
#     the character string "custom_colors".
#
# - Custom bootstrap.css (Optional)
#   - If a custom bootsrap.css file will be used to format the application
#     appearance, set "use_custom_css" to "TRUE".
#   - The bootstrap.css file should be placed in a subdirectory of the
#     application titled "www".
#   - Assign the name of bootstrap.css file to the character string 
#     "custom_css".

###############################################################################
## SCRIPT OVERVIEW

###############################################################################
## END OF SCRIPT
###############################################################################