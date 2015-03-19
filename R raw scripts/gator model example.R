# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 3/17/2015
# Date updated:

###############################################################################
## SCRIPT OVERVIEW

# goal: Prototype fitting a multonomial logit model and simulating the data.
#       Uses the simcf package and the example described in the help docs for
#       the mlogitsimev function.
#
#       Example source:
#       library(simcf)
#       help(mlogitsimev)
#
#       The goal is then take this example build a Shiny package on top of it.
#       We will explore how efficient it will be to simulate data from a 
#       multonomial logit model and and visualize the results on the fly via 
#       Shiny.

# sketch of script
# - COPY OF SCRIPT FROM HELP DOCS
#

###############################################################################
## COPY OF EXAMPLE SCRIPT
# Multinomial Logistic Regression of alligator food
# See tile package function lineplot for graphical presentation of this example

# Load data and libraries
# BRIAN UPDATE: the dataset comes from the simcf package - the help doc fails
#   to clarify this; I corrected the order to insure the loading occurs 
#   correctly; I also changed "require" to "library" since no testing of
#   the loading actually occurs (the point of using "require")
library(simcf)
library(MASS)
library(nnet)
data(gator)

# Estimate MNL using the nnet library
mlogit.result <- multinom(food ~ size + female, Hess=TRUE)
pe <- mlogit.result$wts[c(6,7,8,10,11,12)]
# point estimates
vc <- solve(mlogit.result$Hess)       # var-cov matrix

# Simulate parameters from predictive distributions
sims <- 10000
simbetas <- mvrnorm(sims,pe,vc)       # draw parameters, using MASS::mvrnorm
simb <- array(NA, dim = c(sims,3,2))  # re-arrange simulates to array format
simb[,,1] <- simbetas[,1:3]           #   for MNL simulation
simb[,,2] <- simbetas[,4:6]

# Create full factorial set of counterfactuals
# BRIAN UPDATE: original doc had a typo (cffactorial rather than the correct 
#   cfFactorial)
sizerange <- seq(1, 4, by = 0.1)          # range of counterfactual sizes
femalerange <- c(0,1)                 # range of counterfactual sexes
xhyp <- cfFactorial(size = sizerange, female = femalerange)

# Simulate expected probabilities
mlogit.qoi1 <- mlogitsimev(xhyp,simb,ci=0.67)
print(mlogit.qoi1)

###############################################################################
## BRIAN ADDITIONS: Cleaning up for visualizing with ggplot
# load the libraries for cleaning and visualizing
library(tidyr)
library(ggplot2)

# the mlogit structure is a collection of arrays but ggplot wants dataframes...
# extracting the arrays as matrices and binding them together
tidy_sim <- rbind(matrix(mlogit.qoi1$lower, ncol = 3),
                  matrix(mlogit.qoi1$upper, ncol = 3),
                  matrix(mlogit.qoi1$pe, ncol = 3)
)

# now formatting the resulting collection into a dataframe that fits our
# visualizing goals
tidy_sim <- data.frame(tidy_sim)
# the name info is specified in Adolph's lecture example, slide 53:
# http://faculty.washington.edu/cadolph/mle/topic5.p.pdf
names(tidy_sim) <- c("invertebrates", "fish", "other")
# adding factors to identify key features of the data - these may seem 
# arbitrary (sorry!) but are intuited from the example itself
tidy_sim$measure_type <- rep(c("lower", "upper", "pe"), each = 62)
tidy_sim$sex <- rep(c("male", "female"), each = 31, 3)
tidy_sim$size <- rep(sizerange, 6)
# collapsing and spreading variables to make visualizing easy - this is
# a bit arbitrary (convenient for how Brian uses ggplot)
tidy_sim <- gather(tidy_sim, food, measure, invertebrates, fish, other)
tidy_sim <- spread(tidy_sim, measure_type, measure)

# plot
ggplot(tidy_sim, aes(x = size, y = pe, group = food, ymin = lower, ymax = upper)) + 
    geom_line() +
    # takes the ymin and ymax and draws a ribbon around the lines
    geom_ribbon(alpha = 0.5, aes(fill = food)) + 
    theme_bw() +
    xlab("Length of Gator (Meters)") +
    ylab("p(Primary Food Type Is ... | Length)")



###############################################################################
## END OF SCRIPT
###############################################################################