# Author: Brian Waismeyer
# Contact: bwaismeyer@gmail.com

# Date created: 4/8/2015
# Date updated: 4/14/2015

###############################################################################
## SCRIPT OVERVIEW

# goal: This is a minor revision of Chris Adolph's mlogitsimev function from
#       his simcf package (https://github.com/chrisadolph/tile-simcf).
#
#       The function passes representative generated data to a collection of
#       simulated coefficients. It then summarizes the results to return key
#       features for each representative case passed to the cofficients:
#       point estimate (mean), upper value (quantile based on given confidence 
#       interval), lower value (quantile based on given confidence interval).
#
#       The updated function:
#       1. Changes the point estimate technique from "mean" (as described above)
#          to "median". This is perhaps a more common choice for this kind of
#          simulation and avoids issues that arise as confidence intervals get
#          narrow (e.g., the mean falling outside the upper and lower 
#          quartiles).
#       2. Allows the user to request the "cloud" that results from feeding
#          the represenative data to the coefficient estimates. This prevents
#          the normal function behavior, which returns a summary of these 
#          results for each case in the representative data.
#       3. Adjusts order of the outcome columns to match intuitive expectations
#          (reference outcome is ordered as the FIRST column rather than LAST).
#          Originally, the reference outcome was ordered as the LAST column, but
#          in most models/outputs the reference outcome is ordered FIRST.

# sketch of script
# - the function as written by Chris Adolph
#   - any changes are marked with ## REVISION ##

###############################################################################
## STEP

mlogitsimev_med <- function (x, b, ci = 0.95, constant = 1, z = NULL, g = NULL, 
                             predict = FALSE, sims = 10, 
                             ## REVISION ##
                             # added return_cloud argument
                             return_cloud = FALSE) 
{
    if (!is.array(b)) {
        stop("b must be an array")
    }
    if (any(class(x) == "counterfactual") && !is.null(x$model)) {
        x <- model.matrix(x$model, x$x)
        x <- array(x, dim = c(nrow(x), ncol(x), dim(b)[3]))
    }
    else {
        if (any(class(x) == "list")) 
            x <- x$x
        if (is.data.frame(x)) 
            x <- as.matrix(x)
        if (!is.array(x)) {
            if (!is.matrix(x)) {
                x <- t(x)
            }
            x <- array(x, dim = c(nrow(x), ncol(x), dim(b)[3]))
        }
        else {
            x <- array(x, dim = c(nrow(x), ncol(x), dim(b)[3]))
        }
        if (!is.na(constant)) {
            xnew <- array(NA, dim = c(nrow(x), (ncol(x) + 1), 
                                      dim(b)[3]))
            for (i in 1:dim(x)[3]) {
                xnew[, , i] <- appendmatrix(x[, , i, drop = FALSE], 
                                            rep(1, dim(x)[1]), constant)
            }
            x <- xnew
        }
    }
    if (!is.null(g)) {
        usegamma <- TRUE
    }
    else {
        usegamma <- FALSE
    }
    if (usegamma && !is.array(z)) {
        stop("if g is provided, z must be an array with dimension 3 equal to the number of categories")
    }
    esims <- nrow(as.matrix(b))
    res <- list(lower = array(0, dim = c(dim(x)[1], (dim(x)[3] + 1), length(ci))), 
                upper = array(0, dim = c(dim(x)[1], (dim(x)[3] + 1), length(ci)))
                )
    if (predict) 
        res$pv <- NULL
    for (iscen in 1:dim(x)[1]) {
        simdenom <- 0
        for (icat in 1:(dim(b)[3])) {
            if (usegamma) {
                newdenom <- exp(b[, , icat] %*% x[iscen, , icat] + 
                                    g %*% z[iscen, , icat])
            }
            else {
                newdenom <- exp(b[, , icat] %*% x[iscen, , icat])
            }
            simdenom <- simdenom + newdenom
        }
        if (usegamma) {
            simdenom <- simdenom + exp(g %*% z[iscen, , dim(z)[3]])
        }
        else {
            simdenom <- simdenom + 1
        }
        simy <- matrix(NA, nrow = dim(b)[1], ncol = (dim(b)[3] + 
                                                         1))
        
        for (icat in 1:dim(x)[3]) {
            if (usegamma) 
                simy[, icat] <- exp(b[, , icat] %*% x[iscen, 
                                                      , icat] + g %*% z[iscen, , icat])/simdenom
            else simy[, icat] <- exp(b[, , icat] %*% x[iscen, 
                                                       , icat])/simdenom
        }
        if (usegamma) 
            simy[, ncol(simy)] <- exp(g %*% z[iscen, , dim(g)[3]])/simdenom
        else simy[, ncol(simy)] <- 1/simdenom
        
        simy <- apply(simy, 2, sort)
        
        ## REVISION ##
        # reorder columns so the REFERENCE OUTCOME is now the FIRST column
        simy <- simy[, c(4, 1:3)]
        
        
        ## REVISION ##
        # return the cloud of estimates if requested
        if(return_cloud) {
            return(simy)
        }
        
        ## REVISION ##
        # technique for calculating point estimate (pe) changed from mean to
        # median
        res$pe <- rbind(res$pe, apply(simy, 2, median))
        length.simy <- nrow(simy)
        low <- up <- NULL
        for (k in 1:length(ci)) {
            for (icat in 1:(dim(b)[3] + 1)) {
                res$lower[iscen, icat, k] <- rbind(low, quantile(simy[, 
                                                                      icat], probs = (1 - ci[k])/2))
                res$upper[iscen, icat, k] <- rbind(up, quantile(simy[, 
                                                                     icat], probs = (1 - (1 - ci[k])/2)))
            }
        }
        if (predict) {
            pv <- NULL
            for (ipred in 1:dim(b)[1]) {
                pv <- c(pv, resample(1:dim(simy)[2], size = sims, 
                                     prob = simy[ipred, ], replace = TRUE))
            }
            res$pv <- rbind(res$pv, pv)
            low <- up <- NULL
            for (k in 1:length(ci)) {
                for (icat in 1:(dim(b)[3] + 1)) {
                    res$plower[iscen, icat, k] <- rbind(low, quantile(pv[, 
                                                                         icat], probs = (1 - ci[k])/2))
                    res$pupper[iscen, icat, k] <- rbind(up, quantile(pv[, 
                                                                        icat], probs = (1 - (1 - ci[k])/2)))
                }
            }
        }
    }
    res
}

###############################################################################
## END OF SCRIPT
###############################################################################