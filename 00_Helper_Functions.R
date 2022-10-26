#### This file defines various helper functions utilized in the other code files ####
#### These functions are organized based on the file in which they are first utilized #######

######################## ---- Helper Functions for "02_sequence_synthesizer.R" ---- ##########################

# add a named column variable to an existing matrix/dataframe or another column
add_variable <- function(new_var, new_var_name, old_data, old_data_names = colnames(old_data)){
  
  temp <- cbind(new_var, old_data)
  
  colnames(temp) <- c(new_var_name, old_data_names)
  
  return(temp)
  
}

# draws from multinomial distribution characterized by probabilities in each row of predicted probs
# returns vector of synthetic data
MN_draws <- function(predicted_probs, m){
  
  temp <- apply(predicted_probs, 1, function(x) apply(rmultinom(m,1,x), 2, function(y) which(y == 1)))
  
  if (m==1) return(as.matrix(temp)) else return(t(temp))
  
}

############ Helper Function for `02_sequence_CART.R` #########

#library(synthpop)
library(rpart)

###############################################################################
############# custom syn.cart function that tracks run time ###################
###############################################################################

run_cart <- function(y, x, xp, num_synth, type, b = 5, complexity_param = 1e-08){
  y <- factor(y)
  fit_time <- system.time(
    fit <- rpart::rpart(y ~ ., 
                        data = as.data.frame(cbind(y, x)), 
                        method = "class",
                        control = list(minbucket = b, cp = complexity_param)))[3]
  
  if (type == "Partial"){
    sample_time1 <- system.time(nodes <- predict(object = fit, newdata = xp))[3]
    sample_time2 <- system.time(new <- replicate(num_synth, expr = apply(nodes, MARGIN=1, FUN=function(s) gdata::resample(colnames(nodes), size=1, prob=s)), simplify = FALSE))[3]
    new <- as.data.frame(do.call(cbind, lapply(new, function(s) factor(s, levels = levels(y)))))
    st <- sample_time1 + sample_time2
  } else if (type == "Full"){
    new <- list()
    st <- 0
    for (n in 1:num_synth){
      sample_time1 <- system.time(nodes <- predict(object = fit, newdata = as.data.frame(add_variable(xp[[1]][,n], "Rating", xp[[2]]))))[3]
      sample_time2 <- system.time(new_2 <- apply(nodes, MARGIN = 1, FUN = function(s) gdata::resample(colnames(nodes), size = 1, prob = s)))[3]
      new[[n]] <- new_2
      st <- st + sample_time1 + sample_time2
    }
    new <- as.data.frame(do.call(cbind, lapply(new, function(s) factor(s, levels = levels(y)))))
  }
  
  return(list(res = new, fit = fit, ft = fit_time, st = st))
}

####################### ---- Helper functions for "02_pMSE_m.R" and "02_pMSE_m_i.R" ##################

### PCA on Text Variables ###

PCA <- function(text_data, number_components){
  
  # PCA model - currently not scaling the textual variables
  pc <- prcomp(x = X, center = TRUE, scale = FALSE)
  
  pc_X <- pc$x[,1:number_components]
  
  return(pc_X)
  
}

### Random projections approach to dimension reduction ###

RP <- function(text_data, number_of_dimensions){
  
  # random standard normal draws
  R <- matrix(rnorm(number_of_dimensions*ncol(text_data)), nrow = ncol(text_data), ncol = number_of_dimensions)
  
  # scale columns to unit length
  R <- apply(R, 2, function(x) x/sqrt(sum(x^2)))
  
  # project data
  rp_X <- text_data %*% R
  
  return(rp_X)
  
}

##### The following are helper functions used to calculate the pMSE ratio for #####
##### a sequence of lambda or min-bucket values ############################

# combine a synthetic dataset with the original data,
# create a synthetic dummy variable
pMSE_data_prep <- function(ds, sds, pca_data){
  
  # number of observations in the original data
  n <- nrow(pca_data)
  
  # reconstruct original data
  original_data <- cbind(ds,pca_data,rep(0,n))
  
  names(original_data)[c(1,2,ncol(original_data))] <- c('Rating','State','Synthetic')
  
  # synthetic dataset
  SDS <- cbind(sds,pca_data,rep(1,n))
  
  names(SDS)[c(1,2,ncol(SDS))] <- c('Rating','State','Synthetic')
  
  # combine original and synthetic data
  SDS <- rbind(SDS, original_data)
  SDS[,2] <- as.factor(SDS[,2])
  
  return(SDS)
  
}

# calculate the pMSE ratio for a given model type and dataset, also provide the names of synthetic variables
# 'm' specifies a model with main effects only
# 'm + i' specifies a model with main effects and interactions of synthetic variables with all other variables
calculate_pMSE <- function(ds, syn_names, model_type = 'm', maxit = 15000){
  
  N <- nrow(ds)
  
  # main effects model
  if (model_type == 'm'){
    
    model_formula <- as.formula('Synthetic ~ .')
    
    # main effects + synthetic interactions model
  } else if (model_type == 'm + i'){
    
    mf <- 'Synthetic ~ .'

    for (n in syn_names){

      mf <- paste0(mf, ' + ', n, ':.')

    }
    
    #model_formula <- as.formula('Synthetic ~ . + Rating:State')
    model_formula <- as.formula(mf)
    
    # error for incorrect model type
  } else {
    
    return("Incorrect model type specified: must be either 'm' for main effects model or 'm + i' for main effects and synthetic variable interactions.")
    
  }
  
  ## propensity score model
  psm <- glm(model_formula, data = ds, family = 'binomial', control = list(maxit = maxit))
  
  if (psm$converged == FALSE) {
    
    print(paste0("Warning: Logistic model did not converge in ", 
        maxit, " iterations. You should increase parameter 'maxit'."))
    
  }
  
  ## number of model features involving synthesized variables
  namescoef <- names(psm$coefficients)
  coefOK <- rep(FALSE, length(namescoef))
  for (nn in syn_names) coefOK[grepl(nn, namescoef)] <- TRUE
  k <- sum(coefOK)
  
  # predicted propensity scores
  preds <- predict(psm, newdata = ds[,-ncol(ds)], type = 'response')
  
  # calculate propensity score mean squared error
  pMSE <- 1/N * sum((preds - 0.5)^2)
  
  # expected null pMSE value given the number of features involving synthesized variables
  EpMSE <- (k-1)/(8*N)
  
  # pMSE ratio
  pMSE_ratio <- pMSE/EpMSE
  
  #return(list(pMSE_ratio, psm))
  return(pMSE_ratio)
  
}

## perform the above `calculate_pMSE` for a sequence of lambda or minbucket values
## syn_Y and syn_Z are dataframes corresponding to synthetic Y and Z values - each column
## contains the synthetic values for all observations for a different value of lambda or b
pMSE_for_sequence <- function(original_variables, syn_Y, syn_Z, pca_data, model_type){
  
  seq_len <- length(syn_Y)
  ratios <- list()
  
  if (!is.list(syn_Z)){
    
    # calculate pMSE ratios for state only synthesis
    for (l in 1:seq_len){
      
      lr <- c()
      
      for (i in 1:m){
        
        data_combined <- pMSE_data_prep(ds = original_variables, sds = add_variable(syn_Z, 'Rating', syn_Y[[l]][,i], 'State'), pca_data = pca_data)
        
        r <- calculate_pMSE(ds = data_combined, syn_names = c("State"), model_type = model_type)
        
        lr <- c(lr, r)
        
      }
      
      ratios[[l]] <- lr
      
    }
    
  } else {
    
    # calculate pMSE ratios for full synthesis
    for (l in 1:seq_len){
      
      lr <- c()
      
      for (i in 1:m) {
        
        data_combined <- pMSE_data_prep(ds = original_variables, sds = add_variable(syn_Z[[l]][,i], 'Rating', syn_Y[[l]][,i], 'State'), pca_data = pca_data)
        
        r <- calculate_pMSE(ds = data_combined, syn_names = c('Rating','State'), model_type = model_type)
        
        lr <- c(lr, r)
        
      }
      
      ratios[[l]] <- lr
    
    }
    
  }
  
  return(ratios)
  
}

###### The below function is not currently used ######

################################################################################
############# function to generate synthetic data for SSPPCA ###################
################################################################################

# syn_pois <- function(N, D, true_d){
#   
#   ####### Generate synthetic data #######
#   
#   ## v1 and E will be used in every synthetic X
#   # define feature v1 from poisson RV with lambda = 20
#   v1 <- rpois(N, 20)
#   
#   # construct error E from Poisson RV with lambda = 2, and 
#   # multiply by -1 or 1 with equal probability
#   E <- matrix(rpois(N*D, 2)*sample(c(-1,1), N*D, replace = T), N, D)
#   
#   ## generate synthetic X based on the desired true dimension
#   ## can take values of 1, 2, or 3
#   if (true_d == 1){
#     
#     # construct synthetic 1-dimensional data, 
#     # using v1 twice and 2*v1 eight times
#     X <- matrix(c(rep(v1, 2), rep(2*v1, D-2)), N, D) + E
#     
#   } else if (true_d == 2){
#     
#     # create a second feature v2
#     v2 <- rpois(N, 30)
#     
#     # construct synthetic 2-dimensional data, 
#     # using v1 twice, v2 twice, and v1 + 3*v2 D-4 times
#     X <- matrix(c(rep(v1, 2), rep(v2, 2), rep(v1 + 3*v2, D-4)), N, D) + E
#     
#   } else if (true_d == 3){
#     
#     # construct second and third features v2, v3
#     v2 <- rpois(N, 30)
#     v3 <- rpois(N, 50)
#     
#     # construct synthetic 3-dimensional data, 
#     # using v1 twice, v2 twice, v3 twice,
#     # and 3*v1 + 2*v2 + 2*v3 D-6 times
#     X <- matrix(c(rep(v1, 2), rep(v2, 2), rep(v3, 2), rep(3*v1 + 2*v2 + 2*v3, D-6)), N, D) + E
#     
#   }
#   
#   return (X)
#   
# }

###############################################################################################
############# joint posterior, objective, and gradient functions for SSPPCA ###################
###############################################################################################

####### Define log-posterior function #######

# lp <- function(par, X, a, nW, D, d, N){
#   
#   # reconstruct W and Y
#   W <- matrix(data = par[1:nW], nrow = D)
#   Y <- matrix(data = par[(nW + 1):length(par)], ncol = N)
#   
#   WY <- W%*%Y
#   
#   # objective function terms
#   o1 <- sum(diag(crossprod(X,WY)))
#   o2 <- sum(exp(WY))
#   o3 <- 0.5 * sum(diag(crossprod(Y)))
#   o4 <- 0.5 * sum(diag(crossprod(W)%*%diag(a, d, d)))
#   
#   return (o1 - o2 - o3 - o4)
#   
# }
# 
# ####### Define Objective function for optimization #######
# 
# objective <- function(par, X, W0, a, nW, D, d, N, dlta, k){
#   
#   # reconstruct W and Y
#   W <- matrix(data = par[1:nW], nrow = D)
#   Y <- matrix(data = par[(nW + 1):length(par)], ncol = N)
#   
#   WY <- W%*%Y
#   
#   # objective function terms
#   o1 <- sum(diag(crossprod(X,WY)))
#   o2 <- sum(exp(WY))
#   o3 <- 0.5 * sum(diag(crossprod(Y)))
#   o4 <- 0.5 * sum(diag(crossprod(W)%*%diag(a, d, d)))
#   o5 <- k * sum(W^2 / (W0^2 + dlta))
#   
#   return (o1 - o2 - o3 - o4 - o5)
#   
# }
# 
# ####### Define Gradient function for optimization #######
# 
# gradient <- function(par, X, W0, a, nW, D, d, N, dlta, k){
#   
#   # reconstruct W and Y
#   W <- matrix(data = par[1:nW], nrow = D)
#   Y <- matrix(data = par[(nW + 1):length(par)], ncol = N)
#   
#   eWY <- exp(W%*%Y)
#   
#   # gradients for W
#   dW1 <- tcrossprod(X,Y)
#   dW2 <- tcrossprod(eWY,Y)
#   dW3 <- W%*%diag(a, d, d)
#   dW4 <- 2*k*W / (W0^2 + dlta)
#   
#   dW <- dW1 - dW2 - dW3 - dW4
#   
#   # gradients for Y
#   dY1 <- crossprod(W,X)
#   dY2 <- crossprod(W,eWY)
#   
#   dY <- dY1 - dY2 - Y
#   
#   return (c(dW, dY))
#   
# }
