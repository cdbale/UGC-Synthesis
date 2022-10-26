############################################################
######  Generate Synthetic Data Using CART Model ###########
############################################################

# directory for CART output
dir.create(paste("output/","CART ",today,sep=""))

#####################
#### Synthesizers  ##
#####################

# We will run synthesizers for a sequence of Complexity Parameter values

# lists to store synthetic data
CART_Y_conf <- CART_Y_priv <- CART_Z <- list()

### vectors for run times when synthesizing rating ###
cart_rt_fit_times <- c()
cart_rt_sample_times <- c()

### vectors for run times when synthesizing state ###
cart_srt_fit_times <- c()
cart_srt_sample_times_conf <- c()
cart_srt_sample_times_priv <- c()

## vectors to store number of splits in tree
ns_r <- c()
ns_s <- c()



## under-sample and oversample for computational reasons
samples=c(200,300,500,821,1000,2000,5000,10000,50000)
m=1

for (i in 1:length(samples)){
  
  set.seed(1111)
  draws=sample(1:length(Y),samples[i],replace=TRUE)
  
  # dataframes for use in CART synthesizers
  dX <- as.data.frame(X)
  dZX <- as.data.frame(cbind(Z,X))
  names(dZX)[1] <- "Rating"
      
    dX=dX[draws,]
    dZX=dZX[draws,]
    Ys=Y[draws]
    Zs=Z[draws]
    Xs=X[draws,]
    complexity=.02
   
    


# sequence of minbucket values
#mb <- 1:nmb
# sequence of complexity parameters
#CP <- exp(seq(log(1e-8), log(0.09), length.out = num_CP))

# loop over all minbucket values
#for (i in 1:num_CP){
  
  #### Model 1: Synthesize State (Y) using CART model on confidential rating and text c(Z,X)
  cart1 <- run_cart(y = Ys, x = dZX, xp = dZX, num_synth = m, type = "Partial", complexity_param = complexity)
  ns_s <- c(ns_s, unname(tail(cart1$fit$cptable[,"nsplit"],1)))
  cart_srt_fit_times[i] <- cart1$ft
  cart_srt_sample_times_conf[i] <- cart1$st
  
  ## store synthetic state based on confidential rating and text
  CART_Y_conf[[i]] <- cart1$res
  
  #names(CART_Y_conf[[i]]) <- rep("State", m)
  
  #### Model 2: Synthesize Rating(Z) and State(Y) sequentially
  
  ## Synthesize Rating(Z) first using only the text
  cart2_1 <- run_cart(y = factor(Zs), x = dX, xp = dX, num_synth = m, type = "Partial", complexity_param = complexity)
  ns_r <- c(ns_r, unname(tail(cart2_1$fit$cptable[,"nsplit"],1)))
  cart_rt_fit_times[i] <- cart2_1$ft
  cart_rt_sample_times[i] <- cart2_1$st
  
  # store synthetic rating
  CART_Z[[i]] <- cart2_1$res
  #names(CART_Z[[i]]) <- rep("Rating", m)
  
  ## Synthesize State(Y) using CART fit to confidential rating and text, synthesized from 
  ## synthetic Rating(Z) and text
  #dsRX <- as.data.frame(cbind(CART_Z[[i]],X))
  cart2_2 <- run_cart(y = Ys, x = dZX, xp = list(CART_Z[[i]], X), num_synth = m, type = "Full", complexity_param = complexity)
  cart_srt_sample_times_priv[i] <- cart2_2$st
  
  ## store synthetic state
  CART_Y_priv[[i]] <- cart2_2$res
  

}




# save the model fit and sampling times
c_times <- data.frame(
  rating_fit = cart_rt_fit_times,
  state_fit = cart_srt_fit_times,
  rating_sample = cart_rt_sample_times,
  state_sample_conf = cart_srt_sample_times_conf,
  state_sample_priv = cart_srt_sample_times_priv
)

write.csv(c_times, paste0("output/CART ", today, "/CART_model_times.csv"), row.names = FALSE)
