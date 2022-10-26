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

# dataframes for use in CART synthesizers
dX <- as.data.frame(X)
dZX <- as.data.frame(cbind(Z,X))
names(dZX)[1] <- "Rating"

# sequence of minbucket values
#mb <- 1:nmb
# sequence of complexity parameters
CP <- exp(seq(log(1e-8), log(0.09), length.out = num_CP))

# loop over all minbucket values
for (i in 1:num_CP){
  
  #### Model 1: Synthesize State (Y) using CART model on confidential rating and text c(Z,X)
  cart1 <- run_cart(y = Y, x = dZX, xp = dZX, num_synth = m, type = "Partial", complexity_param = CP[i])
  ns_s <- c(ns_s, unname(tail(cart1$fit$cptable[,"nsplit"],1)))
  cart_srt_fit_times[i] <- cart1$ft
  cart_srt_sample_times_conf[i] <- cart1$st
  
  ## store synthetic state based on confidential rating and text
  CART_Y_conf[[i]] <- cart1$res
  
  #names(CART_Y_conf[[i]]) <- rep("State", m)
  
  #### Model 2: Synthesize Rating(Z) and State(Y) sequentially
  
  ## Synthesize Rating(Z) first using only the text
  cart2_1 <- run_cart(y = factor(Z), x = dX, xp = dX, num_synth = m, type = "Partial", complexity_param = CP[i])
  ns_r <- c(ns_r, unname(tail(cart2_1$fit$cptable[,"nsplit"],1)))
  cart_rt_fit_times[i] <- cart2_1$ft
  cart_rt_sample_times[i] <- cart2_1$st
  
  # store synthetic rating
  CART_Z[[i]] <- cart2_1$res
  #names(CART_Z[[i]]) <- rep("Rating", m)
  
  ## Synthesize State(Y) using CART fit to confidential rating and text, synthesized from 
  ## synthetic Rating(Z) and text
  #dsRX <- as.data.frame(cbind(CART_Z[[i]],X))
  cart2_2 <- run_cart(y = Y, x = dZX, xp = list(CART_Z[[i]], X), num_synth = m, type = "Full", complexity_param = CP[i])
  cart_srt_sample_times_priv[i] <- cart2_2$st
  
  ## store synthetic state
  CART_Y_priv[[i]] <- cart2_2$res
  
  print(paste0("CART Models for complexity parameter ", i, " out of ", length(CP), " completed."))
}

### CART Model
# number of splits for state ~ rating + text model
plot(x = CP, 
     y = ns_s, 
     type = "l", 
     main = "CART Model", 
     lty = 1,
     col = cbPalette[1],
     xlab = "CP",
     ylab = "Split Count",
     ylim = c(0, 110))

# add line for number of splits for rating ~ text model
lines(x = CP, y = ns_r, col = cbPalette[2])
legend("topright","(x,y)",c("State Model","Rating Model"), lty=c(1,1,2,2), col=cbPalette[1:2])

# based on visual analysis, we will restrict our utility (pMSE) evaluation to CART models with CP < 0.08

##################################################################################
b_cp_i <- CP < 0.02
b_cp <- CP[b_cp_i]

# number of remaining lambda values
length(b_cp)

pdf(paste("output/",today,"/","param_split_plots.pdf",sep=""))

par(mfrow=c(2,2))

### GLM Model
# number of non-zero params for state ~ rating + text model
# this is the number of variables with a non-zero coefficient for any class
plot(x = state_model$lambda, 
     y = state_model$df, 
     type = "l", 
     main = "GLM", 
     lty = 1,
     col = cbPalette[1],
     xlab = "Lambda",
     ylab = "Non-Zero Parameter Count")

# add line for number of non-zero params for rating ~ text model
lines(x = rating_model$lambda, y = rating_model$df, col = cbPalette[2])
legend("topright","(x,y)",c("State Model","Rating Model"), lty=c(1,1,2,2), col=cbPalette[1:2])

### CART Model
# number of splits for state ~ rating + text model
plot(x = CP, 
     y = ns_s, 
     type = "l", 
     main = "CART", 
     lty = 1,
     col = cbPalette[1],
     xlab = "Alpha",
     ylab = "Split Count",
     ylim = c(0, 110))

# add line for number of non-zero params for rating ~ text model
lines(x = CP, y = ns_r, col = cbPalette[2])
legend("topright","(x,y)",c("State Model","Rating Model"), lty=c(1,1,2,2), col=cbPalette[1:2])

dev.off()

# save the model fit and sampling times
c_times <- data.frame(
  rating_fit = cart_rt_fit_times,
  state_fit = cart_srt_fit_times,
  rating_sample = cart_rt_sample_times,
  state_sample_conf = cart_srt_sample_times_conf,
  state_sample_priv = cart_srt_sample_times_priv
)

write.csv(c_times, paste0("output/CART ", today, "/CART_model_times.csv"), row.names = FALSE)
