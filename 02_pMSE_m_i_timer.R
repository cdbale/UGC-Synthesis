
################################################################################################################
################################################################################################################
################################################################################################################

library(tidyverse)
library(gridExtra)

### calculate pMSE using model with main effects and interactions ###

### Steps: ###
###   - create combined dataset C: original + synthetic + indicator for synthetic observations
###   - fit logistic regression models on C
###   - calculate pMSE for in-sample predictions for which observations in C are synthetic
###   - calculate ratio of pMSE to its null expectation (the closer to 1 the better)

### Snoke et. al (2018) model propensity scores with logistic regression using all main effects
### and first-order interactions, but omitting quadratic terms.
### We use a logistic regression model with the following:
###     (1) main effects + interaction between synthetic and all other variables

### PCA on Text Variables ###

PCA <- function(text_data, number_components){
  
  # PCA model - currently not scaling the textual variables
  pc <- prcomp(x = Xs, center = TRUE, scale = FALSE)
  
  pc_X <- pc$x[,1:number_components]
  
  return(pc_X)
  
}


pcd <- as.data.frame(PCA(X, 10))

####################################################################################
######################## GLM pMSE ratios for PCA data ##############################
####################################################################################

# pMSE ratios for state only synthesis using model with main effects and synthetic interactions
r_state <- pMSE_for_sequence(origdata, Y_syn_conf, Z, pcd, 'm + i')
r_state_avg <- sapply(r_state, mean)

# pMSE ratios for full synthesis using model with main effects and synthetic interactions
r_full <- pMSE_for_sequence(origdata, Y_syn_priv, Z_syn, pcd, 'm + i')
r_full_avg <- sapply(r_full, mean)

####################################################################################
######################## CART pMSE ratios for PCA data #############################
####################################################################################

# pMSE ratios for state only synthesis using model with main effects and synthetic interactions
r_state_c <- pMSE_for_sequence(origdata, CART_Y_conf[b_cp_i], Z, pcd, 'm + i')
r_state_c_avg <- sapply(r_state_c, mean)

# pMSE ratios for full synthesis using model with main effects and synthetic interactions
r_full_c <- pMSE_for_sequence(origdata, CART_Y_priv[b_cp_i], CART_Z[b_cp_i], pcd, 'm + i')
r_full_c_avg <- sapply(r_full_c, mean)

pdf(paste("output/",today,"/","pMSE_ratios_main_interactions_pca.pdf",sep=""))

par(mfrow=c(2,2))

# plot pMSE ratio for all lambdas for state and full synthesis (main effects + synthetic interactions)
plot(x = sml,
     y = r_state_avg,
     type = "l",
     main = "GLM",
     lty = 1,
     col = cbPalette[1],
     xlab = "Lambda",
     ylab = "pMSE Ratio",
     xlim = c(0,0.05),
     ylim = c(0,5))

# plot pMSE ratio based on main effects model for full synthesis
lines(x = sml, y = r_full_avg, col = cbPalette[2])
abline(h = 1)
legend("topleft","(x,y)",c("State Only","State and Rating"), lty=c(1,1,2,2), col=cbPalette[1:2])

# plot pMSE ratio for all lambdas for state and full synthesis (main effects + synthetic interactions)
plot(x = b_cp,
     y = r_state_c_avg,
     type = "l",
     main = "CART",
     lty = 1,
     col = cbPalette[1],
     xlab = "Alpha",
     ylab = "pMSE Ratio",
     ylim = c(0,5),
     xlim = c(0,0.02))

# add line for number of non-zero params for rating ~ text model
lines(x = b_cp, y = r_full_c_avg, col = cbPalette[2])
abline(h = 1)
legend("topleft","(x,y)",c("State Only","State and Rating"), lty=c(1,1,2,2), col=cbPalette[1:2])

dev.off()
####################################################
####################################################

## Running multiple simulations for the random projection
nsim <- 20
r_state_avg_mat <- NULL
r_full_avg_mat <- NULL
r_state_c_mat <- NULL
r_full_c_mat <- NULL
for (sim in 1:nsim){
  
  # perform random projection
  rp <- as.data.frame(RP(Xs, 10))
  
  ####################### GLM ##################
  ##############################################
  
  # pMSE ratios for state only synthesis using model with main effects
  r_state <- pMSE_for_sequence(origdata, Y_syn_conf, Zs, rp, 'm + i')
  r_state_avg <- sapply(r_state, mean)
  r_state_avg_mat <- rbind(r_state_avg_mat, r_state_avg)
  
  # pMSE ratios for full synthesis using model with main effects
  r_full <- pMSE_for_sequence(origdata, Y_syn_priv, Z_syn, rp, 'm + i')
  r_full_avg <- sapply(r_full, mean)
  r_full_avg_mat <- rbind(r_full_avg_mat, r_full_avg)
  
  ####################### CART #####################
  ##################################################
  
  # pMSE ratios for state only synthesis using model with main effects
  r_state_c <- pMSE_for_sequence(origdata, CART_Y_conf[b_cp_i], Z, rp, 'm + i')
  r_state_c_avg <- sapply(r_state_c, mean)
  r_state_c_mat <- rbind(r_state_c_mat, r_state_c_avg)
  
  # pMSE ratios for full synthesis using model with main effects
  r_full_c <- pMSE_for_sequence(origdata, CART_Y_priv[b_cp_i], CART_Z[b_cp_i], rp, 'm + i')
  r_full_c_avg <- sapply(r_full_c, mean)
  r_full_c_mat <- rbind(r_full_c_mat, r_full_c_avg)
}

####################################################
####################################################

pdf(paste("output/",today,"/","pMSE_ratios_main_interactions_rp.pdf",sep=""))

#par(mfrow=c(2,2))

# plot pMSE ratio for all lambdas for state and full synthesis (main effects only)
# again, lambdas are given in decreasing magnitude, and ratios map correspondingly
# the plotting function automatically reverses the order
# plot(x = sml,
#      y = apply(r_state_avg_mat, 2, mean),
#      type = "l",
#      main = "GLM",
#      lty = 1,
#      col = cbPalette[1],
#      xlab = "Lambda",
#      ylab = "pMSE Ratio",
#      xlim = c(0,0.05),
#      ylim = c(0,5))
# 
qs_state_glm <- apply(r_state_avg_mat, 2, quantile, c(0.025, 0.975))
qs_full_glm <- apply(r_full_avg_mat, 2, quantile, c(0.025, 0.975))
# # plot pMSE ratio based on main effects model for full synthesis
# lines(x = sml, y = apply(r_full_avg_mat, 2, mean), col = cbPalette[2])
# abline(h = 1)
# # polygon(c(sml,rev(sml)), c(qs_state[1,], rev(qs_state[2,])), border = NA,
# #         col = "grey86")
# # lines(x = sml, y = apply(r_state_avg_mat, 2, mean), col = cbPalette[1], lwd=2)
# # polygon(c(sml,rev(sml)), c(qs_full[1,], rev(qs_full[2,])),
# #         col = cbPalette[2],
# #         density = 1, angle = 45)
# legend("topleft","(x,y)",c("State Only","State and Rating"), lty=c(1,1,2,2), col=cbPalette[1:2])

rp_glm <- tibble(
  l = sml,
  ms = apply(r_state_avg_mat, 2, mean),
  mf = apply(r_full_avg_mat, 2, mean),
  ups = qs_state_glm[2,],
  upf = qs_full_glm[2,],
  lows = qs_state_glm[1,],
  lowf = qs_full_glm[1,]
)

colors <- c("State Only" = cbPalette[1], "State and Rating" = cbPalette[2])
glm_plot <- ggplot(rp_glm, aes(x = l, y = ms)) +
  geom_point(aes(color = "State Only"), size = 2) +
  geom_point(aes(x = l, y = mf, color = "State and Rating"), size = 2) +
  geom_errorbar(aes(ymax = ups, ymin = lows, color = "State Only")) +
  geom_errorbar(aes(ymax = upf, ymin = lowf, color = "State and Rating")) +
  geom_hline(yintercept = 1) +
  labs(x = "\nLambda",
       y = "pMSE Ratio\n",
       color = "",
       title = "GLM") +
  scale_color_manual(values = colors) +
  theme_bw(base_family = "") +
  theme(legend.position = c(0.27,.82),
        plot.title = element_text(hjust = 0.5)) +
  ylim(c(0, 5)) +
  xlim(c(0, 0.05))

####################################################################################
######################## CART pMSE ratios for RP data ##############################
####################################################################################

# # plot pMSE ratio for all lambdas for state and full synthesis (main effects only)
# plot(x = b_cp,
#      y = r_state_c_avg,
#      type = "l",
#      main = "CART",
#      lty = 1,
#      col = cbPalette[1],
#      xlab = "CP",
#      ylab = "pMSE Ratio",
#      ylim = c(0,5),
#      xlim = c(0,0.02))
# 
qs_state_cart <- apply(r_state_c_mat, 2, quantile, c(0.025, 0.975))
qs_full_cart <- apply(r_full_c_mat, 2, quantile, c(0.025, 0.975))
# # plot pMSE ratio based on main effects model for full synthesis
# lines(x = b_cp, y = apply(r_full_c_mat, 2, mean), col = cbPalette[2])
# abline(h = 1)
# # polygon(c(b_cp,rev(b_cp)), c(qs_state[1,], rev(qs_state[2,])),
# #         col = cbPalette[1],
# #         density = 0.01, angle = 45)
# # polygon(c(b_cp,rev(b_cp)), c(qs_full[1,], rev(qs_full[2,])),
# #         col = cbPalette[2],
# #         density = 0.01, angle = 45)
# legend("topleft","(x,y)",c("State Only","State and Rating"), lty=c(1,1,2,2), col=cbPalette[1:2])

rp_cart <- tibble(
  l = b_cp,
  ms = apply(r_state_c_mat, 2, mean),
  mf = apply(r_full_c_mat, 2, mean),
  ups = qs_state_cart[2,],
  upf = qs_full_cart[2,],
  lows = qs_state_cart[1,],
  lowf = qs_full_cart[1,]
)

colors <- c("State Only" = cbPalette[1], "State and Rating" = cbPalette[2])
cart_plot <- ggplot(rp_cart, aes(x = l, y = ms)) +
  geom_point(aes(color = "State Only"), size = 2) +
  geom_point(aes(x = l, y = mf, color = "State and Rating"), size = 2) +
  geom_errorbar(aes(ymax = ups, ymin = lows, color = "State Only")) +
  geom_errorbar(aes(ymax = upf, ymin = lowf, color = "State and Rating")) +
  geom_hline(yintercept = 1) +
  labs(x = "\nAlpha",
       y = "pMSE Ratio\n",
       color = "",
       title = "CART") +
  scale_color_manual(values = colors) +
  theme_bw(base_family = "") +
  theme(legend.position = c(0.27,.82),
        plot.title = element_text(hjust = 0.5)) +
  ylim(c(0, 5)) +
  xlim(c(0, 0.02))

grid.arrange(glm_plot, cart_plot, nrow = 2, ncol = 2)
dev.off()

####################################################
####################################################

# now, we need to select the lambda and cp values we would like to use
# moving forward.

# let's use the lambda values for which the difference in observed pMSE
# ratio from 1 is the smallest for full synthesis

# we will do this using the ratios for the main effects + interactions model

# smallest difference in ratios for full synthesis
r_full_avg <- apply(r_full_avg_mat, 2, mean)
diff_b <- order(abs(1 - r_full_avg))[1]

LAMBDA <- c(sml[length(sml)], sml[diff_b], sml[1])

####################################################
####################################################

# now we will combine the synthetic datasets for input into
# the other files

sYp <- Y_syn_priv[which(sml %in% LAMBDA)]
sYc <- Y_syn_conf[which(sml %in% LAMBDA)]
sZ <- Z_syn[which(sml %in% LAMBDA)]

syndata_Z_Y <- syndata_Y <- list()
for (i in 1:length(LAMBDA)){
  syndata_Z_Y[[i]] <- syndata_Y[[i]] <- list()
  
  for (j in 1:m){

    syndata_Z_Y[[i]][[j]] <- cbind(sZ[[i]][,j], sYp[[i]][,j])
    syndata_Y[[i]][[j]] <- cbind(Z, sYc[[i]][,j])
    
  }
}

syndata_Z_Y <- rev(syndata_Z_Y)
syndata_Y <- rev(syndata_Y)
####################################################
####################################################

# now, we need to select the cp values we would like to use
# moving forward.

# we will use the same selection process as for lambda

# smallest difference
r_full_c_avg <- apply(r_full_c_mat, 2, mean)
diff_c <- order(abs(1 - r_full_c_avg))[1]

CART_CP <- c(b_cp[1], b_cp[diff_c], b_cp[length(b_cp)])

#CART_MB <- c(diff_c, floor(new_nmb/2), new_nmb)

####################################################
####################################################

# now we will combine the synthetic datasets for input into
# the other files

sYp <- CART_Y_priv[which(CP %in% CART_CP)]
sYc <- CART_Y_conf[which(CP %in% CART_CP)]
sZ <- CART_Z[which(CP %in% CART_CP)]

CART_Z_Y <- CART_Y <- list()
for (i in 1:length(CART_CP)){
  CART_Z_Y[[i]] <- CART_Y[[i]] <- list()
  
  for (j in 1:m){
    
    CART_Z_Y[[i]][[j]] <- cbind(sZ[[i]][,j], sYp[[i]][,j])
    CART_Y[[i]][[j]] <- cbind(Z, sYc[[i]][,j])
    
  }
}

