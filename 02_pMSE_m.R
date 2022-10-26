################################################################################################################
################################################################################################################
################################################################################################################

### calculate pMSE using main effects only model ###

### Steps: ###
###   - create combined dataset C: original + synthetic + indicator for synthetic observations
###   - fit logistic regression models on C
###   - calculate pMSE for in-sample predictions for which observations in C are synthetic
###   - calculate ratio of pMSE to its null expectation (the closer to 1 the better)

### Snoke et. al (2018) model propensity scores with logistic regression using all main effects
### and first-order interactions, but omitting quadratic terms.
### We use a logistic regression models with the following:
###     (1) main effects

pcd <- as.data.frame(PCA(X, number_components = 10))

####################################################################################
######################## GLM pMSE ratios for PCA data ##############################
####################################################################################

# pMSE ratios for state only synthesis using model with main effects
r_state <- pMSE_for_sequence(origdata, Y_syn_conf, Z, pcd, 'm')
r_state_avg <- sapply(r_state, mean)

#############
#############
#############

# pMSE ratios for full synthesis using model with main effects
r_full <- pMSE_for_sequence(origdata, Y_syn_priv, Z_syn, pcd, 'm')
r_full_avg <- sapply(r_full, mean)

#######################################################################
#######################################################################

####################################################################################
######################## CART pMSE ratios for PCA data #############################
####################################################################################

# pMSE ratios for state only synthesis using model with main effects
r_state_c <- pMSE_for_sequence(origdata, CART_Y_conf[b_cp_i], Z, pcd, 'm')
r_state_c_avg <- sapply(r_state_c, mean)

# pMSE ratios for full synthesis using model with main effects
r_full_c <- pMSE_for_sequence(origdata, CART_Y_priv[b_cp_i], CART_Z[b_cp_i], pcd, 'm')
r_full_c_avg <- sapply(r_full_c, mean)

pdf(paste("output/",today,"/","pMSE_ratios_main_pca.pdf",sep=""))

par(mfrow=c(2,2))
# plot pMSE ratio for all lambdas for state and full synthesis (main effects only)
# again, lambdas are given in decreasing magnitude, and ratios map correspondingly
# the plotting function automatically reverses the order
plot(x = sml,
     y = r_state_avg,
     type = "l",
     main = "GLM",
     lty = 1,
     col = cbPalette[1],
     xlab = "Lambda",
     ylab = "pMSE Ratio",
     xlim = c(0,0.05),
     ylim = c(0,35))

# plot pMSE ratio based on main effects model for full synthesis
lines(x = sml, y = r_full_avg, col = cbPalette[2])
abline(h = 1)
legend("topleft","(x,y)",c("State Only","State and Rating"), lty=c(1,1,2,2), col=cbPalette[1:2])

# plot pMSE ratio for all lambdas for state and full synthesis (main effects only)
plot(x = b_cp,
     y = r_state_c_avg,
     type = "l",
     main = "CART",
     lty = 1,
     col = cbPalette[1],
     xlab = "b",
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
        rp <- as.data.frame(RP(X, 10))
        
        ####################### GLM ##################
        ##############################################
        
        # pMSE ratios for state only synthesis using model with main effects
        r_state <- pMSE_for_sequence(origdata, Y_syn_conf, Z, rp, 'm')
        r_state_avg <- sapply(r_state, mean)
        r_state_avg_mat <- rbind(r_state_avg_mat, r_state_avg)
        
        # pMSE ratios for full synthesis using model with main effects
        r_full <- pMSE_for_sequence(origdata, Y_syn_priv, Z_syn, rp, 'm')
        r_full_avg <- sapply(r_full, mean)
        r_full_avg_mat <- rbind(r_full_avg_mat, r_full_avg)
        
        ####################### CART #####################
        ##################################################
        
        # pMSE ratios for state only synthesis using model with main effects
        r_state_c <- pMSE_for_sequence(origdata, CART_Y_conf[b_cp_i], Z, rp, 'm')
        r_state_c_avg <- sapply(r_state_c, mean)
        r_state_c_mat <- rbind(r_state_c_mat, r_state_c_avg)
        
        # pMSE ratios for full synthesis using model with main effects
        r_full_c <- pMSE_for_sequence(origdata, CART_Y_priv[b_cp_i], CART_Z[b_cp_i], rp, 'm')
        r_full_c_avg <- sapply(r_full_c, mean)
        r_full_c_mat <- rbind(r_full_c_mat, r_full_c_avg)
}

####################################################
####################################################

pdf(paste("output/",today,"/","pMSE_ratios_main_rp.pdf",sep=""))

par(mfrow=c(2,2))

# plot pMSE ratio for all lambdas for state and full synthesis (main effects only)
# again, lambdas are given in decreasing magnitude, and ratios map correspondingly
# the plotting function automatically reverses the order
plot(x = sml,
     y = apply(r_state_avg_mat, 2, mean),
     type = "l",
     main = "GLM",
     lty = 1,
     col = cbPalette[1],
     xlab = "Lambda",
     ylab = "pMSE Ratio",
     xlim = c(0,0.05),
     ylim = c(0,35))

qs_state <- apply(r_state_avg_mat, 2, quantile, c(0.025, 0.975))
qs_full <- apply(r_full_avg_mat, 2, quantile, c(0.025, 0.975))
# plot pMSE ratio based on main effects model for full synthesis
lines(x = sml, y = apply(r_full_avg_mat, 2, mean), col = cbPalette[2])
abline(h = 1)
# polygon(c(sml,rev(sml)), c(qs_state[1,], rev(qs_state[2,])),
#         col = cbPalette[1],
#         density = 0.01, angle = 45)
# polygon(c(sml,rev(sml)), c(qs_full[1,], rev(qs_full[2,])),
#                                          col = cbPalette[2],
#                                          density = 0.01, angle = 45)
legend("topleft","(x,y)",c("State Only","State and Rating"), lty=c(1,1,2,2), col=cbPalette[1:2])

####################################################################################
######################## CART pMSE ratios for RP data ##############################
####################################################################################

# plot pMSE ratio for all lambdas for state and full synthesis (main effects only)
plot(x = b_cp,
     y = r_state_c_avg,
     type = "l",
     main = "CART",
     lty = 1,
     col = cbPalette[1],
     xlab = "CP",
     ylab = "pMSE Ratio",
     ylim = c(0,5),
     xlim = c(0,0.02))

qs_state <- apply(r_state_c_mat, 2, quantile, c(0.025, 0.975))
qs_full <- apply(r_full_c_mat, 2, quantile, c(0.025, 0.975))
# plot pMSE ratio based on main effects model for full synthesis
lines(x = b_cp, y = apply(r_full_c_mat, 2, mean), col = cbPalette[2])
abline(h = 1)
# polygon(c(b_cp,rev(b_cp)), c(qs_state[1,], rev(qs_state[2,])),
#         col = cbPalette[1],
#         density = 0.01, angle = 45)
# polygon(c(b_cp,rev(b_cp)), c(qs_full[1,], rev(qs_full[2,])),
#         col = cbPalette[2],
#         density = 0.01, angle = 45)
legend("topleft","(x,y)",c("State Only","State and Rating"), lty=c(1,1,2,2), col=cbPalette[1:2])

dev.off()
