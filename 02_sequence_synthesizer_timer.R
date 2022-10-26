############################################################
######  Generate Synthetic Data Using Proposed Model #######
############################################################

library(glmnet)

# folder to store later results
today <- Sys.Date()
today=format(today, format="%B %d %Y")
dir.create(paste("output/",today,sep=""))

# number of reviews and number of textual features
dim(X)

# Z = rating
Z=data2$Rating
# Y = state
Y=factor(data2$State)

# combine rating and state
origdata=cbind(Z,Y)

samples=c(200,300,500,821,1000,2000,5000,10000,50000)
m=1

### look at marginal and joint distributions of oversampled state/rating
draws=sample(1:length(Y),50000,replace=TRUE)
Zs=Z[draws]
Xs=X[draws,]
Ys=Y[draws]

mean(abs(table(Z)/sum(table(Z)) - table(Zs)/sum(table(Zs))))

mean(abs(table(Y)/sum(table(Y)) - table(Ys)/sum(table(Ys))))

mean(abs(table(Z, Y)/sum(table(Z, Y)) - table(Zs, Ys)/sum(table(Zs, Ys))))

for (i in 1:length(samples)){
  print(i)
  set.seed(1111)
  draws=sample(1:length(Y),samples[i],replace=TRUE)
  Zs=Z[draws]
  Xs=X[draws,]
  Ys=Y[draws]





#####################
#### Synthesizers  ##
#####################

# add_variable -----> "00_Helper_Functions.R"
# combine rating and text data
r_X <- add_variable(new_var = Zs, new_var_name = "Rating", old_data = Xs)

# model 2: State ~ Rating + Text
state_fit_time <- system.time(state_model <- glmnet(x = r_X, 
                                                    y = Ys, 
                                                    family="multinomial", 
                                                    nlambda = nl, 
                                                    lambda.min.ratio = l_ratio, 
                                                    intercept=FALSE))[3]

# model 1: Rating ~ Text
# use the sequence of lambda values utilized in model 2 - the model for the final synthesis step
rating_fit_time <- system.time(rating_model <- glmnet(x = Xs, 
                                                      y = factor(Zs), 
                                                      family="multinomial", 
                                                      lambda = state_model$lambda, 
                                                      intercept=FALSE))[3]

########################### Plot number of nonzero parameters #####################
# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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

### based on visual evaluation, we will synthesize for lambda < 0.05 and further
### assess data utility and disclosure risk
b_lmda <- state_model$lambda < 0.05
sml <- state_model$lambda[b_lmda]

# number of remaining lambda values
length(sml)

########################### State only Synthesis ##############################

# Generate probabilities for each state based on confidential rating and text
# this is predicted probabilities of each state in a n x N_State x N_lambda array
t1 <- system.time(synprob_state_conf <- predict(state_model, newx = r_X, s = sml, type="response"))[3]

# Generate Synthetic State based on Synthetic Probabilities, confidential rating and text
# MN_draws -----> "00_Helper_Functions.R"

# create m synthetic versions of state (columns) for each value of lambda (list index)
t2 <- system.time(Y_syn_conf <- apply(synprob_state_conf, 3, MN_draws, m, simplify = FALSE))[3]

# store total sampling time for state based on confidential data
state_sample_time_conf <- t1 + t2

########################## Full sequential Synthesis ##########################

# Generate synthetic rating based on the text
t1 <- system.time(synprob_rating <- predict(rating_model, newx = Xs, s = sml, type="response"))[3]
t2 <- system.time(Z_syn <- apply(synprob_rating, 3, MN_draws, m, simplify = FALSE))[3]
rating_sample_time <- t1 + t2

## Generate synthetic state based on synthetic rating and text
## These data take longer to generate since we have to select each
## synthetic rating variable for each lambda and feed it with the text
## into the predict function
draw_state_priv <- function(){
  Y_syn_priv <- list()
  t <- 0
  for (l in 1:length(sml)){
    t1 <- system.time(synprob_state_priv <- lapply(1:m, function(x) predict(state_model, newx=add_variable(Z_syn[[l]][,x], "Rating", Xs), s=sml[l], type="response")[,,1]))[3]
    t2 <- system.time(Y_syn_priv[[l]] <- sapply(synprob_state_priv, function(x) MN_draws(x, 1)))[3]
    t <- t + t1 + t2
  }
  return(list(Ysyn = Y_syn_priv, t = t))
}

# run above function
Y_t <- draw_state_priv()
# store synthetic Y data
Y_syn_priv <- Y_t$Ysyn
# store sampling time 
state_sample_time_priv <- Y_t$t

# save the fit and sampling times for the GLM models
b_times <- data.frame(
  model = c("Rating","State_conf","State_priv"),
  fit = c(rating_fit_time, state_fit_time, NA),
  sampling = c(rating_sample_time, state_sample_time_conf, state_sample_time_priv)
)
write.csv(b_times, paste0("output/", today, "/proposed_model_times",i,".csv"), row.names = FALSE)

}
