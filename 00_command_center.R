
#### Use this program to run all the other programs

xaxis <- c(-1,1)                  ## Set x-axis limits for utility analysis
m <- 20                        ## number of synthetic datasets
nl <- 100                       ## number of lambda values
l_ratio <- 0.001               ## min lambda ratio to max lambda
#nmb <- 215                   ## number of min-bucket values (minimum leaf size for CART)
num_CP <- 100                  ## number of complexity parameter values to test

K <- 5                               ## number of topics
iter <- 500                         ## number of E-M iterations, normally 250 iteration, greatly affects the slowness of `05_utility_*`

source("00_Helper_Functions.R") ## Load various helper (custom) functions

source("01_data_input.R")  ## Read in data and define 821 records in data2 and 9 targets in data_targets

#source("text_mRMRe.R") ## perform maximum relevance minimum redundancy feature selection - not currently used

source("02_sequence_synthesizer.R") ## Generate synthetic data syndata_Z_Y and syndata_Y for sequence of lambda values

source("02_sequence_CART.R") ## Generate synthetic data CART_Z_Y and CART_Y using Baseline CART model for sequence of minbucket values

#source("02_synthesizer.R") ## Generate synthetic data syndata_Z_Y and syndata_Y - not currently used

#source("02_CART_synthesizer.R") ## Generate synthetic data CART_Z_Y and CART_Y using Baseline CART model - not currently used

source("02_pMSE_m.R") ## calculate pMSE ratio from logistic regression with main effects only for both synthesis models

source("02_pMSE_m_i.R") ## calculate pMSE ratio for logistic regression with main effects and interactions for both synthesis models

source("03_risk_measures.R") ## Generate risk evaluation across m simulations in csv file in output folder

source("03_CART_risk_measures.R") ## Generate risk evaluation across m simulations in csv file in output folder

source("04_crosstabs_utility_comparison.R")  ## Generate utility across m simulation pdf and csv file

source("04_CART_crosstabs_utility_comparison.R")  ## Generate utility across m simulation pdf and csv file

source("05_utility_measures.R")  ## Analysis specific utility for lambdas

source("05_CART_utility_measures.R") ## Analysis specific utility for CART generated data
