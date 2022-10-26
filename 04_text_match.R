library("readr") ## to read in the data 
library("dplyr") ## for merging data tables
library("koRpus") ## for merging data tables
library("koRpus.lang.en") ## needed if using R 4.0 or greater

source("stylometry_v3.R")

second_stage_prob <- function(data, true_row, bag_words = TRUE, stop_words = TRUE, stylometry = TRUE, punctuation = TRUE) {
## data is a vector of text reviews, one for each customer/review
## true_row is the text sample of the target that the vendor has in their proprietary data
	
	df <- data.frame()
	for (i in 1:length(data)) {
		rvw <- iconv(data[i], "UTF-8", "ASCII", sub = "")
		df <- stylometry(i,rvw, df, bag_words = bag_words, stop_words = stop_words, stylometry  = stylometry , punctuation = punctuation)
	}
	rvw <- iconv(true_row, "UTF-8", "ASCII", sub = "")
	df <- stylometry("true_row",rvw, df, bag_words = bag_words, stop_words = stop_words, stylometry  = stylometry , punctuation = punctuation)
	df[is.na(df)] <- 0

	## Remove very sparse variables only if there are at least 20 documents
	if (nrow(df) >= 20) {
	  	colRem <- NULL
	  	for (j in 2:ncol(df)) {
	  		if (sum(sign(df[,j])) < 5)
	  			colRem <- c(colRem, j)
	  		#cat(j)
	  	}#;cat('\n')   
	  	df <- df[,-1*colRem]
	}

	df_scaled <- df
	df_scaled[,-1] <- scale(df[,-1], center=TRUE, scale=TRUE)
	df_scaled[is.na(df_scaled)] <- 0
	
	tr <- df_scaled[nrow(df_scaled),-1]
	dmat_row <- apply(df_scaled[-1*nrow(df_scaled),-1], 1, function(m) (tr - m)^2)
	if (is.list(dmat_row)) {
	  dmat_row <- matrix(unlist(dmat_row), length(dmat_row), ncol(df_scaled)-1, byrow = TRUE)
	}

	d <- sqrt(colSums(dmat_row))
	dinv <- 1/d
	dinv[dinv == Inf] <- 1e8
	dist_prob <- dinv / sum(dinv, na.rm=T)
	# dist_prob[dist_prob == 0] <- NA

	dfeatures <- dmat_row[,which.max(dist_prob)]
	

#	list(prob = prob, top100features = colnames(df)[1+order(dfeatures, decreasing = FALSE)[1:100]])
	list(prob = dist_prob, topfeatures = colnames(df)[1+which(dfeatures == min(dfeatures[df[nrow(df),-1] != 0]) & df[nrow(df),-1] != 0)])
}

#data <- read_csv("../../outputs/Protection/1letter10km2yrs/None/MJ7possibilities1protected.csv")
#targets <- read_csv("../../outputs/Protection/targets.csv")

## just do the 5st row in the targets files (MJ) for illustration
#prob_allvars <- second_stage_prob(data$Text, targets$Text[5], bag_words = TRUE, stop_words = TRUE, stylometry = TRUE, punctuation = TRUE)
# prob_bagofwords <- second_stage_prob(data$Text, targets$Text[5], bag_words = TRUE, stop_words = FALSE, stylometry = FALSE, punctuation = FALSE)
# prob_stopwords <- second_stage_prob(data$Text, targets$Text[5], bag_words = FALSE, stop_words = TRUE, stylometry = FALSE, punctuation = FALSE)
# prob_stylometry <- second_stage_prob(data$Text, targets$Text[5], bag_words = FALSE, stop_words = FALSE, stylometry = T, punctuation = FALSE)
# prob_punctuation <- second_stage_prob(data$Text, targets$Text[5], bag_words = FALSE, stop_words = FALSE, stylometry = FALSE, punctuation = TRUE)

#print(prob_allvars)
#print(prob_allvars$prob)
#print(prob_allvars$topfeatures)

# print(prob_bagofwords)
# print(prob_bagofwords$prob)
# print(prob_bagofwords$topfeatures)
# 
# print(prob_stopwords)
# print(prob_stopwords$prob)
# print(prob_stopwords$topfeatures)
# 
# print(prob_stylometry)
# print(prob_stylometry$prob)
# print(prob_stylometry$topfeatures)
# 
# print(prob_punctuation)
# print(prob_punctuation$prob)
# print(prob_punctuation$topfeatures)
