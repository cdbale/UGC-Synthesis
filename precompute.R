source("precomputeTextMat.R") ## new function
source("second_stage_prob_new.R") ## new function

source("01_data_input.R")  ## Read in data and define 821 records in data2 and 9 targets in data_targets
source("04_text_match.R")  ## Defines the old function second_stage_prob and its helper function stylometry.

## the original version
start <- proc.time()
old <- second_stage_prob(data = data2$Text, true_row = data_targets$Text[1], bag_words = TRUE, stop_words = TRUE, punctuation = TRUE, stylometry = TRUE)
stop <- proc.time()

print(T*(stop[3] - start[3]))

## the new version
start <- proc.time()
df1 <- precomputeTextMat(data = data2$Text, bag_words = TRUE, stop_words = TRUE, punctuation = TRUE, stylometry = TRUE)
df2 <- precomputeTextMat(data = data_targets$Text, bag_words = TRUE, stop_words = TRUE, punctuation = TRUE, stylometry = TRUE)
new <- list()
for (t in 1:T) {
  new[[t]] <- second_stage_prob_new(df1, df2[t,])
}
stop <- proc.time()
print(stop[3] - start[3])

df_final <- data.frame()
for (t in 1:T) {
  df <- data.frame(prob_docs = new[[t]]$prob_docs, target = rep(t, length(new[[t]]$prob_docs)), docID = 1:length(new[[t]]$prob_docs))
  df_final <- rbind(df_final, df)
}
write.table(df_final, "./precomputed_probabilities_text/precomputed_probabilities_reviews_raw.csv", sep=",", row.names = FALSE, col.names = TRUE)

df_final <- data.frame()
for (t in 1:T) {
  df <- data.frame(prob_features = new[[t]]$prob_features, target = rep(t, length(new[[t]]$prob_features)), feature = names(new[[t]]$prob_features))
  df_final <- rbind(df_final, df)
}
write.table(df_final, "./precomputed_probabilities_text/precomputed_probabilities_features_raw.csv", sep=",", row.names = FALSE, col.names = TRUE)
