second_stage_prob_new <- function(df1, df2) {
  df <- suppressMessages(full_join(df1,df2))
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
  
  # tr <- df_scaled[nrow(df_scaled),-1]
  # dmat_row <- apply(df_scaled[-1*nrow(df_scaled),-1], 1, function(m) (tr - m)^2)
  # if (is.list(dmat_row)) {
  #   dmat_row <- matrix(unlist(dmat_row), length(dmat_row), ncol(df_scaled)-1, byrow = TRUE)
  # }
  # d <- sqrt(colSums(dmat_row))
  dmat <- as.matrix(dist(df_scaled[,-1]))
  d <- dmat[nrow(df_scaled),-1*nrow(df_scaled)]
  dinv <- 1/d
  dinv[dinv == Inf] <- 1e8
  # dist_prob_features <- dinv / sum(dinv, na.rm=T)
  # names(dist_prob_features) <- colnames(df)[-1]
  # dist_prob[dist_prob == 0] <- NA
  
  # d <- sqrt(rowSums(dmat_row))
  # dinv <- 1/d
  # dinv[dinv == Inf] <- 1e8
  dist_prob_docs <- dinv / sum(dinv, na.rm=T)
  names(dist_prob_docs) <- df$Name[-1*nrow(df)]
  
  list(prob_docs = dist_prob_docs)
  # list(prob = dist_prob, topfeatures = colnames(df)[1+which(dfeatures == min(dfeatures[df[nrow(df),-1] != 0]) & df[nrow(df),-1] != 0)])
}
