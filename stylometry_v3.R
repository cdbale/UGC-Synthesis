########################################### LIBRARIES AND FUNCTIONS ########################################### 
library("tm")
library("koRpus")
library("dplyr")
library("stringr") 

## Input:  path of a plain text file, dataframe to store results, boolean for whether to count frequency of all words, boolean to count frequency of stop words (e.g., of, and, the)
## Output: data frame with a single row of different style/content variables
stylometry = function(file_path, text_object, df, bag_words = FALSE, stop_words = FALSE, stylometry = FALSE, punctuation = FALSE) {
  counter <- nrow(df)
  df[counter + 1, "Name"] <- file_path
  counter <- nrow(df)

  tagged.text <- tokenize(text_object, lang = "en", format="obj")
  text_stat <- describe(tagged.text)
  
  tokenized.text <- taggedText(tagged.text)
  tb <- table(tokenized.text$token)
  
  if (punctuation) {
	  df[counter, "punctuation_numExclamations"] <-   max(0, as.numeric(tb[names(tb) == "!"]))
	  df[counter, "punctuation_numPeriods"] <- max(0, as.numeric(tb[names(tb) == "."]))
	  df[counter, "punctuation_numQuestionMarks"] <- max(0, as.numeric(tb[names(tb) == "?"]))
	  df[counter, "punctuation_numHashtags"] <- max(0, as.numeric(tb[names(tb) == "#"]))
	  df[counter, "punctuation_numAts"] <- max(0, as.numeric(tb[names(tb) == "@"]))
	  df[counter, "punctuation_numDollarSigns"] <-   max(0, as.numeric(tb[names(tb) == "$"]))
	  df[counter, "punctuation_numPercentageSigns"] <-   max(0, as.numeric(tb[names(tb) == "%"]))
	  df[counter, "punctuation_numAmpersands"] <-   max(0, as.numeric(tb[names(tb) == "&"]))
	  df[counter, "punctuation_numCommas"] <-   max(0, as.numeric(tb[names(tb) == ","]))
	  df[counter, "punctuation_numSemiColin"] <-   max(0, as.numeric(tb[names(tb) == ";"]))
	  df[counter, "punctuation_numColin"] <-   max(0, as.numeric(tb[names(tb) == ":"]))
	  df[counter, "punctuation_numParentheses"] <-   max(0, as.numeric(tb[names(tb) == "("]) + as.numeric(tb[names(tb) == ")"]))
	  df[counter, "punctuation_numWords"] <- text_stat$words
	  df[counter, "punctuation_numChars"] <- text_stat$all.chars
  }
  if (stylometry) {
	  df[counter, "freq_yules_k"] <- lex.div(tagged.text, measure = "K", quiet = TRUE, char = c())@K.ld
	  
  }  
  ## bag of words 
  if (bag_words | stop_words | stylometry) {
    text_lower <- tolower(text_object)	  
    tagged.text <- tokenize(text_lower, lang = "en", format="obj")
    tokenized.text <- taggedText(tagged.text)
    tb_lower <- table(tokenized.text$token)
    words <- tokenized.text$token[tokenized.text$tag == "word.kRp"]    
    stopwords=tm::stopwords("en")
  }

  if (bag_words) {
    if ( sum(!(words %in% stopwords)) > 0) {
      words_bag <- words[!(words %in% stopwords)]
      for (j in 1:length(words_bag)) {
        if (tb_lower[words_bag[j]] > 0) {
          df[counter,sprintf("bagofwords_%s",tolower(words_bag[j]))] <- tb_lower[words_bag[j]]
        }
      }
    }  
  }
  
  ## frequency of "function" words (stopwords)
  if (stop_words) {
    stopwords_count <- words[words %in% stopwords]
    for (j in 1:length(stopwords_count)) {
      df[counter,sprintf("stopwords_%s", stopwords_count[j])] <- tb_lower[stopwords_count[j]]
    }
  }
  
  if (stylometry) {
    wordDistn <- text_stat$lttr.distrib
    for (j in 1:ncol(wordDistn)) {
      df[counter, sprintf("freq_Wordlen%d",j)] <- wordDistn["num",j]
    }
  
    ## frequency of words with a mix of upper and lower case words 
    text_upper <- toupper(text_object)
    tagged.text <- tokenize(text_upper, lang = "en", format="obj")
    tokenized.text <- taggedText(tagged.text)
    tb_upper <- table(tokenized.text$token)
    
    df[counter, "freq_mixedCaseWords"] <- sum(tb[!(names(tb) %in% names(tb_lower) | names(tb) %in% names(tb_upper))])

    ## frequency of a to z, ignoring case
    df[counter, "freq_a"] <- sum(str_count(text_lower, "a"))
    df[counter, "freq_b"] <- sum(str_count(text_lower, "b"))
    df[counter, "freq_c"] <- sum(str_count(text_lower, "c"))
    df[counter, "freq_d"] <- sum(str_count(text_lower, "d"))
    df[counter, "freq_e"] <- sum(str_count(text_lower, "e"))
    df[counter, "freq_f"] <- sum(str_count(text_lower, "f"))
    df[counter, "freq_g"] <- sum(str_count(text_lower, "g"))
    df[counter, "freq_h"] <- sum(str_count(text_lower, "h"))
    df[counter, "freq_i"] <- sum(str_count(text_lower, "i"))
    df[counter, "freq_j"] <- sum(str_count(text_lower, "j"))
    df[counter, "freq_k"] <- sum(str_count(text_lower, "k"))
    df[counter, "freq_l"] <- sum(str_count(text_lower, "l"))
    df[counter, "freq_m"] <- sum(str_count(text_lower, "m"))
    df[counter, "freq_n"] <- sum(str_count(text_lower, "n"))
    df[counter, "freq_o"] <- sum(str_count(text_lower, "o"))
    df[counter, "freq_p"] <- sum(str_count(text_lower, "p"))
    df[counter, "freq_q"] <- sum(str_count(text_lower, "q"))
    df[counter, "freq_r"] <- sum(str_count(text_lower, "r"))
    df[counter, "freq_s"] <- sum(str_count(text_lower, "s"))
    df[counter, "freq_t"] <- sum(str_count(text_lower, "t"))
    df[counter, "freq_u"] <- sum(str_count(text_lower, "u"))
    df[counter, "freq_v"] <- sum(str_count(text_lower, "v"))
    df[counter, "freq_w"] <- sum(str_count(text_lower, "w"))
    df[counter, "freq_x"] <- sum(str_count(text_lower, "x"))
    df[counter, "freq_y"] <- sum(str_count(text_lower, "y"))
    df[counter, "freq_z"] <- sum(str_count(text_lower, "z"))
    
    ## frequency of 0-9 
    df[counter, "freq_0"] <- sum(str_count(text_lower, "0"))
    df[counter, "freq_1"] <- sum(str_count(text_lower, "1"))
    df[counter, "freq_2"] <- sum(str_count(text_lower, "2"))
    df[counter, "freq_3"] <- sum(str_count(text_lower, "3"))
    df[counter, "freq_4"] <- sum(str_count(text_lower, "4"))
    df[counter, "freq_5"] <- sum(str_count(text_lower, "5"))
    df[counter, "freq_6"] <- sum(str_count(text_lower, "6"))
    df[counter, "freq_7"] <- sum(str_count(text_lower, "7"))
    df[counter, "freq_8"] <- sum(str_count(text_lower, "8"))
    df[counter, "freq_9"] <- sum(str_count(text_lower, "9"))
  }  
  
  ## return the data frame
  return(df)
}




