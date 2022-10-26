
library(tm)
library(readr)
library(stm)

####################
#### Data Input ####
####################

## Declare the targets for analysis
data=read.csv("../data/ConsumerAffairs/CombinedNoDuplicates.csv")[,-1]
targets=targets_old=c(1919,1874,1645,1337,160,457,1305,1354,1900)       ## first 4 are small cities, second 5 are bigger cities
T=length(targets_old)      
jtargets=goldstandard=c(1918,1873,1644,1336,159,456,1304,1353,1899)     ## matching reviews to use in-sample
for (t in 1:length(targets_old)){targets[t]=which(data$Row==targets_old[t]);jtargets[t]=which(data$Row==goldstandard[t])} ## index targets and their matching records
data_targets=data[targets,]                                             ## save the targets 
j_targets=data[jtargets,]                                               ## save the target matches

data=data[which(is.na(data$Rating)==0),]                                ## remove reviews with no ratings

state1=which(data$State=="NM")
state2=which(data$State=="GA")
state3=which(data$State=="FL")
state4=which(data$State=="CA")
state5=which(data$State=="NC")
state6=which(data$State=="TX")
state7=which(data$State=="VA")
state8=which(data$State=="IL")
data2=data[c(state1,state2,state3,state4,state5,state6,state7,state8),]  

for (t in 1:length(targets_old)){targets[t]=which(data2$Row==data_targets$Row[t])}
data2=data2[-targets,]                                                         ## remove targets
for (t in 1:length(targets_old)){jtargets[t]=which(data2$Row==j_targets$Row[t])}

## Declare the targets for analysis

T=length(targets_old)                                                        ## Number of targets
data_targets$State=factor(data_targets$State)
data2$State=factor(data2$State)

#############################
# invalid_utf8_ <- function(x){
# 
#   !is.na(x) & is.na(iconv(x, "UTF-8", "UTF-8"))
# 
# }
# 
# detect_invalid_utf8 <- function(string, seperator){
# 
#   stringSplit <- unlist(strsplit(string, seperator))
# 
#   invalidIndex <- unlist(lapply(stringSplit, invalid_utf8_))
# 
#   data.frame(
#     word = stringSplit[invalidIndex],
#     stringIndex = which(invalidIndex == TRUE)
#   )
# 
# }

textual=as.character(data2$Text)

#invalids <- do.call("rbind", lapply(textual, function(x) detect_invalid_utf8(x, " ")))

#textual_byte <- iconv(enc2utf8(invalids$word), sub = "byte")

#textual_UTF <- iconv(invalids$word, "UTF-8", "UTF-8", sub='')

#textual_byte

#textual_UTF

#########################################

## Create a data frame with each column being a textual variable, entries are frequencies
Encoding(textual) <- "UTF-8"
textual <- iconv(textual, "UTF-8", "UTF-8", sub='')
corpus <- Corpus(VectorSource(textual))
#corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus <- tm_map(corpus, tolower) # convert text to lowercase
corpus <- tm_map(corpus, removeNumbers) #remove numbers
corpus <- tm_map(corpus, removePunctuation) #remove punctuation
corpus <- tm_map(corpus, stripWhitespace) #remove white space
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("SMART"))) 
corpus <- tm_map(corpus, function(x) removeWords(x, c("usaa","company","insurance","customer"))) 
dtm <- DocumentTermMatrix(corpus)
td.mat <- data.frame(as.matrix(dtm))
mm <- readCorpus(td.mat)

# create X matrix of text data for observations with a value for State
X <- as.matrix(dtm[!is.na(data2$State),])

