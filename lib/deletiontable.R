library(tm)
library(topicmodels)
library(dplyr)
library(tidytext)

###############################################
############deletion table#####################
###############################################

###calculate how many rows in the deletion table
allterm <- as.data.frame(tf)
wordlength <- function(wordlist){
  sum <- 0
  for (i in 1:nrow(wordlist)){
    sum <- sum + nchar(wordlist$term[i])
  }
  return(sum)
}
nallchar <- wordlength(allterm)


######for one word find the deletion cadidate######
candidate <- function(word){
  cand <- data.frame(matrix(ncol = 3, nrow = nchar(word)))
  for (i in 1:nchar(word)){
    word1 <- word
    str_sub(word1, i, i) <- ''
    cand[i,] <- c(word1, substr(word, i,i), i-1)
  }
  return(cand)
}

candidate(a)


##########get the table for all terms#######
deltable <- data.frame(matrix(ncol = 3, nrow = nallchar))
len <- rep(NA, nrow(allterm))

for (t in 1:nrow(allterm)){
  if (t == 1){
    len[t] <- nchar(allterm$term[t])
    deltable[1:len[t], ] <- candidate(allterm$term[t])
  }else{
    len[t] <- nchar(allterm$term[t])
    deltable[(sum(len[1:(t-1)])+1):sum(len[1:t]), ] <- candidate(allterm$term[t])
  }
}



######check the table##############
colnames(deltable) <- c('Key', 'Correction', 'Position')
head(deltable, 50)


