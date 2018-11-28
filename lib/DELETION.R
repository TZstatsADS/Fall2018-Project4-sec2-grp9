library(dplyr)
library(tm)
library(topicmodels)
library(stringr)

###############################################
############deletion table#####################
###############################################

###calculate how many rows in the deletion table
allterm <- as.data.frame(tf)
wordlength <- function(wordlist){
  sum <- 0
  for (i in 1:nrow(wordlist)){
    sum <- sum + nchar(wordlist$words[i])
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



##########get the table for all terms#######
deltable <- data.frame(matrix(ncol = 3, nrow = nallchar))
len <- rep(NA, nrow(allterm))

for (t in 1:nrow(allterm)){
  if (t == 1){
    len[t] <- nchar(allterm$words[t])
    deltable[1:len[t], ] <- candidate(allterm$words[t])
  }else{
    len[t] <- nchar(allterm$words[t])
    deltable[(sum(len[1:(t-1)])+1):sum(len[1:t]), ] <- candidate(allterm$words[t])
  }
}

deltable <- deltable[deltable$Correction %in% letters, ]

######check the table##############
colnames(deltable) <- c('Key', 'Correction', 'Position')

###### get deletion candidate ######
get_deletion <- function(error_word) {
  res <- deltable[deltable$Key == error_word, ]
  if (nrow(res) == 0) {
    return(NA)
  }
  else {
    return(res)
  }
}

false_word <- tolower(tesseract_vec[!tesseract_if_clean]) # errors in file 5
delete_choice <- lapply(false_word, get_deletion)
delete_choice <- delete_choice[!is.na(delete_choice)]
library(plyr)
delete_candidate <- ldply(delete_choice) # deletion candidate as data frame
delete_candidate <- unique(delete_candidate)

library(stringi)

# Convert back to correct version
correction <- function(row){
  can <- vector(, length = 2)
  can[1] <- row[1]
  
  if(as.numeric(row[3] == 0)){
    can[2] <- paste0(row[2], row[1])
  }
  
  if(as.numeric(row[3] == 1)){
    can[2] <- paste0(stri_sub(row[1], 1, 1), row[2], 
                     stri_sub(row[1], 2, nchar(row[1]))) 
  }
  else{
    can[2] <- paste0(stri_sub(row[1], 1, as.numeric(row[3])), row[2], 
                     stri_sub(row[1], as.numeric(row[3]) + 1, nchar(row[1])))  
  }
  return(can)
}

# return a data frame with error and corresponding correction
final_deletion_can <- (t(apply(delete_candidate, 1, correction)))

final_final <- as.matrix(cbind(delete_candidate, Word = final_deletion_can[, 2])) # deletion candidate without score


# Read deletion confusion matrix
cm <- read.csv("del_matrix.csv", row.names = 1, header = TRUE)

library(stringr)

# P(t|c)
source("char")
source("two_char")
score.1 <- function(row, cm){
  letter2num <- function(x) {
    utf8ToInt(x) - utf8ToInt("a") + 1L
  }
  
  corpus.dup <- words
  if(row[3] == 0){
    x <- 27
    y <- letter2num(row[2])
  }
  else{
    x <- letter2num(substr(row[1], row[3], row[3]))
    y <- letter2num(row[2])
  }
  s1 <- cm[x, y]
  
  if((row[3] == 0)){
    s2 <- as.numeric(char[row[2]])
  } 
  else{
    s2 <- as.numeric(two_char[paste0(substr(row[1], row[3], row[3]), row[2])])
  }
  return(s1/s2)
}


# freq(c)
score.2 <- function(row) {
  freqc <- as.numeric(tf[tf$term == row[4], 2])
  return(freqc)
}

# Add score to deletion candidate
final_final <- cbind(final_final, as.vector(apply(final_final, 1, score.2)))
final_final <- cbind(final_final, as.vector(apply(final_final, 1, score.1, cm =cm)))

final_final <- final_final[, -c(2, 3)]
final_final <- cbind(final_final, as.numeric(final_final[, 3])*as.numeric(final_final[, 4]))
final_final <- final_final[, c(1, 2, 5, 3, 4)]
colnames(final_final) <- c("error", "cor", "Rawscore", "freq(cor)", "Pr(t/c)")
