library(tm)
library(topicmodels)
library(stringr)

data('AssociatedPress', package = 'topicmodels')

terms <- Terms(AssociatedPress) #extract the unqiue terms 

length(terms)

dictionary <- terms

library(dplyr)

library(tidytext)

ap_td <- tidy(AssociatedPress)

ap_td  #the matrix: the document, the term, the count 

tf <- ap_td %>% 
  
  count(term, sort =F, wt=count) 

head(tf, 20) # the frequency of the term 


a <- 'apple'
nchar(a)



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

false_word <- tesseract_vec[!tesseract_if_clean] # errors in file 5
delete_choice <- lapply(false_word, get_deletion)
delete_choice <- delete_choice[!is.na(delete_choice)]
library(plyr)
delete_candidate <- ldply(delete_choice) # deletion candidate as data frame

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
final_deletion_can <- unique(t(apply(delete_candidate, 1, correction)))