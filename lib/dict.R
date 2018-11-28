library(dplyr)

files <- list.files(path="/Users/Boris/Documents/GitHub/Fall2018-Project4-sec2--sec2proj4_grp9/data/ground_truth",pattern = '*.txt', full.names =TRUE  )
words <- c()
for (file in files ){
  text <- readLines(file, warn = F)
  text <- paste(text, collapse = " ")
  text.words <- strsplit(text, split = "(\\s|[[:punct:]])+")[[1]]
  words <- append(words, text.words)
}
words <- tolower(words)
head(words) # the dictionary without duplication
fq_table <- table(words) # the frequency of words from table function 

N <- length(words) # how many words in ground truth 
N
vocabulary <- unique(words) # the unique words in dictionary 
V  <- length(vocabulary)  # the number of unique words 
V

dict <- matrix(vocabulary, ncol = 1)
colnames(dict) <- 'words'
head(dict)            # the dictionary made from matrix 

frequency <- data_frame(words, ncol = 1)
colnames(frequency) <- c("words","n")
head(frequency, 20)

frequency
tf <-  frequency %>% 
  count(words)

head(tf, 20)


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



######check the table##############
colnames(deltable) <- c('Key', 'Correction', 'Position')
head(deltable, 50)








































































