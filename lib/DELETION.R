

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

#false_word <- tolower(tesseract_vec[!tesseract_if_clean]) # errors in file
delete_choice <- lapply(false_word, get_deletion)
delete_choice <- delete_choice[!is.na(delete_choice)]
library(plyr)
delete_candidate <- ldply(delete_choice) # deletion candidate as data frame
delete_candidate <- unique(delete_candidate)


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
# cm <- read.csv(paste(wd, "/data/del_matrix.csv", sep = ""), row.names = 1, header = TRUE)

library(stringr)

# P(t|c)
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
