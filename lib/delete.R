

#deletion
delete <- function(error, letter, i){
  return(paste(substring(error, c(0, i), c(i-1,nchar(error)+1)), collapse=letter))

}

final_delete <- function(error){
  word_list <- list()
  for (i in 1 : (nchar(error)+1)){
    ith_list <- sapply(letters, delete, error=error, i=i)
    word_list[[i]] <- ith_list
  }
  words <- data_frame(unname(unlist(word_list)))
  colnames(words) <- 'words' # colname is 'word'
 return(words)
  }





























































