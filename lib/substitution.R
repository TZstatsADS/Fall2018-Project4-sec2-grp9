

#substitution 
substitution <- function(error, letter, i){
  substr(error, i, i) <- letter 
  return(error)
}
final_sub <- function(error){
  word_list <- list()
  for (i in 1: nchar(error)){
    ith_list <- sapply(letters, substitution, error=error, i=i)
    word_list[[i]] <- ith_list
  }
  words <- data_frame(unname(unlist(word_list)))
  colnames(words) <- 'words' # colname is 'words'
  return(words)
}

#find the matching word with dictionary 
sub_can <- function(error){
  return(final_sub(error)%>% 
           inner_join(dict)%>% 
           unique())
}





































