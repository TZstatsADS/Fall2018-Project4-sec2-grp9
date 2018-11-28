
###################
###substitution####
##################


#first substitutio
sub_matrix <- read.csv('sub_matrix.csv')
rownames(sub_matrix) <- letters
substitution <- function(error, letter, i){
  x <- strsplit(error, split = "")[[1]][i]
  substr(error, i, i) <- letter 
  
  return(c(error, letter, x))
}


#find the matching word with dictionary 
sub_can <- function(error){
  word_list <- list()
  for (i in 1: nchar(error)){
    ith_list <- sapply(letters, substitution, error=error, i=i)
    word_list[[i]] <- ith_list
  }
  x <- matrix(unlist(word_list), ncol = 3, byrow = T)
  colnames(x) <- c('words', 'cor', 'typo')
   merge_words <-  merge(x, dict, by.x ='words', by.y ='words' )
   error_num <- rep(error, nrow(merge_words))
   return(cbind(error_num, merge_words))
  
}

#give each score for every candidate 
results_sub <- function(error){
  y<- sub_can(error)
  df <- data.frame(y[,1:2])
  for (i in 1: nrow(sub_can(error))){
    x <- sub_can(error)[i,]
    prior <- (tf[tf$term==x$words,]$n + 0.5)/(300847+16902)
    p_t_c <- as.numeric(sub_matrix[(x$typo),(x$cor)])/as.numeric(char[x$cor])
    df[i, 3] <- prior*p_t_c
    df[i, 4] <- tf[tf$term==x$words,]$n
    df[i,5] <- p_t_c
  }
  colnames(df) <- c("error","cor","Rawscore", "freq(cor)","Pr(t/c)")
  return(df)
}
#return a dataframe 
new_false_word <- unique(false_word[sapply(false_word,str_detect,"^[[:alpha:]]{1,}+$" )])
var <- c()
for (i in 1: length(new_false_word)){
  if (nrow(sub_can(new_false_word[i]))>0){
    var <- append(var, new_false_word[i])
  }
}
var
sub_can('pry')
results_sub('by')
results_sub('pry')
sub_candidate <- lapply(var, results_sub)
length(var)
for ( i in 1:length(var)){
  print(results_sub(var[i]))
}
var[1]
