cm <- read.csv("del_matrix.csv", row.names = 1, header = TRUE)

library(stringr)

score.1 <- function(row, cm){
  letter2num <- function(x) {
    utf8ToInt(x) - utf8ToInt("a") + 1L
  }
  
  score <- vector("numeric")
  if(row[3] == 0){
    x <- 27
    y <- letter2num(row[2])
  }
  else{
    x <- letter2num(substr(row[1], row[3], row[3]))
    y <- letter2num(row[2])
  }
  s1 <- cm[x, y]

  s2 <- sum(lapply(corpus.dup, str_count, pattern = paste0(substr(row[1], row[3], row[3]), row[2]))) 
  return(s1/s2)
}




