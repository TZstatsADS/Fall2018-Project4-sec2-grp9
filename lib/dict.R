library(dplyr)

files <- list.files(path='../data/ground_truth',pattern = '*.txt', full.names =TRUE  )
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

frequency <- data_frame(words, ncol=1)
colnames(frequency) <- c("words",'n')
head(frequency, 20)
frequency

tf <-  frequency %>% 
  count(words, sort = T)

save(words, file = "../lib/words.RData")
save(tf, file = "../lib/tf.RData")

#single character

char <- table(strsplit(paste(words, collapse = ""), split = "")[[1]])[letters] # the chars for every character 
char #the char for single character 

#two characters
new_word <- words[nchar(words)>=2]
for ( j in 1: length(new_word)){
  for (i in 1: (nchar(new_word[j])-1)){
    c <- append(c, substr(new_word[j], start=i, stop=i+1))
  }
}

m <-c()
for (i in letters) { 
  for (j in letters){
    m <- append(m, paste(c(i, j), collapse = ""))
  }
  }

two_char <- table(c)[m]
save(two_char, file = "../lib/two_char.RData")














