
library(tm)
library(topicmodels)
library(dplyr)
data('AssociatedPress', package = 'topicmodels')


terms <- Terms(AssociatedPress) #extract the unqiue terms 
head(terms)
length(terms)
dictionary <- terms
dict <- data_frame(dictionary) # the dictionary 
colnames(dict) <- 'words' # create matrix for dictionary, and colname is 'words'
dict
library(dplyr)
library(tidytext)
ap_td <- tidy(AssociatedPress)
ap_td  #the matrix: the document, the term, the count 
tf <- ap_td %>% 
  count(term, sort =F, wt=count) 
head(tf, 20) # the frequency of the term 





#





