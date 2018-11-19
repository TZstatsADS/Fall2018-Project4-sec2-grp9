
library(tm)
library(topicmodels)
library(dplyr)
library(tidytext)
data('AssociatedPress', package = 'topicmodels')


terms <- Terms(AssociatedPress) #extract the unqiue terms 



dict <- data.frame(terms) # the dictionary 
colnames(dict) <- 'words' # create matrix for dictionary, and colname is 'words'



ap_td <- tidy(AssociatedPress)
ap_td  #the matrix: the document, the term, the count 
tf <- ap_td %>% 
  count(term, sort =F, wt=count) 
head(tf, 20) # the frequency of the term 





#





