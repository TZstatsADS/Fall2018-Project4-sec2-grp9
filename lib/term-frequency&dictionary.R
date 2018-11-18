
library(tm)
library(topicmodels)
data('AssociatedPress', package = 'topicmodels')


terms <- Terms(AssociatedPress) #extract the unqiue terms 
head(terms)
length(terms)
dictionary <- terms
dictionary # the dictionary 
library(dplyr)
library(tidytext)
ap_td <- tidy(AssociatedPress)
ap_td  #the matrix: the document, the term, the count 
tf <- ap_td %>% 
  count(term, sort =F, wt=count) 
head(tf, 20) # the frequency of the term 























































