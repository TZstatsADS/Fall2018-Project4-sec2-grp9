##############################
## Garbage detection
## Ref: first three rules in the paper
##      'On Retrieving Legal Files: Shortening Documents and Weeding Out Garbage'
## Input: one word -- token
## Output: bool -- if the token is clean or not
##############################

ifCleanToken <- function(cur_token){
  now <- 1
  if_clean <- TRUE
  a <- "a"
  ## in order to accelerate the computation, conduct ealy stopping
  rule_list <- c("nchar(cur_token)>20", # 1. if a string is more than 20 characters in length, it is a barbage, 
                 "str_count(cur_token, pattern = '[A-Za-z0-9]') <= 0.5*nchar(cur_token)",#2.  If the number of punctuation characters in a string is greater than the number of alphanumeric characters, it is garbage
                 "length(unique(strsplit(gsub('[A-Za-z0-9]','',substr(cur_token, 2, nchar(cur_token)-1)),'')[[1]]))>1", #3. Ignoring the first and last characters in a string, if there are two or more different punctuation characters in thestring, it is garbage
                 "sum(nchar(c(str_extract_all(cur_token,'([[:alpha:]])\\1+')[[1]],'a')))>3",#4.if there are or more identical characters in a row in a string. 
                 "(length(regmatches(cur_token, gregexpr('[A-Z]', cur_token))[[1]])>length(regmatches(cur_token, gregexpr('[a-z]', cur_token))[[1]]))&(length(regmatches(cur_token, gregexpr('[A-Z]', cur_token))[[1]])<nchar(cur_token))",#5. 
                 #uppercase number greater than lowercase, and uppercase less than total length
                 "str_detect(cur_token, '^[[:alpha:]]{1,}+$')&&(nchar(gsub('[aeiou]+', '', str_to_lower(gsub('[[:punct:]]+','',cur_token))))>(8*nchar(gsub('[^aeiou]+', '',str_to_lower(gsub('[[:punct:]]+','',cur_token))))))", # 6.1 all character are alphabetic and consonants are greater than 8 times voewls
                 "str_detect(cur_token, '^[[:alpha:]]{1,}+$')&&(8*nchar(gsub('[aeiou]+', '', str_to_lower(gsub('[[:punct:]]+','',cur_token))))<(nchar(gsub('[^aeiou]+', '',str_to_lower(gsub('[[:punct:]]+','',cur_token))))))", #6.2 vice-versa? 
                 "(max(nchar(regmatches(str_to_lower(cur_token),gregexpr('[b-df-hj-np-tv-xz]+|[:punct:]+', str_to_lower(cur_token)))[[1]]))>=5)|(max(nchar(regmatches(str_to_lower(cur_token),gregexpr('[aeiou]+', str_to_lower(cur_token)))[[1]]))>=4)",#7. four and more vowels, five or more consonanats
                 "(sum(str_detect(substring(cur_token, c(1, nchar(cur_token)), c(1, nchar(cur_token))), '[a-z]+'))==2)&&(str_detect(substr(cur_token, 2, nchar(cur_token)-1), '[A-Z]+'))"##8. first and last are lowercase and anyother are uppercase
  ) 
  while((if_clean == TRUE)&now<=length(rule_list)){
    if(eval(parse(text = rule_list[now]))){
      if_clean <- FALSE
    } 
    now <- now + 1
  }
  return(if_clean)
}
