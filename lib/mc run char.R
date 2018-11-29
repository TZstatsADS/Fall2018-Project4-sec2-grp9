load('../lib/deletiontable.RData')
deltable <- deltable[deltable$Correction %in% letters, ]
load('../lib/words.RData')
load('../lib/tf.RData')
load('../lib/two_char.RData')
load('../lib/char.RData')
library(vecsets)
source('../lib/iftoken.R')
file_name_vec <- list.files('../data/ground_truth')
sub_matrix <- read.csv("../data/sub_matrix.csv")
rownames(sub_matrix) <- letters
cm <- read.csv('../data/del_matrix.csv', row.names = 1, header = T)
add_mat = read.csv(file = "../data/add_matrix.csv")
library(parallel)
library(doParallel)
cl <- detectCores()
registerDoParallel(cl)
library(dplyr)
library(tm)
library(stringr)
library(stringi)




final.table.char <- foreach (txti = 1:100) %dopar% {
#for(txti in 1:2){
  ## Load tesseract and groud truth
  current_file_name <- sub(".txt","",file_name_vec[txti])
  current_file_name
  ## read the ground truth text
  current_ground_truth_txt <- readLines(paste('../data/ground_truth/',
                                              current_file_name,".txt",sep=""), warn=FALSE)
  
  ## read the tesseract text
  current_tesseract_txt <- readLines(paste("../data/tesseract/",
                                           current_file_name,".txt",sep=""), warn=FALSE)
  clean_tesseract_txt <- paste(current_tesseract_txt, collapse = " ")
  
  ## detect tesseract word error
  tesseract_vec <- str_split(clean_tesseract_txt," ")[[1]] #1124 tokens
  tesseract_if_clean <- unlist(lapply(tesseract_vec,ifCleanToken)) # source code of ifCleanToken in in lib folder
  false_word <- tolower(tesseract_vec[!tesseract_if_clean])
  ground_truth_vec <- str_split(paste(current_ground_truth_txt, collapse = " ")," ")[[1]]
  
  ##### Deletion #####
  
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
  ##########################
  
  ###### Substitution #########
  
  load('../lib/dict.RData')
  
  #first substitutio
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
      p_t_c <- as.numeric(sub_matrix[x$typo,x$cor])/as.numeric(char[x$cor])
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
  # sub_matrix <- read.csv("/Users/brett/Documents/CU/5243 ADS/Project 4/Fall2018-Project4-sec2--sec2proj4_grp9/data/sub_matrix.csv")
  # rownames(sub_matrix) <- letters
  
  
  sub_candidate <- lapply(var, results_sub)
  sub_candidate <- ldply(sub_candidate)
  ############################################
  
  ######### Insertion ################
  dict <- data.frame(dict)
  dict$words = as.character(dict$words)
  
  # the insertion,delete the character from the error,find candicates
  aftermake = data.frame()
  
  for (i in 1:length(false_word)){
    thisstr1 = false_word[i]
    thisstr = tolower(false_word[i])
    n = length(unlist(strsplit(thisstr, "")))
    thisvec = c()
    for (j in 1:n){
      thismake1 = unlist(strsplit(thisstr, ""))[-j]
      thismake2 = paste(thismake1, sep = "",collapse = "")
      thisvec[j] = thismake2
      
    }
    
    thisdf_inser = data.frame(thisvec)
    thisdf = cbind(rep(thisstr1,nrow(thisdf_inser)),thisdf_inser)
    colnames(thisdf) = c("error","insertion")
    thisdf$error = as.character(thisdf$error)
    thisdf$insertion = as.character(thisdf$insertion)
    aftermake = rbind(aftermake,thisdf)
  }
  
  aftermake$error  = as.character(aftermake$error)
  aftermake$insertion = as.character(aftermake$insertion)
  aftermake_df = data_frame(as.character(unlist(aftermake$insertion)))
  colnames(aftermake_df) = "words"
  #aftermake_df
  #aftermake
  
  # confusion matrix of insertion typo
  # add_mat = read.csv(file = "/Users/brett/Documents/CU/5243 ADS/Project 4/Fall2018-Project4-sec2--sec2proj4_grp9/data/add_matrix.csv")
  
  
  # find the insertion correction that in dictionary
  corrections = aftermake_df%>% 
    inner_join(dict)
  
  freq_c = c()
  for (i in 1:nrow(unique(corrections))){
    thiscor = as.character(unique(corrections)[i,])
    r = as.numeric(tf[tf$term == thiscor,'n'])
    
    freq_c = c(freq_c,r+0.5)
  }
  freq_df = cbind(unique(corrections),freq_c)
  #prior_ele = freq_c/(N+V/2)
  
  #channel_ele
  channel_ele = c()
  for(i in 1:nrow(unique(corrections))){
    thiscor = as.character(unique(corrections)[i,])
    errors =  unique(aftermake[aftermake$insertion == thiscor,'error'])
    n = length(errors)
    channel = c()
    for (j in 1:n){
      thiserror = errors[j]
      #thiscor = aftermake$insertion[1]
      #thiserror = aftermake$error[1]
      index  =sum(strsplit(thiserror, "")[[1]] == strsplit(thiscor, "")[[1]])+1 # find the position of the deleting letter
      thiserror1 = strsplit(thiserror, "")[[1]]
      thiscor1 = strsplit(thiscor, "")[[1]]
      #n1 = length(strsplit(thiserror, "")[[1]])
      if (index == 1){
        this_channel = 0
      }else{
        x = thiserror1[index-1]
        y = thiserror1[index]
        add_num = add_mat[add_mat$X == x, y]
        #charx = sum(as.numeric(add_mat[add_mat$X == x, ]))
        this_channel = add_num/char[x]
      }
      channel = c(channel,this_channel)
    }
    cor_channel = sum(channel)
    channel_ele = c(channel_ele,cor_channel)
  }
  
  rawscore = freq_c*channel_ele
  #percentage = round(rawscore/sum(rawscore)*100)
  corrections1 = unique(corrections)
  
  # find original error words
  n = nrow(corrections1)
  originalerror = c()
  correctionsfinal = c()
  rawscore1 = c()
  freq_c1 = c()
  channel_ele1 = c()
  for (i in 1:n){
    thisrawscore = rawscore[i]
    thisfrec = freq_c[i]
    thischannel_ele = channel_ele[i]
    
    thiscorr = as.character(corrections1[i,])
    thisorginalerror = aftermake[aftermake$insertion == as.character(corrections1[i,]),"error"]
    thisorginalerrors = unique(thisorginalerror)
    m = length(thisorginalerrors)
    correctionsfinal = c(correctionsfinal,rep(thiscorr,m))
    rawscore1 = c(rawscore1,rep(thisrawscore,m))
    freq_c1 = c(freq_c1,rep(thisfrec,m))
    channel_ele1 = c(channel_ele1,rep(thischannel_ele,m))
    originalerror = c(originalerror, thisorginalerrors)
  }
  
  length(originalerror)
  length(correctionsfinal)
  length(rawscore1)
  length(freq_c1)
  length(channel_ele1)
  result_insertion = cbind(originalerror,correctionsfinal, rawscore1,freq_c1,channel_ele1)
  colnames(result_insertion) <- c("error", "cor", "Rawscore", "freq(cor)", "Pr(t/c)")
  ###############################################
  
  ####### Final Candidate ############
  df <- rbind(result_insertion, final_final, as.matrix(sub_candidate))
  df <- data.frame(df)
  df$Rawscore <- as.numeric(as.character(df$Rawscore))
  df$freq.cor. <- as.numeric(as.character(df$freq.cor.))
  df <- split(df, f = df$error)
  
  
  find <- function(df){
    if(nrow(df[df$Rawscore == max(df$Rawscore), ]) != 1){
      df <- df[df[, 4] == max(df[, 4]), ]
    }
    else{
      df <- df[df$Rawscore == max(df$Rawscore), ]
    }
  }
  
  result <- ldply(lapply(df, find), data.frame)
  result <- result[, -1]
  ##################################################
  
  ####### create new tess ############
  index = which(tesseract_if_clean==FALSE)
  indexadd = cbind(index,false_word)
  false_word1 = false_word
  # how did we fixed?
  n = length(result$error)
  for (i in 1:n){
    this_error = as.character(result$error[i])
    this_fix = as.character(result$cor[i])
    ix = which(false_word == this_error)
    false_word1[ix] = this_fix
  }
  
  number.error.corrected = length(this_fix)
  tesseract_vec1 = tesseract_vec
  n = length(false_word1)
  for(i in 1:n){
    ix1 = index[i]
    tesseract_vec1[ix1] = false_word1[i]
  }
  ##########################################
  ground_truth_char <- strsplit(paste(tolower(ground_truth_vec), collapse = ""), split = "")[[1]]
  tesseract_char <- strsplit(paste(tolower(tesseract_vec), collapse = ""), split = "")[[1]]
  
  tesseract_char1 <- strsplit(paste(tolower(tesseract_vec1), collapse = ""), split = "")[[1]]
  
  old_intersect_char <- vintersect(tolower(ground_truth_char), tolower(tesseract_char))
  new_intersect_char <- vintersect(tolower(ground_truth_char), tolower(tesseract_char1))
  
  ocr.error <- length(tesseract_char) - length(old_intersect_char)
  total.dect <- length(false_word)
  afterc.error <- length(tesseract_char) - length(new_intersect_char)
  before.precision <- length(old_intersect_char)/length(tesseract_char)
  before.recall <- length(old_intersect_char)/length(ground_truth_char)
  after.recall <- length(new_intersect_char)/length(ground_truth_char)
  after.precision <- length(new_intersect_char)/length(tesseract_char1)
  print(c(ocr.error, total.dect, afterc.error, before.recall, before.precision, after.recall, after.precision))
}

final.table.char <- ldply(final.table.char)
colnames(final.table.char) <- c("OCR.error", "Total.error.detected", "Error.after.correction",
                           "before.recall", "before.precision", "after.recall", "after.precision")
write_csv(final.table.char, "../lib/final_csv_char.csv")
