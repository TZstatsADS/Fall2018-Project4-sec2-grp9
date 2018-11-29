
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
