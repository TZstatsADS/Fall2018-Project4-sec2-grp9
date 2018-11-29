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
