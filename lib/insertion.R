# the insertion,delete the character from the error
aftermake = list()
for (i in 1:length(tesseract_delete_error_vec)){
  thisstr = tolower(tesseract_delete_error_vec[i])
  n = length(unlist(strsplit(thisstr, "")))
  thisvec = c()
  for (j in 1:n){
     
     thismake1 = unlist(strsplit(thisstr, ""))[-j]
     thismake2 = paste(thismake1, sep = "",collapse = "")
     thisvec[j] = thismake2
  }
  thisvec
  
  aftermake[[i]] = thisvec    
}
aftermake