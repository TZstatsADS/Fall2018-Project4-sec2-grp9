

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

# write_csv(result, '/Users/brett/Documents/CU/5243 ADS/Project 4/Fall2018-Project4-sec2--sec2proj4_grp9/data/result-48.csv')
