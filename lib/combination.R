library(plyr)

df <- rbind(result_df, final_final, results_sub)
df <- split(df, f = result_df$error)

find <- function(df){
  if(nrow(df[df$Rawscore == max(df$Rawscore), ]) != 1){
    df <- df[df$`freq(cor)` == max(df$`freq(cor)`), ]
  }
  else{
    df <- df[df$Rawscore == max(df$Rawscore), ]
  }
}

result <- ldply(lapply(df, find), data.frame)

