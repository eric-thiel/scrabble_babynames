### create values df ###
# (1 point)-A, E, I, O, U, L, N, S, T, R
# (2 points)-D, G
# (3 points)-B, C, M, P
# (4 points)-F, H, V, W, Y
# (5 points)-K
# (8 points)- J, X
# (10 points)-Q, Z

library(dplyr)

letter_score =
  function(letter){
    ifelse(letter %in% c("e","a","i","o","n","r","t","l","s","u"),1,
    ifelse(letter %in% c("d","g"),2,
    ifelse(letter %in% c("b","c","m","p"),3,
    ifelse(letter %in% c("f","h","v","w","y"),4,
    ifelse(letter %in% c("k"),5,
    ifelse(letter %in% c("j","x"),8,
    ifelse(letter %in% c("q","z"),10,0)))))))
  }

ex_names = c("Eric","Michael","Daniel", "Samuel")
ex_names = data.frame(ex_names)
library(readr)
top_1000_names <- read_csv("~/Downloads/baby_scrab - Sheet1.csv")


hold_df <- data.frame(matrix(ncol = 2, nrow = 0))
ragrat <- c("word","score")
colnames(hold_df) <- ragrat

calc_letter_score = function(dataframe){
  for(i in 1:nrow(dataframe)){
    word = dataframe[i,]
    word <- tolower(word)
    word_split <- strsplit(word,"")
    score = sapply(lapply(word_split, letter_score),sum)
    combine = data.frame(word, score)
    hold_df = rbind(hold_df, combine)
  }
  return(hold_df)
}

x = calc_letter_score(top_1000_names)%>%filter(score == 12)%>%
  mutate(er = grepl("er",word))%>% filter(er == FALSE) %>% select(-er)

OFFICIAL_GUESS = x %>% filter(word == "Vincent")
