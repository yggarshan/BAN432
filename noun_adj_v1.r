setwd("C:/Users/vegar/Desktop/NHH/BAN432 (Textual)/Exam")

library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(tidyr)

filenames_n_ad <- list.files(path= paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj"))

#### New IPOs (2017-2019) ####

ind_new <- grep("-2019-", filenames_n_ad)
filenames_new <- filenames_n_ad[ind_new]

txt_new <- list()

for (i in 1:313){
  txt_new[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_new[i]))
}

txt_new[[1]]

txt_new <- unlist(txt_new)

df_new <- data.frame(token=rep(NA,length(txt_new)))
df_new$token <- txt_new


df_new <- df_new %>% separate(token, c("token", "xpos"), sep="\t")

df_new <- df_new[-1,]
df_new$token <- tolower(df_new$token)


df_new <- df_new %>% filter(token != "") %>%
  group_by(token) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#### Old IPOs (2008-2010) ####

ind_old <- grep("-2010-|-2009-", filenames_n_ad)
filenames_old <- filenames_n_ad[ind_old]

txt_old <- list()

for (i in 1:313){
  txt_old[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_old[i]))
}


txt_old <- unlist(txt_old)
txt_old <- tolower(txt_old)


df_old <- data.frame(token=rep(NA,length(txt_old)))
df_old$token <- txt_old



df_old <- df_old %>% separate(token, c("token", "xpos"), sep="\t")
df_old <- df_old[-1,]
df_old$token <- tolower(df_old$token)




df_old <- df_old %>% filter(token != "") %>%
  group_by(token) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


#### Join data frames ####
df_new$age <- rep("new", nrow(df_new))
df_old$age <- rep("old", nrow(df_old))

df_full <- rbind(df_new, df_old)
df_full <- spread(df_full, age, n)
df_full <- df_full %>% mutate(diff=new-old)



#df_new$token <- unlist(lapply(df_new$token, function(x) gsub(x=x,"([^a-zA-Z0-9])", ""))) # removing noise


