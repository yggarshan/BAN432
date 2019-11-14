setwd("C:/Users/vegar/Desktop/NHH/BAN432 (Textual)/Exam")

library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(tidyr)

filenames_n_ad <- list.files(path= paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj"))

#### New IPOs (2017-2019) ####

ind_new <- grep("-2019-|-2018-|-2017-", filenames_n_ad)
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


df_new <- df_new %>% group_by(token) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

#### Old IPOs (2008-2010) ####

ind_old <- grep("-2010-|-2009-", filenames_n_ad)
filenames_old <- filenames_n_ad[ind_old]

txt_old <- list()

for (i in 1:313){
  txt_old[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_old[i]))
}

txt_old[[1]]

txt_old <- unlist(txt_old)
df_old <- data.frame(token=rep(NA,length(txt_old)))
df_old$token <- txt_old



df_old <- df_old %>% separate(token, c("token", "xpos"), sep="\t")
df_old <- df_old[-1,]


df_new <- df_new %>% group_by(token) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))