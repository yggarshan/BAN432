setwd("C:/Users/vegar/Desktop/NHH/BAN432 (Textual)/Exam")

library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(tidyr)


#### Extract file names ####

filenames_full <- list.files(path= paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj"))


#### New IPOs (2016-2019) ####

ind_new <- grep("-2019-|-2018-|-2017-|-2016", filenames_full)
filenames_new <- filenames_full[ind_new]

txt_new <- list()

for (i in 1:length(filenames_new)){
  txt_new[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_new[i]))
}

#### Old IPOs (2009-2010) ####

ind_old <- grep("-2009-|-2010", filenames_full)
filenames_old <- filenames_full[ind_old]

txt_old <- list()

for (i in 1:length(filenames_old)){
  txt_old[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019/", filenames_old[i]))
}

# Prøver å lage felles corpus. Dette kan potensielt gjøre at et ord fjernes i både new og old dersom den fjernes i den ene av dem
nouns_adjs <- VCorpus(VectorSource(c(txt_old, txt_new)))

# Task 3 A
dtm_A <- DocumentTermMatrix(nouns_adjs,
                            control=list(tolower=T,      # specify different controls, e.g. tolower
                                         removePunctuation=T,
                                         removePunctuation=T,
                                         removeNumbers=T,
                                         stemming = T,
                                         weighting = weightBin,
                                         wordLengths = c(2,20),
                                         bounds=list(
                                           global = c(length(c(txt_new,txt_old))*0.05,
                                                      length(c(txt_new,txt_old))*0.5) # terms in 5 to 50 of the documents
                                         )
                            ))

df_comb_full$token <- unlist(lapply(df_comb_full$token, function(x) gsub(x=x,"([^a-zA-Z0-9])", ""))) # removing noise



##### Turn combined corpus into df #####
comb_m <- as.matrix(dtm_A) # combined matrix
len_old <- length(txt_old)
len_new <- length(txt_new)

df_comb_old <- data.frame(token=colnames(dtm_A[1:len_old,]), # subsetting only "old" 
                          count=colSums(comb_m[1:len_old,])/len_old) # documents from dtm_A
df_comb_old$age <- rep("old", nrow(df_comb_old)) # creating age column for df_old

df_comb_new <- data.frame(token=colnames(dtm_A[len_old+1:len_new,]), 
                          count=colSums(comb_m[len_old+1:len_new,])/len_new)
df_comb_new$age <- rep("new", nrow(df_comb_new))


# Turning the two df's into one combined df 
df_comb_full <- rbind(df_comb_new, df_comb_old)
df_comb_full <- spread(df_comb_full, age, count)
df_comb_full$new <- ifelse(is.na(df_comb_full$new), 0, df_comb_full$new)
df_comb_full$old <- ifelse(is.na(df_comb_full$old), 0, df_comb_full$old)


df_comb_full <- df_comb_full %>% mutate(diff = new-old)
