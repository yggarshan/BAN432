setwd("C:/Users/vegar/Desktop/NHH/BAN432 (Textual)/Exam")

library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(tidyr)


#### Extract file names ####

filenames_full <- list.files(path= paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj"))


#### New IPOs (2017-2019) ####

ind_new <- grep("-2019-|-2018-|-2017-|-2016", filenames_full)
filenames_new <- filenames_full[ind_new]

txt_new <- list()

for (i in 1:length(filenames_new)){
  txt_new[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_new[i]))
}

      # Create corpus # 
new_corp <- VCorpus(VectorSource(txt_new))

      # Create DTM
dtm_new <- DocumentTermMatrix(new_corp,
                            control=list(tolower=T,                          # Make all words lower case
                                         stopwords=T,                        # Remove all stopwords
                                         removePunctuation=T,                # Remove all symbols
                                         removeNumbers=T,                    # Remove all numbers
                                         stripWhitespace = T,                # Remove whitespace
                                         wordLengths = c(2,20),              # Min letters 
                                         weighting = weightBin,
                                         bounds=list(
                                           global = c(round(length(new_corp)*0.005), # Terms in 5 to 50%
                                                      round(length(new_corp)*0.40))  # of the documents
                                                    )
                                        )
                             )

new_m <- as.matrix(dtm_new)



#### Old IPOs (2008-2010) ####

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

#### Pyramid Plot #####
top25_df <- df_full %>% 
  filter_all(all_vars(.>0)) %>% 
  top_n(n=25, wt=diff) %>% 
  arrange(desc(diff))

pyramid.plot(
  top25_df$new, 
  top25_df$old,
  labels = top25_df$token, 
  top.labels = c("OLD", "Words", "NEW"), 
  main = "Words in Common", 
  gap =1000,
  space=0.2
)













#                                                                             
##                                                                          
###                                                                        
#### Testing: Collapse new and old lists to two string vectors; new & old ####
###                                                                        
##                                                                          
#                                                                           


comparison.cloud(as.matrix(df_full), colors=c("orange", "blue"), max.words=100)
# The problem with this one is that it has too many common words, must apply something
# like the "bounds=" argument
# Could be done through rowSums(tdm_m) and only keep higher than x and lower than y
# head(rowSums(tdm_m))



#### Pyramid Plot #####
top25_df <- tdm_m %>% as_data_frame(rownames="word") %>% 
  filter_all(all_vars(.>0)) %>% 
  mutate(difference = OLD-NEW) %>% 
  top_n(n=25, wt=difference) %>% 
  arrange(desc(difference))

pyramid.plot(
  top25_df$OLD, 
  top25_df$NEW,
  labels = top25_df$word, 
  top.labels = c("OLD", "Words", "NEW"), 
  main = "Words in Common", 
  gap =1000,
  space=0.2
)