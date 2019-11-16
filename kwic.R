# Set current working directory
setwd('C:/Users/Yngve/Google Drive/Skolerelatert/NHH/Master/BAN432/Final exam')

library(dplyr)
library(tm)
library(tidyr)
library(data.table)
library(tm)
library(stringr)
require(dplyr)

# Function to run KWIC analysis 
make.KWIC <- function(index.env, business.des, n, doc.nr){
  KWIC <- tibble(keyword = business.des[index.env], 
                 surrounding = sapply(index.env,
                                      function(i) {paste(business.des[c(((i-n):(i-1)), 
                                                                        ((i+1):(i+n)))],
                                                         collapse = " ")}),
                 doc.nr = doc.nr,
                 position.in.text = index.env/(length(business.des)*0.01))
  return(KWIC)
}


# Function to remove duplicates text files
remove_duplicates <- function(text_list){
  rows_delete = c()
  list_delete = list()
  for (i in 1:length(text_list)) {
    for (j in 2:length(text_list)) {
      identical(text_list[[i]], text_list[[j]])
      if (j < i) { rows_delete[j] <- NA
      }   else if (identical(text_list[[i]], text_list[[j]]) == TRUE) {
        rows_delete[j] <- j
        list_delete[[i]] <-  rows_delete[-c(which(is.na(rows_delete)),which(rows_delete==""),which(rows_delete==i))]
      } 
      else if (identical(text_list[[i]], text_list[[j]]) == FALSE) {
        rows_delete[j] <- NA
      }
    }
  }
  final_delete <- c(unique(unlist(list_delete)))  #2 documents are identical
  final_text <- text_list[-c(final_delete)]
}


#### Extract file names ####
filenames_full <- list.files(path= paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj"))


#### New IPOs (2019) ####
ind_new <- grep("-2019-", filenames_full)
filenames_new <- filenames_full[ind_new]
txt_new <- list() # create list where IPO reports will be stored


# Extract all files in working directory that matches specified year(s)
for (i in 1:length(filenames_new)){
  txt_new[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_new[i]))
}


# Remove duplicate files with own function
txt_new <- remove_duplicates(txt_new) # remove duplicate files with own created function


###### Turn list of new words into vector
# turn into one large string of words
all_new <- paste(txt_new, collapse=" ")     # Combine all texts to one large string
all_new <- tolower(all_new)                 # Make all words lower case


#Tokanize the list of new IPO reports 
new_tok <- sapply(txt_new, function(i) scan(text = i,
                                                    what = "character",
                                                    quote = ""))


# Data cleaning of vector of new words
new_tok <- sapply(new_tok, function(i) removePunctuation(i))              # Remove punctuation
new_tok <- sapply(new_tok, function(i) tolower(i))                        # Make all to lower
new_tok <- sapply(new_tok, function(i) gsub(x = i,                        # Remove words tags
                                            pattern = "nn|nns|jj|ii|nnp", 
                                            replacement = " "))


#Create a list of terms of relevant technology terms
dic.emerg <- "emerg|technolog|leading"


#Locate the terms from dic.emerg in business.des
index.emerg <- unique(sapply(new_tok, function(i) unique(grep(dic.emerg, x = i ))))


#Run KWIC analysis and save results to a list 
result <- list() # create new list to store KWIC results 
sur.words <- c() # create vector to store surrounding words


for(i in 1:length(index.emerg)){ # run KWIC analysis
  result[[i]] <- make.KWIC(index.emerg[[i]], # report index
                           new_tok[[i]],     # word that the analysis is looking for
                           n = 5,            # amount of surrounding words to extract
                           doc.nr = i)       # document number
  
  sur.words[i] <- paste((result[[i]]$surrounding), collapse=" ") # surrounding words is a
                                                                 # list of all words found
}

# Clean the list of surrounding words
sur.words.new <- paste(sur.words, collapse="") # combine all surrounding words into one string
sur.words.new.split <- unlist(strsplit(sur.words.new, " ")) # tokenize (split) all words from string into vector
sur.words.new.split <- tolower(sur.words.new.split)
sur.words.new.split <- removeNumbers(sur.words.new.split)
sur.words.new.split <- removePunctuation(sur.words.new.split)
sur.words.new.split <- unlist(lapply(sur.words.new.split, function(x) gsub(x=x,"([^a-zA-Z0-9])", "")))
sur.words.new.split <- removeWords(sur.words.new.split, stopwords("en"))
sur.words.new.split <- stemDocument(sur.words.new.split)

df.new <- as.data.frame(table(sur.words.new.split))
colnames(df.new) <- c('token', 'count')
df.new$token <- as.character(df.new$token)
df.new <- df.new %>% filter(token != "", nchar(token) > 2, count>1, count<150)



# create a df where all words are counted in each of the data sets
df.new <- as.data.frame(table(sur.words.new.split))
colnames(df.new) <- c('token', 'count')
df.new$age <- rep("new", nrow(df.new))




#### Old IPOs (2008-2010) ####


ind_old <- grep("-2009-", filenames_full)
filenames_old <- filenames_full[ind_old]

txt_old <- list()

for (i in 1:length(filenames_old)){
  txt_old[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_old[i]))
}

txt_new <- remove_duplicates(txt_new) # remove duplicate files with 

# Prepare "old" data set
txt_old <- unlist(txt_old)

df_old <- data.frame(token=rep(NA,length(txt_old)))
df_old$token <- txt_old

df_old <- df_old %>% separate(token, c("token", "xpos"), sep="\t")
df_old <- df_old[-1,]

df_old$token <- tolower(df_old$token)
df_old$token <- removePunctuation(df_old$token)
df_old$token <- removeNumbers(df_old$token)


df_old <- df_old  %>%
  group_by(token) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

df_old$token <- unlist(lapply(df_old$token, function(x) gsub(x=x,"([^a-zA-Z0-9])", "")))


#### Join data frames ####
df_old$token <- stemDocument(df_old$token)
df_old <- df_old  %>%
  filter(token != "" & nchar(token) > 1) %>% 
  group_by(token) %>% 
  summarise(count = sum(n)) %>% 
  arrange(desc(count))

df_new$token <- stemDocument(df_new$token)
df_new <- df_new  %>%
  filter(token != "" & nchar(token) > 1) %>%
  group_by(token) %>% 
  summarise(count = sum(n)) %>% 
  arrange(desc(count))

df_new$age <- rep("new", nrow(df_new))
df_old$age <- rep("old", nrow(df_old))

df_full <- rbind(df_new, df_old)
df_full <- spread(df_full, age, count)
df_full$new <- ifelse(is.na(df_full$new), 0, df_full$new)
df_full$old <- ifelse(is.na(df_full$old), 0, df_full$old)
df_full <- df_full %>% mutate(diff=new-old)
df_full$token <- removeWords(df_full$token, stopwords("en"))
df_full <- df_full %>% filter(token != "", diff > 0)

df_full <- df_full %>% mutate(log_diff = log(abs(diff)))

wordlist <- df_full %>% filter(token %in% df.new$token)

#### Create a weightbin column ####


