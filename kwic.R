# Set current working directory
setwd('C:/Users/Yngve/Google Drive/Skolerelatert/NHH/Master/BAN432/Final exam')

library(dplyr)
library(tm)
library(qdapTools)
library(qdapRegex)
library(wordcloud)
library(tidyr)

library(data.table)
library(tm)
library(stringr)
library(dynverse)
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
txt_new <- remove_duplicates(txt_new) # remove duplicate files with 


###### Turn list of new words into vector #####
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
dic.emerg <- "emerg|technolog|patent|disrupt"

#Locate the terms from dic.emerg in business.des
index.emerg <- unique(sapply(new_tok, function(i) unique(grep(dic.emerg, x = i ))))


#Run KWIC analysis and save results to a list 
result <- list()
sur.words <- c()

for(i in 1:length(index.emerg)){
  result[[i]] <- make.KWIC(index.emerg[[i]],
                           new_tok[[i]],
                           n = 5,
                           doc.nr = i)
  
  sur.words[i] <- paste((result[[i]]$surrounding), collapse=" ") # surrounding words
}

sur.words.new <- paste(sur.words, collapse="")
sur.words.new.split <- strsplit(sur.words.new, " ")



# create a df where all words are counted in each of the data sets
df.new <- as.data.frame(table(sur.words.new.split))
colnames(df.new) <- c('token', 'count')
df.new$age <- rep("new", nrow(df.new))
df.full$new <- ifelse(is.na(df.full$new), 0, df.full$new)

# df.full$token <- unlist(lapply(df.full$token, function(x) gsub(x=x,"([^a-zA-Z0-9])", ""))) # removing noise



