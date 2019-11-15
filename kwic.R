# Set current working directory
setwd('C:/Users/Yngve/Google Drive/Skolerelatert/NHH/Master/BAN432/Final exam')


library(dplyr)
library(tm)
library(qdapTools)
library(qdapRegex)
library(wordcloud)



library(data.table)
library(tm)
library(stringr)
library(dynverse)

#### Extract file names ####
filenames_full <- list.files(path= paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj"))

#### New IPOs (2019) ####
ind_new <- grep("-2019-", filenames_full)
filenames_new <- filenames_full[ind_new]

txt_new <- list()

for (i in 1:length(filenames_new)){
  txt_new[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_new[i]))
}



###### Turn list of new words into vector
# turn into one large string of words
all_new <- paste(txt_new, collapse=" ")     # Combine all texts to one large string
all_new <- tolower(all_new)                 # Make all words lower case
all_new <- removePunctuation(all_new)       # Remove symbols


#Tokanize the list 
new_tok <- sapply(txt_new, function(i) scan(text = i,
                                                    what = "character",
                                                    quote = ""))

#Remove unnecessary punctuation and apply tolower
new_tok <- sapply(new_tok, function(i) removePunctuation(i))
new_tok <- sapply(new_tok, function(i) tolower(i))
new_tok <- sapply(new_tok, function(i) gsub(x = i, 
                                            pattern = "nn|nns|jj|ii|nnp", 
                                            replacement = " "))

#Create a list of terms for the sustainability score

dic.emerg <- "emerging|technology|technology|emerging|patent|disruptive technology"

#Locate the terms from dic.emerg in business.des
index.emerg <- unique(sapply(new_tok, function(i) unique(grep(dic.emerg, x = i ))))

#Create KWIC function
require(dplyr)
make.KWIC <- function(index.env, business.des, n, doc.nr){
  KWIC <- tibble(keyword = business.des[index.env], 
                 surrounding = sapply(index.env,
                                      function(i) {paste(business.des[c(((i-n):(i-1)), ((i+1):(i+n)))],collapse = " ")}),
                 doc.nr = doc.nr,
                 position.in.text = index.env/(length(business.des)*0.01))
  return(KWIC)
}

#Run KWIC analysis and save results to a list 
result <- list()
sur.words <- c()

for(i in 1:108){
  result[[i]] <- make.KWIC(index.emerg[[i]],
                           new_tok[[i]],
                           n = 10,
                           doc.nr = i)
  
  sur.words[i] <- paste((result[[i]]$surrounding), collapse=" ") # surrounding words
}

sur.words.new <- paste(sur.words, collapse="")
sur.words.new.split <- strsplit(sur.words.new, " ")


###### Turn list of old words into vector
#### Old IPOs (2009-2010) ####
ind_old <- grep("-2009-|-2010", filenames_full)
filenames_old <- filenames_full[ind_old]

txt_old <- list()

for (i in 1:length(filenames_old)){
  txt_old[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019/", filenames_old[i]))
}

