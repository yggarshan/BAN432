# Set current working directory
setwd('C:/Users/Yngve/Google Drive/Skolerelatert/NHH/Master/BAN432/Final exam')
load('common_unique_words.RData')
load('tok.RData') # tokenized list of all documents

library(sjmisc)
library(rlist)
library(dplyr)
library(tm)
library(tidyr)
library(data.table)
library(tm)
library(stringr)
require(dplyr)

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


####### Prøver med ett og ett år ######
filenames_full <- list.files(path= paste0(getwd(),"/ipos_2nd_qtr_2008_2019"))

#
tok_sample <- c()
for (i in 1:length(filenames_full)){
  tok_sample[i] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019/", filenames_full[i]))
}

## Read in all files, yearwise
doc_year <- list()
doc_year[[1]]  <- unlist(tok_sample[grep('-2008-', filenames_full)])
doc_year[[2]]  <- tok_sample[grep('-2009-', filenames_full)]
doc_year[[3]]  <- tok_sample[grep('-2010-', filenames_full)]
doc_year[[4]]  <- tok_sample[grep('-2011-', filenames_full)]
doc_year[[5]]  <- tok_sample[grep('-2012-', filenames_full)]
doc_year[[6]]  <- tok_sample[grep('-2013-', filenames_full)]
doc_year[[7]]  <- tok_sample[grep('-2014-', filenames_full)]
doc_year[[8]]  <- tok_sample[grep('-2015-', filenames_full)]
doc_year[[9]]  <- tok_sample[grep('-2016-', filenames_full)]
doc_year[[10]] <- tok_sample[grep('-2017-', filenames_full)]
doc_year[[11]] <- tok_sample[grep('-2018-', filenames_full)]
doc_year[[12]] <- tok_sample[grep('-2019-', filenames_full)]


#### 2008 ####
ind_new <- grep("-2008-", filenames_full)
filenames_new <- filenames_full[ind_new]
txt_08 <- list() # create list where IPO reports will be stored

for (i in 1:length(filenames_new)){
  txt_08[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_new[i]))
}

txt_08 <- remove_duplicates(txt_08)

txt_08 <- unlist(txt_08)
txt_08 <- sapply(txt_08, function(x) gsub("\\t.*", "", x))
txt_08 <- sapply(txt_08, function(x) gsub("([^a-zA-Z0-9])", "", x))
names(txt_08) <- NULL

#### 2018 ####
ind_new <- grep("-2018-", filenames_full)
filenames_new <- filenames_full[ind_new]
txt_18 <- list() # create list where IPO reports will be stored

for (i in 1:length(filenames_new)){
  txt_18[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_new[i]))
}
txt_18 <- remove_duplicates(txt_18)

txt_18 <- unlist(txt_18)
txt_18 <- sapply(txt_18, function(x) gsub("\\t.*", "", x))
txt_18 <- sapply(txt_18, function(x) gsub("([^a-zA-Z0-9])", "", x))
names(txt_18) <- NULL

txt_08_18 <- list(unlist(txt_08), unlist(txt_18))


# Create corpus # 
new_corp <- VCorpus(VectorSource(txt_08_18))

# Create DTM
dtm_new <- DocumentTermMatrix(new_corp,
                              control=list(tolower=T,                          # Make all words lower case
                                           stopwords=T,                        # Remove all stopwords
                                           removePunctuation=T,                # Remove all symbols
                                           removeNumbers=T,                    # Remove all numbers
                                           stripWhitespace = T,                # Remove whitespace
                                           stemming = T,
                                           wordLengths = c(2,20))              # Min letters 
)


df <- data.frame(token=colnames(dtm_new), # subsetting only "old" 
                 count=colSums(as.matrix(dtm_new))) # documents from dtm_A

# dtm_new$v <- dtm_new$v / sqrt(row_sums(dtm_new^2))[dtm_new$i] # normalization ref. Hoberg and Philips Får ikke til å fungere


# Find cosine simularity
cos.sim <- function(A, B){ # Create function for cosine simularity
  sum(A*B)/sqrt(sum(A^2)*sum(B^2))
}

df_cosSim <- data.frame(cosine_similarity=NA, year=NA) # creating cosine simularity to store cosine simularity numbers

for(i in 1:dtm_new$ncol){
  df_cosSim[i,1] <- cos.sim(dtm_new[1, i],
                            dtm_new[2, i])
}

test <- cos.sim(dtm_new[1,],
                dtm_new[2,])







######### Gjør det samme som ovenfor, bare i funksjon ###########

dtm_allYears <- function(years_list){
  # Iterating through all years in years_list
  for(i in 1:length(years_list)){
    txt_all <- list() # list where vector of words from every year will be stored
    
    ind_new <- grep(paste0("-", years_list[i], "-"), filenames_full)
    filenames_new <- filenames_full[ind_new]
    txt_y <- list() # create list where IPO reports will be stored
    
    # Reading all reports for the respective year
    for (i in 1:length(filenames_new)){
      txt_y[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_new[i]))
    }
    
    # Removing duplicates with own developed function
    txt_y <- remove_duplicates(txt_y)
    
    # Unlisting and cleaning reports 
    txt_y <- unlist(txt_y) # list on years instead of single reports
    txt_y <- sapply(txt_y, function(x) gsub("\\t.*", "", x)) # remove word tags
    txt_y <- sapply(txt_y, function(x) gsub("([^a-zA-Z0-9])", "", x)) # remove noise
    names(txt_y) <- NULL # remove rownames
    
    # Combining vector of words from every year into one list
    list.append(txt_all, unlist(txt_y))
  }
  
    # Creating DTM
    dtm_new <- DocumentTermMatrix(VCorpus(VectorSource(txt_all)),
                                  control=list(tolower=T,                          # Make all words lower case
                                               stopwords=T,                        # Remove all stopwords
                                               removePunctuation=T,                # Remove all symbols
                                               removeNumbers=T,                    # Remove all numbers
                                               stripWhitespace = T,                # Remove whitespace
                                               stemming = T,
                                               wordLengths = c(2,20))              # Min letters 
    )
    
  return(dtm_new)  
  }
  

years <- c(2008,2009)
dtm_all <- dtm_allYears(years)
