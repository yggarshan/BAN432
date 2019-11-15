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

#### Old IPOs (2009-2010) ####
ind_old <- grep("-2009-|-2010", filenames_full)
filenames_old <- filenames_full[ind_old]

txt_old <- list()

for (i in 1:length(filenames_old)){
  txt_old[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019/", filenames_old[i]))
}


# turn into one large string of words
all_new <- paste(txt_new, collapse=" ")     # Combine all texts to one large string
all_new <- tolower(all_new)                 # Make all words lower case
all_new <- removePunctuation(all_new)       # Remove symbols

############################# Assignment 2 ###########################


# split relevant part of the vector at "|"
edgar.index <- strsplit(edgar.index.raw[12:length(edgar.index.raw)], 
                        split = "|", 
                        fixed = T)
# make a matrix
edgar.index <- matrix( unlist(edgar.index), ncol = 5, byrow = T)


# make a data.frame
edgar.index <- data.frame(edgar.index, stringsAsFactors = F)

# column names
colnames(edgar.index) <- c("cik", "company.name", "form.type", "date.filed", "url")

# Limit to 10K
edgar.index <- edgar.index[edgar.index$form.type == "10-K", ]

# Extract sustainable companies from 'comp'
comp.sus <- comp[comp[,3]==T,]

# Limit to sustainable companies
edgar.sus <- edgar.index[edgar.index$cik %in% comp.sus$cik,]

# Define file path
edgar.sus$file.path <- paste0("tenK", 1:69, ".txt")

# Step 2: loop and download/save 

for(i in 1:nrow(edgar.sus)){
  # download.file
  download.file(url = paste0("https://www.sec.gov/Archives/", 
                             edgar.sus$url[i]), 
                destfile = edgar.sus$file.path[i])
}

##################################
####TASK 2########################
##################################

# We created a loop that sequentially,
#Reads in a text as tenK. 
#We assume that the actual 10K is located between the first time <TEXT> and </TEXT> are mentioned. Hence, we extract that part
#Then we remove all html-tags in the form of <html-tag>
#We remove all punctuation and all numbers
#We proceed to removing excessive whitespace: before, after and within a string.
#We remove empty strings
#Finally, we save the next clean text file 

for (i in 1:69) {
  tenK <- readLines(paste0("tenK",i,".txt"))
  
  First <- grep("<TEXT>",tenK)
  Last <- grep("</TEXT>", tenK)
  extract <- tenK[First[1]:Last[1]]
  
  no_html <- gsub("<.*?>","",extract)
  no_entities <- gsub("&.+?;","",no_html)
  no_punc  <- gsub("[^[:alnum:]]", " ", no_entities)
  no_numbers <- removeNumbers(no_punc)
  extract_final<- gsub("^\\s+|\\s+$","", no_numbers)
  extract_final <- gsub("\\s+"," ",extract_final)
  extract_final <- extract_final[extract_final != ""] 
  
  write.csv(extract_final, file = paste0("clean_10K_",i,".txt"), row.names=FALSE)
  print(i)
}


##################################
####TASK 3########################
##################################

# load in cleaned 10k text files into a list 

ten_K_text <- list()
for (i in 1:69){
  ten_K_text[[i]] <- readLines(paste0("clean_10K_",i,".txt"))
}

#Tokanize the list 

business.des <- sapply(ten_K_text, function(i) scan(text = i,
                                                    what = "character",
                                                    quote = ""))
#Remove unnecessary punctuation and apply tolower

business.des <- sapply(business.des, function(i) removePunctuation(i))
business.des <- sapply(business.des, function(i) tolower(i))

#Create a list of terms for the sustainability score

dic.env <- "sustainab|carbon|environment|eco system|pollut|emission|climate change|stakeholder|ecologic|recycle|warming|planet|community|waste|renewable|communit"

#Locate the terms from dic.env in business.des
index.env <- unique(sapply(business.des, function(i) unique(grep(dic.env, x = i ))))

#Create KWIC function
require(dplyr)
make.KWIC <- function(index.env, business.des, n, doc.nr){
  KWIC <- tibble(left = sapply(index.env,
                               function(i) {paste(business.des[(i-n):(i-1)], collapse = " ")}),
                 keyword = business.des[index.env], 
                 right = sapply(index.env,
                                function(i) {paste(business.des[(i+1):(i+n)], collapse = " ")}),
                 doc.nr = doc.nr,
                 position.in.text = index.env/(length(business.des)*0.01))
  return(KWIC)
}

#Run KWIC analysis and save results to a list 

result <- list()
for(i in 1:length(business.des)){
  result[[i]] <- make.KWIC(index.env[[i]],
                           business.des[[i]],
                           n = 5,
                           doc.nr = i)
}

#Look at the result
result[[1]]
result[[5]]