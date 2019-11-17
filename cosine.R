# Set current working directory
setwd('C:/Users/Yngve/Google Drive/Skolerelatert/NHH/Master/BAN432/Final exam')
load('common_unique_words.RData')
load('tok.RData') # tokenized list of all documents

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

# Funksjon av det som skjer nedenfor
# Creating function to create all DTMs
years_list1 <- c(2008:2010)
dtm_list <- list()
dtm_from_year <- function(years_list){
  for(j in 1:length(years_list)){
    # Find all IPOs from relevant year
    ind_new <- grep(pattern=paste0('-',as.character(years_list[j]),'-'), x = filenames_full)
    filenames_new <- filenames_full[ind_new]
    txt_new <- list()
    txt_list <- list()
    
    for (i in 1:length(filenames_new)){
      txt_new[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_new[i]))
    }
    txt_string <- unlist(txt_new)
    txt_string <- sapply(txt_string, function(x) gsub("\\t.*", "", x))
    txt_string <- sapply(txt_string, function(x) gsub("([^a-zA-Z0-9])", "", x))
    names(txt_string) <- NULL
    
    list.append(txt_list, unlist(txt_string))
  }
    
    # Create DTM of all words from this year
    dtm <- DocumentTermMatrix(VCorpus(VectorSource(txt_list)), # takes corpus as input
                              control=list(tolower=T,      # ensures that words are treated equally, no matter the case
                                           removePunctuation=T, # to omit all dollar signs, and other punctuation
                                           removeNumbers=T, # to omit numbers in the documents
                                           stemming = T
                              ))
    
    
    cat('Found DTM for ', as.character(years_list[j]), '...')
  return(dtm_list)
  }
  #dtm_list <- lapply(dtm_list, function(x) gsub("\\t.*", "", x=x))


####### Prøver med ett og ett år ######
filenames_full <- list.files(path= paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj"))

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
txt_08 <- remove_duplicates(txt_18)

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


# Find cosine simularity



################### Before using tok.RData file ###############################

#### Extract file names ####


# Creating function to create all DTMs
years_list1 <- c(2008:2010)
dtm_list <- list()
dtm_from_year <- function(years_list){
  for(j in 1:length(years_list)){
    # Find all IPOs from relevant year
    ind_new <- grep(pattern=paste0('-',as.character(years_list[j]),'-'), x = filenames_full)
    filenames_new <- filenames_full[ind_new]
    txt_new <- list()
    
    for (i in 1:length(filenames_new)){
      txt_new[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_new[i]))
    }
    txt_string <- unlist(txt_new)
    txt_string <- sapply(txt_string, function(x) gsub("\\t.*", "", x))
    txt_string <- sapply(txt_string, function(x) gsub("([^a-zA-Z0-9])", "", x))
    names(txt_string) <- NULL
    
    # Create DTM of all words from this year
    dtm <- DocumentTermMatrix(VCorpus(VectorSource(txt_string)),
                                control=list(tolower=T,      # ensures that words are treated equally, no matter the case
                                             removePunctuation=T, # to omit all dollar signs, and other punctuation
                                             removeNumbers=T, # to omit numbers in the documents
                                             bounds=list(
                                               global = c(5, 50) # terms in 5 to 50 of the documents
                                             )
                                ))
    dtm_list[[j]] <- dtm
    
    
  cat('Found DTM for ', as.character(years_list[j]), '...')
  }
  #dtm_list <- lapply(dtm_list, function(x) gsub("\\t.*", "", x=x))
  return(dtm_list)
}
test <- dtm_from_year(years_list1)





#################################
################################# Try from saturday
#################################


# Creating a list with tokanized lists for all years
txt_tok <- list()
dtm_from_year <- function(year){
  txt_new <- list()
  dtm_year <- list()  # list to store all DTMs
  for(j in 1:length(year)){       # Looping through all years specified
    # Download texts from each year  
    ind_new <- grep(pattern=paste0('-',as.character(year[j]),'-'), x = filenames_full)
    filenames_new <- filenames_full[ind_new]
  
  # Extract all files in working directory that match specified year
    for (i in 1:length(filenames_new)){
      txt_new[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_new[i]))
    }
    # Create DocumentTermMatrix
      dtm <- DocumentTermMatrix(VCorpus(VectorSource(txt_new)),
                                    control=list(tolower=T,                          # Make all words lower case
                                                 stopwords=T,                        # Remove all stopwords
                                                 removePunctuation=T,                # Remove all symbols
                                                 removeNumbers=T,                    # Remove all numbers
                                                 stripWhitespace = T)                 # Remove whitespace
      )
      dtm$v <- dtm$v / sqrt(row_sums(dtm^2))[dtm$i] # normalization ref. Hoberg and Philips
      
      list.append(dtm_year,dtm)
      
      
  #names(dtm_year)[j] <- year[j]  # name of list element in dtm_year is set to the respective year
  }
  return(dtm_year)
  cat('Found words from ', as.character(year[i]), '...')
  }

dtms <- dtm_from_year(c(2018,2019))


wordcloud()
