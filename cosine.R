# Set current working directory
setwd('C:/Users/Yngve/Google Drive/Skolerelatert/NHH/Master/BAN432/Final exam')
load('common_unique_words.RData')
load('tok.RData') # tokenized list of all documents


doc_08 <- lapply(tok_sample, function(x) grep('-2008-', names(tok_sample)))

which(grep('-2008-', names(tok_sample)), tok_sample)

doc_08 <- (tok_sample[grep('-2008-', names(tok_sample))])
doc_09 <- (tok_sample[grep('-2009-', names(tok_sample))])
doc_10 <- (tok_sample[grep('-2010-', names(tok_sample))])
doc_11 <- (tok_sample[grep('-2011-', names(tok_sample))])
doc_12 <- (tok_sample[grep('-2012-', names(tok_sample))])
doc_13 <- (tok_sample[grep('-2013-', names(tok_sample))])
doc_14 <- (tok_sample[grep('-2014-', names(tok_sample))])
doc_15 <- (tok_sample[grep('-2015-', names(tok_sample))])
doc_16 <- (tok_sample[grep('-2016-', names(tok_sample))])
doc_17 <- (tok_sample[grep('-2017-', names(tok_sample))])
doc_18 <- (tok_sample[grep('-2018-', names(tok_sample))])
doc_19 <- (tok_sample[grep('-2019-', names(tok_sample))])



names(tok_sample)


#Create index dictionary with words from 2008 (documents_by_years[[1]])
index_2008 <- paste(unlist(Final_tok[c(documents_by_years[[1]])]))

#save(documents_by_years, file ="documents_positions.RData")

#Either collapse all other years data into 1 file?

other_years_all <- list()

#Collapse into year data files
for (i in 2:length(documents_by_years)) {
  other_years_all[[i-1]]<- paste(unlist(Final_tok[c(documents_by_years[[i]])]))
}


difference_years_all <- list()

#Get a set of unique words, compared to 2008..done yearly
for (i in 2:length(documents_by_years)) {
  difference_years_all[[i-1]]<- other_years_all[[i-1]][c(which(!other_years_all[[i-1]] %in% index_2008))]
}



################### Before using tok.RData file ###############################

#### Extract file names ####
filenames_full <- list.files(path= paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj"))


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
