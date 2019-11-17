# Set current working directory
setwd('C:/Users/Yngve/Google Drive/Skolerelatert/NHH/Master/BAN432/Final exam')

#### Extract file names ####
filenames_full <- list.files(path= paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj"))


# Creating function to create all DTMs
years_list <- c(2008:2019)
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
    test2 <- unlist(txt_new)
    
    # Create DTM of all words from this year
    dtm <- DocumentTermMatrix(VCorpus(VectorSource(txt_new)),
                                control=list(tolower=T,      # ensures that words are treated equally, no matter the case
                                             removePunctuation=T, # to omit all dollar signs, and other punctuation
                                             removeNumbers=T, # to omit numbers in the documents
                                             bounds=list(
                                               global = c(5, 50) # terms in 5 to 50 of the documents
                                             )
                                ))
    dtm_list[[j]] <- as.matrix(dtm)
    
  }
  return(dtm_list)
  cat('Found DTM for ', as.character(year[i]), '...')
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
