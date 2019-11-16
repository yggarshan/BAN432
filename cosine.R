# Set current working directory
setwd('C:/Users/Yngve/Google Drive/Skolerelatert/NHH/Master/BAN432/Final exam')

#### Extract file names ####
filenames_full <- list.files(path= paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj"))



year=c(2008:2019)
# Creating a list with tokanized lists for all years
txt_tok <- list()
words_from_years <- function(year){
  txt_new <- list()
  words_year <- list()
  words_list <- list()
  ind_new <- grep(paste0('-',as.character(year),'-'), filenames_full)
  
  filenames_new <- filenames_full[ind_new]
  
  # Extract all files in working directory that matches specified year(s)
  for (i in 1:length(filenames_new)){
    txt_new[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019_nouns_adj/", filenames_new[i]))
    
  # Tokanize list
    words_vector <- unlist(txt_new[[1]])             # Combine all texts to one large string
    words_vector <- sapply(words_vector, function(x) gsub("\\t.*", x=x, "")) # remove everything from \t and behind
    names(words_vector) = NULL                       # remove names (equal to the previous values)
    words_vector <- strsplit(words_vector, " ")      # Split words into vector of words
    words_list[[i]] <- words_vector # put vector of words into list
  }
  for(j in 1:length(year)){       # Looping through all years specified
    words_year[[j]] <- unlist(words_list)
    names(words_year)[j] <- year[j]
  }
  return(words_year)
  cat('Found words from ', as.character(year[i]), '...')
  }

words_year <- words_from_years(2019)
