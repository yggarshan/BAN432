# Set current working directory
setwd('C:/Users/Yngve/Google Drive/Skolerelatert/NHH/Master/BAN432/Final exam')


library(dplyr)
library(tm)
library(qdapTools)
library(qdapRegex)
library(wordcloud)
library(plotrix)


#### Extract file names ####

filenames_full <- list.files(path= paste0(getwd(),"/ipos_2nd_qtr_2008_2019"))


#### New IPOs (2017-2019) ####

ind_new <- grep("-2019-|-2018-|-2017-", filenames_full)
filenames_new <- filenames_full[ind_new]

txt_new <- list()

for (i in 1:30){
  txt_new[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019/", filenames_new[i]))
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
                                           # add weighting here?
                                           bounds=list(
                                             global = c(round(length(new_corp)*0.05), # Terms in 5 to 50%
                                                        round(length(new_corp)*0.50))  # of the documents
                                           )
                              )
)

new_m <- as.matrix(dtm_new)



inspect(dtm_new[1:2,1:10])


#### Old IPOs (2008-2010) ####

ind_old <- grep("-2010-|-2009-|-2008-", filenames_full)
filenames_old <- filenames_full[ind_old]

txt_old <- list()

for (i in 1:30){
  txt_old[[i]] <- readLines(paste0(getwd(),"/ipos_2nd_qtr_2008_2019/", filenames_old[i]))
}

# Create corpus # 
old_corp <- VCorpus(VectorSource(txt_old))

# Create DTM
dtm_old <- DocumentTermMatrix(old_corp,
                              control=list(tolower=T,                          # Make all words lower case
                                           stopwords=T,                        # Remove all stopwords
                                           removePunctuation=T,                # Remove all symbols
                                           removeNumbers=T,                    # Remove all numbers
                                           stripWhitespace = T,                # Remove whitespace
                                           wordLengths = c(2,20),              # Min letters 
                                           # add weighting here?
                                           bounds=list(
                                             global = c(round(length(old_corp)*0.05), # Terms in 5 to 50%
                                                        round(length(old_corp)*0.50))  # of the documents
                                           )
                              )
)


inspect(dtm_old[1:2,1:10])





#                                                                             
##                                                                          
###                                                                        
#### Testing: Collapse new and old lists to two string vectors; new & old ####
###                                                                        
##                                                                          
#                                                                           

all_new <- paste(txt_new, collapse=" ")     # Combine all texts to one large string
all_new <- tolower(all_new)                 # Make all words lower case
all_new <- removeNumbers(all_new)           # Remove numbers
all_new <- removePunctuation(all_new)       # Remove symbols
all_new <- rm_nchar_words(all_new, "1,2")   # Remove words of 2 or less letters, PS: really slow, regex probably quicker



all_old <- paste(txt_old, collapse=" ")     # Combine all texts to one large string
all_old <- tolower(all_old)                 # Make all words lower case
all_old <- removeNumbers(all_old)           # Remove numbers
all_old <- removePunctuation(all_old)       # Remove symbols
all_old <- rm_nchar_words(all_old, "1,2")   # Remove words of 2 or less letters, PS: really slow, regex probably quicker


# Create Corpus
newold_corp <- VCorpus(VectorSource(c(all_new, all_old)))


# Term Document Matrix (comparison.cloud() requires TDM)
newold_tdm <- TermDocumentMatrix(newold_corp,
                                 control = list(stopwords = T,
                                                stripWhitespace = T,
                                                removeNumbers = T))
colnames(newold_tdm) <- c("NEW", "OLD")
tdm_m <- as.matrix(newold_tdm)

comparison.cloud(tdm_m, colors=c("orange", "blue"), max.words=100)
# The problem with this one is that it has too many common words, must apply something
# like the "bounds=" argument
# Could be done through rowSums(tdm_m) and only keep higher than x and lower than y
# head(rowSums(tdm_m))
