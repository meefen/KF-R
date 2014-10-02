### Global variables shared across ui.R and server.R

require(shinyIncubator)
require(rCharts) # @dev
library(tm)
library(wordcloud)
library(memoise)

source("force_directed_network.R")

options(RCHART_LIB = c('polycharts', 'morris', "nvd3", "timeline", "dimple"))

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(text) {
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, tolower)
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  head(sort(rowSums(m), decreasing = TRUE), 50)
})