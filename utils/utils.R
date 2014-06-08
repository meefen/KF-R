### Utils

StripHTMLTags <- function(htmlString) {
  ## Function to strip HTML tags and multiple spaces in post body
  tmp = gsub("<.*?>|\n|&nbsp;", " ", htmlString)
  return(gsub("^ *|(?<= ) | *$", "", tmp, perl=T))
}

CalendarHeatmap <- function(data, title="") {
  ## Create a calendar heatmap
  
  library(lattice)
  library(chron)
  tryCatch({
    source("utils/calendarHeat.R")
  }, error = function(e) {
    source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
  })
  # Plot as calendar heatmap
  calendarHeat(data$date, data$value, varname=title)
}

FilterPostsByAuthors <- function(posts, authorIds) {
  ### Filter posts by authors
  ###
  ### Params: 
  ###       posts - a data frame containing posts
  ###       authorIds - a string vector of author ids
  
  myPostsInd = sapply(posts$authors, function(df) {
    length(intersect(authorIds, df$guid)) > 0
  })
  posts[myPostsInd, ]
}