### Utils

StripHTMLTags <- function(htmlString) {
  ## Function to strip HTML tags and multiple spaces in post body
  tmp = gsub("<.*?>|\n|&nbsp;", " ", htmlString)
  return(gsub("^ *|(?<= ) | *$", "", tmp, perl=T))
}

CleanJSONText = function(text) {
  ## Clean json text that might cause lexical error when parsing
  
  text = gsub("\n|\t", "<br />", text)
  gsub("(\\\\)([a-zA-Z])", " \\2", text) # get rid of backslash
#   DumpText(text2)
}

StrpKFTime <- function(x, format = "%b %d, %Y %r") {
  ### convert KF time in str to POSIXlt
  
  as.character(strptime(x, format))
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

HTMLUL <- function(v) {
  ### Create a UL element from a string vector
  
  paste0("<ul>",
         paste0(paste0("<li>", v, "</li>"), collapse = ""),
         "</ul>")
}

CountWords <- function(str) {
  ### Roughly count words in a string
  
  sapply(gregexpr("\\W+", str), length) + 1
}

kf.sna.time = function(time){
  return(strptime(time, "%b %d, %Y %I:%M:%S %p"))
}

DumpText = function(str, file = "dump.txt") {
  ### Dump text to a file
  
  fileConn<-file(file)
  writeLines(text, fileConn)
  close(fileConn)
}

# CreateChordDiagram <- function(df.edges) {
#   ### Function to create ChordDiagram
#   ### Reference: http://mostlyconjecture.com/2014/05/03/chord-diagrams-with-rcharts/
#   ###
#   ### Params: 
#   ###     df.edges: a 2-column dataframe containing edges of the network
#   ###               column 1: source; column 2: target
#   
#   d <- df.edges
#   names(d) <- c("from", "to")
#   
#   require(igraph)
#   require(RColorBrewer)
#   require(plyr)
#   require(ggplot2)
#   require(doMC)
#   require(foreach)
#   require(rCharts)
#   
#   ChordDiagram = setRefClass('ChordDiagram', contains = 'rCharts', methods = list(
#     initialize = function(){
#       callSuper()
#       LIB <<- get_lib("../www/chord_diagram")
#       lib <<- "chord_diagram"
#       templates$script <<- '
#       <script type="text/javascript">
#       function draw{{chartId}}(){
#       var params = {{{ chartParams }}}
#       var chart = {{{ chordD }}}
#       
#       d3.select("#" + params.id) 
#       .datum({"data":{{{data}}}, "matrix":{{{matrix}}} })
#       .call(chart)
#       return chart;
#       };
#       
#       $(document).ready(function(){
#       draw{{chartId}}()
#       });
#       
#       </script>'
#     },
#     getPayload = function(chartId){
#       chordD = toChain(params[!(names(params) %in% c('dom', 'data', 'matrix'))], "d3.chordDiagram()")
#       chartParams = RJSONIO:::toJSON(params)
#       list(chordD = chordD, chartParams = chartParams, data=toJSONArray(params[['data']]),
#            matrix=toJSONArray(params[['matrix']]), chartId = chartId, lib = basename(lib), liburl = LIB$url
#       )
#     }
#   ))
#   
#   registerDoMC(4)
#   g <- graph.data.frame(d)
#   ## bipartite graphs require supplying new codes for sources and targets
#   ## since graph.bipartite checks that elements in one group don't appear in
#   ## the other.  since a node can be both source and target, the dataset as
#   ## is violates that rule.
#   sources <- unique(d$from)
#   targets <- unique(d$to)
#   from_id_dict <- data.frame(ids = (length(targets)+1):(length(sources) + length(targets)), 
#                              row.names = sources)
#   to_id_dict <- data.frame(ids = 1:length(targets), row.names = targets)
#   d$target <- to_id_dict[as.character(d$to), "ids"]
#   d$source <- from_id_dict[as.character(d$from), "ids"]
#   edgelist <- sapply(apply(d[, c("source", "target")], 1, "["), "[")
#   bg <- graph.bipartite(type = c(rep(0, length = length(targets)), rep(1, length = length(sources))), 
#                         edges = edgelist, directed = T)
#   targ_graph <- bipartite.projection(bg, which = "false")  # project second column
#   V(targ_graph)$sources <- sapply(1:length(V(targ_graph)), function(x) {
#     d$source[which(d$target == x)]
#   })
#   two_way_links <- foreach(v = V(targ_graph)$sources, .combine = rbind) %dopar% {
#     unlist(sapply(V(targ_graph)$sources, function(x) {
#       length(intersect(v, x))/length(x)
#     }))
#   }
#   diag(two_way_links) <- 0
#   row.names(two_way_links) <- 1:length(V(targ_graph))
#   colnames(two_way_links) <- 1:length(V(targ_graph))
#   
#   sum_indegree <- ddply(d, .(target), nrow)
#   #   sum_indegree <- sum_indegree[order(sum_indegree$V1, decreasing = T)[1:10], ]
#   sum_indegree$color <- brewer.pal(nrow(sum_indegree), name = "BrBG")
#   names(sum_indegree) <- c("name", "sum", "color")
#   sum_indegree_matrix <- matrix(two_way_links[sum_indegree$name, sum_indegree$name], dimnames = NULL, 
#                                 nrow = nrow(sum_indegree))
#   sum_indegree$name <- row.names(to_id_dict) # use name
#   ch <- ChordDiagram$new()
#   ch$params$data = sum_indegree
#   ch$params$matrix <- sum_indegree_matrix
#   ch$params$height <- 700
#   ch$params$width <- 700
#   return(ch)
# }

# GetSignificantTermsArray <- function(s) {
#   library(RCurl)
#   ch = getCurlHandle()
#   curlSetOpt(.opts = list(CURLOPT_URL = 'http://api.search.yahoo.com/ContentAnalysisService/V1/termExtraction',
#                   CURLOPT_POST = 1,
#                   CURLOPT_RETURNTRANSFER = 1,
#                   cookiejar = "", 
#                   followlocation = TRUE),
#              curl = ch)
#   
#   curl_setopt( $ch, CURLOPT_POSTFIELDS, 'appid=YOUR_APP_ID&context=' . urlencode($s) );
#   $xml = curl_exec($ch);
#   curl_close($ch);
#   
#   // A workaround deletes the Schema declarations, as they
#   // confuse PHP5
#   $xml = str_replace('xsi:schemaLocation="urn:yahoo:srch http://api.search.yahoo.com/ContentAnalysisService/V1/TermExtractionResponse.xsd"', ' ', $xml);
#   $xml = str_replace('xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="urn:yahoo:srch"', ' ', $xml);
#   
#   $arrTerms = array();
#   
#   // The nice native PHP XML functions
#   $dom = new domdocument;
#   $dom->loadXml($xml);
#   $xpath = new domxpath($dom);
#   $xNodes = $xpath->query('//Result');
#   $i = 0;
#   foreach ($xNodes as $xNode) {
#     $arrTerms[$i++] = $xNode->firstChild->data;
#   }
#   
#   return $arrTerms;
# }


# 
# # Using "memoise" to automatically cache the results
# getTermMatrix <- function(text) {
#   
#   library(tm)
#   library(wordcloud)
#   
#   myCorpus = Corpus(VectorSource(text))
#   myCorpus = tm_map(myCorpus, content_transformer(tolower))
#   myCorpus = tm_map(myCorpus, removePunctuation)
#   myCorpus = tm_map(myCorpus, removeNumbers)
#   myCorpus = tm_map(myCorpus, removeWords,
#                     c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
#   
#   myDTM = TermDocumentMatrix(myCorpus,
#                              control = list(minWordLength = 1))
#   
#   m = as.matrix(myDTM)
#   
#   sort(rowSums(m), decreasing = TRUE)
# }