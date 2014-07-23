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