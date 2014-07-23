##########################
### Library for KF API ###
##########################

### API URLs
loginURL = "rest/account/userLogin/"
postsURL = "rest/content/getSectionPosts/"
viewsURL = "rest/content/getSectionViews/"
viewURL  = "rest/content/getView/"

### Load utility functions
source("utils/utils.R")

library(jsonlite)

CreateCurlHandle <- function() {
  ### Create a curl handle that will be shared among API calls
  
  library(RCurl)
  curl = getCurlHandle()
  curlSetOpt(cookiejar = "", 
             followlocation = TRUE, 
             curl = curl) # do not need to read the cookies
  return(curl)
}

Authenticate <- function(host, username, password, curl) {
  ### Authenticate user, and return authentication results
  
  loginURL = paste0(host, loginURL)
  auth = list(userName=username, password=password)
  tryCatch({
    fromJSON(
      postForm(loginURL, .params = auth, curl=curl, style="POST"),
      flatten = TRUE
    )
  }, error = function(e) {
    return(list())
  })
  
}

GetSectionPosts <- function(host, sectionId, curl) {
  ### Get posts in a section
  
  pURL = paste0(host, postsURL, sectionId)
  df = fromJSON(getURL(pURL, curl=curl), flatten = TRUE)
  df$body_text = StripHTMLTags(df$body)
  return(df)
}

GetSectionViews <- function(host, sectionId, curl) {
  ### Get views in a section
  
  vURL = paste0(host, viewsURL, sectionId)
  fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
}

GetView <- function(host, viewId, curl) {
  ### Get view info
  
  vURL = paste0(host, viewURL, viewId)
  view = fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
  view$viewPostRefs$postInfo$body_text = StripHTMLTags(view$viewPostRefs$postInfo$body)
  return(view)
}

GetLogs <- function(host, viewId, curl) {
  tryCatch({  
    vURL = paste0(host, "rest/mobile/getPostHistoriesForView/", viewId)
    fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
  }, error = function(e) {
    return(e)
  })
}

GetAllAuthors <- function(host, communityId, curl) {
  tryCatch({  
    vURL = paste0(host, "rest/mobile/getAllAuthors/", communityId)
    fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
  }, error = function(e) {
    return(e)
  })
}

SelectCommunity <- function(host, communityId, curl) {
  tryCatch({  
    vURL = paste0(host, "rest/account/selectSection/", communityId)
    fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
  }, error = function(e) {
    return(e)
  })
}


