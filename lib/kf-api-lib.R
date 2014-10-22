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

suppressMessages(library(jsonlite))
suppressMessages(library(dplyr))

CreateCurlHandle <- function() {
  ### Create a curl handle that will be shared among API calls
  
  library(RCurl)
  curl = getCurlHandle()
  curlSetOpt(cookiejar = "", 
             followlocation = TRUE, 
             curl = curl) # do not need to read the cookies
  return(curl)
}

EnsureHost <- function(host) {
  ### Ensure host ends with '/'
  
  if(grepl("\\/$", host))
    host
  else
    paste0(host, "/")
}

Authenticate <- function(host, username, password, curl) {
  ### Authenticate user, and return authentication results
  
  host = EnsureHost(host)
  loginURL = paste0(host, loginURL)
  auth = list(userName=username, password=password)
  
  tryCatch({
    regs = fromJSON(
      postForm(loginURL, .params = auth, curl=curl, style="POST"),
      flatten = TRUE
    )
    tbl_df(regs) %>% arrange(desc(dateCreated))
  }, error = function(e) {
    return(list())
  })
}

SelectCommunity <- function(host, sectionId, curl) {
  ### Tell the server that we are going to use data from this community
  ### Required for any futher data queries
  
  host = EnsureHost(host)
  tryCatch({  
    vURL = paste0(host, "rest/account/selectSection/", sectionId)
    fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
  }, error = function(e) {
    return(e)
  })
}

GetSectionPosts <- function(host, sectionId, curl) {
  ### Get posts in a section
  
  host = EnsureHost(host)
  pURL = paste0(host, postsURL, sectionId)
  text = getURL(pURL, curl=curl)
  text = CleanJSONText(text)
#   text = StripHTMLTags(text)
  df = fromJSON(text, flatten = TRUE)
#   df = rjson::fromJSON(text)
  df$body_text = StripHTMLTags(df$body)
  return(df)
}

GetSectionViews <- function(host, sectionId, curl) {
  ### Get views in a section
  
  host = EnsureHost(host)
  vURL = paste0(host, viewsURL, sectionId)
  views = fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
  views$created = StrpKFTime(views$created) # convert time
  return(views)
}

GetView <- function(host, viewId, curl) {
  ### Get view info
  
  host = EnsureHost(host)
  vURL = paste0(host, viewURL, viewId)
  view = fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
  view$viewPostRefs$postInfo.body_text = StripHTMLTags(view$viewPostRefs$postInfo.body)
  return(view)
}

GetLogs <- function(host, viewIds, curl) {
  ### Get post histories from views
  ### Note: viewIds is a vector
  
  host = EnsureHost(host)
  tryCatch({
    logs = lapply(viewIds, function(viewId) {
      vURL = paste0(host, "rest/mobile/getPostHistoriesForView/", viewId)
      df = tryCatch(
        fromJSON(getURL(vURL, curl=curl), flatten=TRUE),
        error = function(e) NULL
      )
      if(!is.null(df) && ncol(df) == 5) 
        df$userName <- NA # strangely some views returned df with 5 cols
      return(df)
    })
    do.call("rbind", logs)
  }, error = function(e) {
    print(e)
  })
}

GetAllAuthors <- function(host, sectionId, curl) {
  ### Get all authors in a section / community
  
  host = EnsureHost(host)
  tryCatch({  
    vURL = paste0(host, "rest/mobile/getAllAuthors/", sectionId)
    fromJSON(getURL(vURL, curl=curl), flatten=TRUE)
  }, error = function(e) {
    return(e)
  })
}
