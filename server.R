### Server

# require(rChartsCalendar)
require(tm)
require(xtable)
require(plyr)
require(reshape2)
require(ENA)
suppressMessages(require(dplyr))
require(shiny)
suppressMessages(require(shinyIncubator))

source("utils/utils.R") # Load utility functions
source("lib/kf-api-lib.R") # Load KF API library

shinyServer(function(input, output, session) {
  
  ## Create a curl handle that will be shared among API calls
  curl = NULL
  
  ## Cache authorId, sectionId
  authorId = NULL
  sectionId = NULL
  communityInfo = NULL
  selectedViews = NA

  ## Load apps
  apps = dir("apps", pattern = '^app', full = TRUE)
  for (app in apps) {
    source(app, local = TRUE)
  }
  
  # Store in a convenience variable
  cdata <- session$clientData
  
  # Values from cdata returned as text
  output$clientdataText <- renderText({
    cnames <- names(cdata)
    
    allvalues <- lapply(cnames, function(name) {
      paste(name, cdata[[name]], sep=" = ")
    })
    paste(allvalues, collapse = "\n")
  })
  
})