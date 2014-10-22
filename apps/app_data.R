### Reactive data function

####################################
### Reactive expressions for API ###
####################################

auth <- reactive({
  ### Reactive expression to authenticate a user
  ### returns authentication results
  ###   NULL    - login never attempted
  ###   list()  - login failed
  ###   a df    - login succeeded
  
  if (input$doLogin == 0) {
    return(NULL)
  }
  
  isolate({
    curl <<- CreateCurlHandle() # create a new curl handle for each authentication
    Authenticate(input$host, input$username, input$password, curl)
  })
})

getSectionViews <- reactive({
  ### Get section views
  
  if (input$doSelect == 0) {
    return(list())
  }
  
  isolate({
    GetSectionViews(input$host, input$sectionId, curl)
  })
})

getSectionPosts <- reactive({
  ### Get all posts in a section
  
  if (input$doSelect == 0) {
    return(list())
  }
  
  GetSectionPosts(input$host, sectionId, curl)
})

getMySectionPosts <- reactive({
  ### Get my posts in a section
  
  if (input$doSelect == 0) {
    return(list())
  }
  
  posts <- getSectionPosts()
  FilterPostsByAuthors(posts, authorId)
})

getMyPostTS <- reactive({
  ### Get time series data of my posts
  
  posts <- getMySectionPosts()
  
  # sort posts first -- very important
  posts$date = as.Date(round(strptime(posts$created, "%b %d, %Y %I:%M:%S %p"), "day"))
  posts = posts[with(posts, order(date)), ]
  N <- 1e4  # some magic number, possibly an overestimate
  tmp <- data.frame(date = rep(as.Date(.leap.seconds[1]), N), 
                    post = rep(0, N),
                    vocab = rep(0, N), stringsAsFactors=FALSE)
  j = 1
  for(i in 1:nrow(posts)) {
    post = posts[i, ]
    date = post$date
    
    ## same date already in
    idx = which(tmp$date == date)
    if (length(idx) > 0) {
      tmp[idx, 2] = tmp[idx, 2] + 1
      tmp[idx, 3] = tmp[idx, 3] + CountWords(post$body_text)
      next
    }
    x = max(tmp$post) + 1
    y = max(tmp$vocab) + CountWords(post$body_text)
    tmp[j, ] <- list(date, x, y)
    j = j + 1
  }
  tmp = tmp[tmp$post != 0, ]
  tmp[with(tmp, order(date)), ]
})

getGroupPostTS <- reactive({
  ### Get time series data of group posts
  
  posts <- getSectionPosts()
  
  authors = unique(do.call("rbind", posts$authors)) # all authors (having duplicates)
  
  # sort posts first -- very important
  posts$date = as.Date(round(strptime(posts$created, "%b %d, %Y %I:%M:%S %p"), "day"))
  posts = posts[with(posts, order(date)), ]
  N <- 1e4  # some magic number, possibly an overestimate
  tmp <- data.frame(date = rep(as.Date(.leap.seconds[1]), N), 
                    author = rep("", N), post = rep(0, N),
                    vocab = rep(0, N), stringsAsFactors=FALSE)
  j = 1
  for(i in 1:nrow(posts)) {
    post = posts[i, ]
    date = post$date
    
    ## same author + same date already in
    idx = which(tmp$author == post$primaryAuthorId & tmp$date == date)
    if (length(idx) > 0) {
      tmp[idx, 3] = tmp[idx, 3] + 1
      tmp[idx, 4] = tmp[idx, 4] + CountWords(post$body_text)
      next
    }
    ## else
    idx = which(tmp$author == post$primaryAuthorId)
    x = 1; y = CountWords(post$body_text)
    if (length(idx) > 0) { ## same author already in
      x = max(tmp$post[idx]) + x
      y = max(tmp$vocab[idx]) + y
    }
    tmp[j, ] <- list(date, post$primaryAuthorId, x, y)
    j = j + 1
  }
  tmp = tmp[tmp$post != 0, ]
  tmp = merge(tmp, authors[, c("guid", "userName", "firstName")], 
              by.x = "author", by.y = "guid")
  tmp[with(tmp, order(userName, date)), ]
})

getAllAuthors <- reactive({
  ### Get all authors in a section
  
  if (input$doSelect == 0) {
    return(list())
  }
  
  GetAllAuthors(input$host, sectionId, curl)
})

getLogs <- reactive({
  ### Get logs in selected views
  
  GetLogs(input$host, selectedViews, curl)
})

getReadLogs <- reactive({
  
  posts <- getSectionPosts()
  authors <- getAllAuthors()
  logs <- getLogs()
  
  primaryAuthorIds <- posts$primaryAuthorId
  names(primaryAuthorIds) = unique(posts$guid)
  authornames <- authors$userName
  names(authornames) = unique(authors$guid)
  #print(logs)
  
  readlogs = subset(logs, operationType=="READ" & entityType=="POST")
  data.frame("from"=readlogs$userName, 
             "to"=authornames[primaryAuthorIds[readlogs$entityId]], 
             timestamp=kf.sna.time(readlogs$accessTime))
})

kf.sna.data <- reactive({
  ### Get SNA data
  
  readlogs = getReadLogs()
  df = data.frame("source"=readlogs$from, "target"=readlogs$to)
  
  df
})