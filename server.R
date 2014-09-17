### Server

# require(rChartsCalendar)
require(tm)
require(xtable)
require(plyr)
require(reshape2)

source("utils/utils.R") # Load utility functions
source("lib/kf-api-lib.R") # Load KF API library

shinyServer(function(input, output, session) {
  
  ## Create a curl handle that will be shared among API calls
  curl = NULL
  
  ## Cache authorId, sectionId
  authorId = NULL
  sectionId = NULL
  
  ###############################################
  ###             Output functions            ###
  ### dependent on reactive expressions below ###
  ###############################################
  
  output$selectSection <- renderUI({
    ### Update KF section selection UI
    ### based on authentication results
    
    regs <- auth()
    
    if (length(regs) == 0) { # before login or when login fails
      return(NULL)
    } else {
      tmp <- list()
      tmp[ regs$sectionTitle ] <- regs$sectionId
      
      wellPanel(
        selectInput(inputId = "sectionId", 
                    label = "Choose a Knowledge Building Community:", 
                    choices = tmp),
        actionButton("doSelect", label = "Go!")
      )
    }
  })
  
  output$sectionInfo <- renderText({
    ### Update section info when a section is selected
    
    regs <- auth()
    
    if (is.null(regs))
      HTML("Please login!")
    
    else {
      if (length(regs) == 0) {
        HTML("Login failed. Please try again!")
      } else {
        authorId <<- regs$authorInfo.guid[1]
        
        if (is.null(input$doSelect)) {
          HTML(paste0("<h2>Welcome ", regs$authorInfo.firstName[1], "!</h2>",
                      "Login success! Please choose a community on the left-side panel."))
        } else {
          if(input$doSelect == 0) {
            HTML(paste0("<h2>Welcome ", regs$authorInfo.firstName[1], "!</h2>",
                        "Login success! Please choose a community on the left-side panel."))
          } else {
            sectionId <<- input$sectionId
            SelectCommunity(input$host, regs[regs$sectionId == sectionId, "guid"], curl)
            views <- getSectionViews()
            html <- paste0("<h2>Welcome ", regs$authorInfo.firstName[1], "!</h2><p>",
                           nrow(views), " views has been created in this community ",
                           "from ", substr(min(views$created), 1, 12), 
                           " to ", substr(max(views$created), 1, 12), "</p>",
                           HTMLUL(views$title),
                           "<h5>Please proceed to analytic tools through the navigation bar.</h5>")
            HTML(html)
          }
        }
      }
    }
  })
  
  output$myPostsInfo <- renderText({
    
    withProgress(session, {
      setProgress(message = "May take a while... please wait", 
                  detail = "Getting your posts...")
      myPosts <- getMySectionPosts()
      
      if(length(myPosts) == 0)
        return(HTML("<h4>Please select a community first!</h4>"))
      
      coAuthors = do.call("rbind", myPosts$authors)
      pIdeas = do.call("rbind", myPosts$promisingIdeas)
      
      html <- paste0("<h2>Summary of My Posts</h2><ul>",
                     "<li># of posts: ", nrow(myPosts), "</li>",
                     "<li># of coauthors: ", length(unique(coAuthors$guid))-1, "</li>",
                     "<li># of promising ideas: ", nrow(pIdeas), "</li>",
                     "<li># of words: ", sum(CountWords(myPosts$body_text)), "</li></ul>")
      HTML(html)
    })
  })
  
  output$myPostsCompare <- renderChart({
    
    withProgress(session, {
      setProgress(message = "May take a while... please wait", 
                  detail = "Getting your posts...")
      posts <- getSectionPosts()
      myPosts <- getMySectionPosts()
      
      authors = do.call("rbind", posts$authors) # all authors (having duplicates)
      tmp = data.frame(author=factor(c("Me","Average"), levels=c("Me","Average")),
                       notes=c(nrow(myPosts), nrow(posts)/length(unique(authors$guid))))
      
      setProgress(detail = "Plotting...")
      p <- rPlot(x = list(var="author", sort="value"), 
                 y = "notes", color = "author", 
                 data = tmp, type = "bar")
      p$addParams(width = 600, dom = "myPostsCompare", 
                  title = "Number of my posts compared to community average")
      p
    })
  })
  
  
  
  output$myPostsTS <- renderChart({
    ### Time series of my posts
    
    withProgress(session, {
      setProgress(message = "May take a while... please wait", 
                  detail = "Compiling time series...")
      
      tmp = getMyPostTS()
      
      setProgress(detail = "Plotting...")
      
      tmp$date = as.character(tmp$date)
      d <- mPlot(
        post ~ date,
        data = tmp,
        type = 'Line'
      )
      d$set(lineWidth = 1)
      
      d$addParams(dom = "myPostsTS")
      d
    })
  })
  
  output$myPostsCalendar <- renderPlot({
    
    withProgress(session, {
      setProgress(message = "May take a while... please wait", 
                  detail = "Getting your posts...")
      myPosts <- getMySectionPosts()
      
      dates = strptime(myPosts$created, "%b %d, %Y %I:%M:%S %p")
      dates_str = as.character(format(dates, format="%Y-%m-%d"))
      tmp = data.frame(table(dates_str))
      names(tmp) = c("date", "value")
      
      setProgress(detail = "Plotting...")
      CalendarHeatmap(tmp, title="Posting Activities")
    })
  })
  
  output$myPostsVocabTS <- renderChart({
    
    withProgress(session, {
      setProgress(message = "May take a while... please wait", 
                  detail = "Compiling time series...")
      
      tmp = melt(getMyPostTS(), id = "date")
      
      setProgress(detail = "Plotting...")
      
      tmp$date = as.character(tmp$date)
      d <- mPlot(
        value ~ date,
        data = tmp,
        group = "variable",
        type = 'Line'
      )
      d$set(lineWidth = 1)
      
      d$addParams(dom = "myPostsVocabTS")
      d
    })
  })
  
  output$myPostsTerms <- renderTable({
    
    withProgress(session, {
      setProgress(message = "May take a while... please wait", 
                  detail = "Getting your posts...")
      myPosts <- getMySectionPosts()
      
      setProgress(detail = "Training corpus...")
      myNotes = Corpus(VectorSource(myPosts$body_text))
      myDtm <- DocumentTermMatrix(myNotes, control = list(
        #   stemming = TRUE, 
        stopwords = TRUE, minWordLength = 3, 
        removeNumbers = TRUE, removePunctuation = TRUE))
      myFreqTerms = head(findFreqTerms(myDtm), 10)
      myFreq = colSums(inspect(myDtm[, myFreqTerms]))
      
      tmp = sort(myFreq, decreasing=TRUE)
      df = data.frame(term=names(tmp), freq=as.integer(tmp), row.names=NULL)
      xtable(df, display=c("d", "s", "d"))
    })
  })
  
  output$myPostsTimeline <- renderChart({
    
    withProgress(session, {
      setProgress(message = "May take a while... please wait", 
                  detail = "Getting your posts...")
      posts <- getMySectionPosts()
      posts$date = as.Date(round(strptime(posts$created, "%b %d, %Y %I:%M:%S %p"), "day"))
      posts = posts[with(posts, order(date)), ]
      
      d4 <- alply(posts, 1, function(x){
        list(
          startDate = gsub("-", ",", as.character(x$date)),
          headline = x$title,
          text = x$body,
          asset = list()
        )
      })
      
      # Create Timeline
      m = Timeline$new()
      m$main(
        headline =  "My timeline",
        type = 'default',
        text = "Using this timeline, you can review all your posts in their chronological order.",
        startDate =  d4[1][[1]]$startDate,
        asset = list(media = 'http://teachbytes.files.wordpress.com/2014/01/timeline-clipart.jpg')
      )
      m$config(
        font = "Merriweather-Newscycle"
      )
      names(d4) <- NULL
      m$event(d4)
      m$addParams(dom = "myPostsTimeline")
      #       m$save("www/embed/myPostTimeline.html")
      #       m$save(paste0("embed/myPostTimeline_", authorId, ".html"))
      m
    })
  })
  
  output$groupInfo <- renderText({
    
    withProgress(session, {
      setProgress(message = "May take a while... please wait", 
                  detail = "Getting community posts...")
      
      posts <- getSectionPosts()
      
      if(length(posts) == 0)
        return(HTML("<h4>Please select a community first!</h4>"))
      
      authorIds = unlist(sapply(posts$authors, function(df) 
        return(df$guid)
      ))
      pIdeaIds = unlist(sapply(posts$promisingIdeas, function(df)
        return(df$guid)
      ))
      
      html <- paste0("<h2>Summary of My Community</h2><ul>",
                     "<li># of posts: ", nrow(posts), "</li>",
                     "<li># of authors: ", length(unique(authorIds)), "</li>",
                     "<li># of promising ideas: ", length(pIdeaIds), "</li>",
                     "<li># of words: ", sum(CountWords(posts$body_text)), "</li></ul>")
      HTML(html)
    })
    
  })
  
  output$scaffoldTracker <- renderChart({
    
    ### Note: scaffolds are saved with CSS class: "startLabelTag"
    
    posts <- getSectionPosts()
    
    scaffolds <- unlist(alply(posts, 1, function(x) {
      r <- regexpr("class=\"startLabelTag\">(.*?)</span>", x$body)
      m <- regmatches(x$body, r)
      gsub("class=\"startLabelTag\">|</span>", "", m)
    }))
    
    tmp = data.frame(table(scaffolds))
    tmp = tmp[with(tmp, order(-Freq)), ]
    
    #     d1 <- dPlot(
    #       x = "Freq", 
    #       y = "scaffolds", 
    #       data = tmp, 
    #       type = 'bar') 
    #     d1$xAxis(type = "addMeasureAxis")
    #     d1$yAxis(type = "addCategoryAxis", orderRule = "Freq", horizontalAlign = "right")
    #     d1$addParams(dom = "scaffoldTracker")
    #     d1
    
    n1 <- nPlot(Freq ~ scaffolds, data = tmp, color="scaffolds", type = "multiBarHorizontalChart")
    n1$chart(
      showControls = FALSE, 
      showLegend = FALSE,
      margin = list(left = 6 * max(nchar(scaffolds))),
      tooltipContent = "#! function(key, x, y) { 
      return x + ': ' + Math.round(y) 
  } !#"
    )
    n1$yAxis(axisLabel = "Frequency",
             tickFormat="#! function(d) {return d3.format(',f')(d);} !#")
    n1$addParams(dom = "scaffoldTracker")
    n1
  })
  
  
  output$groupWriting <- renderChart({
    
    withProgress(session, {
      setProgress(message = "May take a while... please wait", 
                  detail = "Compiling time series...")
      
      tmp = getGroupPostTS()
      
      
      setProgress(detail = "Plotting...")
      
      d <- nPlot(
        post ~ date,
        data = tmp,
        group = "firstName",
        type = 'lineChart'
      )
      d$yAxis(axisLabel = "Post Count")
      d$xAxis(
        axisLabel = "Date",
        tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#"
      )
      
      #       tmp$date = as.character(tmp$date)
      #       d <- mPlot(
      #         post ~ date,
      #         data = tmp,
      #         group = "firstName",
      #         type = 'Line'
      #       )
      #       d$set(lineWidth = 1)
      
      d$addParams(dom = "groupWriting")
      d
    })
  })
  
  output$groupWritingVocab <- renderChart({
    
    withProgress(session, {
      setProgress(message = "May take a while... please wait", 
                  detail = "Compiling time series...")
      
      tmp = getGroupPostTS()
      
      
      setProgress(detail = "Plotting...")
      
      d <- nPlot(
        vocab ~ date,
        data = tmp,
        group = "firstName",
        type = 'lineChart'
      )
      d$yAxis(axisLabel = "Vocabulary Count")
      d$xAxis(
        axisLabel = "Date",
        tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#"
      )
      
      #       tmp$date = as.character(tmp$date)
      #       d <- mPlot(
      #         vocab ~ date,
      #         data = tmp,
      #         group = "firstName",
      #         type = 'Line'
      #       )
      #       d$set(lineWidth = 1)
      
      d$addParams(dom = "groupWritingVocab")
      d
    })
  })
  
  output$semanticOverlap <- renderChart({
    
    
  })
  
  output$selectView <- renderUI({
    ### Update KF view selection UI
    
    if (length(regs) == 0) { # before login or when login fails
      return(NULL)
    } else {
      views <- getSectionViews()
      tmp <- list()
      tmp[ views$title ] <- views$guid
      
      wellPanel(
        selectInput(inputId="viewIds", choices=tmp, label="Select views:", multiple=TRUE),
        actionButton("doSelectView", label="Go!")
      )
    }
  })
  
  output$socialNetwork <- renderPlot({
    
    if(is.null(input$viewIds) | length(input$viewIds) == 0)
      return(list())
    
    library(igraph)
    g <- graph.data.frame(kf.sna.data(), directed=TRUE)
    plot(g, layout=layout.circle, 
         vertex.size=15, vertex.label.color="black", vertex.color="red", 
         edge.arrow.size=0.5, edge.curved=F)
  })
  
  output$socialNetworkJS <- renderForceDirectedNetwork({
    
    if(is.null(input$viewIds) | length(input$viewIds) == 0)
      return(list())
    
    library(igraph)
    g <- graph.data.frame(kf.sna.data(), directed=TRUE)
    #print(as.matrix(get.adjacency(g)))
    as.matrix(get.adjacency(g))
  })
  
  #   output$myPostsCalendar <- renderChart({
  #     
  #     input$doSelect
  #     
  #     withProgress(session, {
  #       setProgress(message = "Creating calendar map...", 
  #                   detail = "Getting posts...")
  #       
  #       myPosts <- getMySectionPosts()
  #       
  #       dates = strptime(myPosts$created, "%b %d, %Y %I:%M:%S %p")
  #       dates_str = as.character(format(dates, format="%Y-%m-%d"))
  #       tmp = data.frame(table(dates_str))
  #       names(tmp) = c("date", "value")
  #       
  #       p2 <- plotCalMap(
  #         x = "date", y = "value",
  #         data = tmp,
  #         domain = 'month', 
  #         start = min(as.character(tmp$date)),
  #         legend = seq(10, 50, 10),
  #         itemName = 'point',
  #         range = 7
  #       )
  #       p2$addParams(height = 300, dom = 'myPostsCalendar')
  #       return(p2)
  #     })
  #   })
  
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
    
    if (input$doSelectView == 0) {
      return(list())
    }
    
    df = GetLogs(input$host, input$viewIds, curl)
    return(df)
  })
  
  kf.sna.data <- reactive({
    ### Get SNA data
    
    if(is.null(input$viewIds) | length(input$viewIds) == 0)
      return(list())
    
    posts <- getSectionPosts()
    authors <- getAllAuthors()
    logs <- getLogs()
    
    primaryAuthorIds <- posts$primaryAuthorId
    names(primaryAuthorIds) = unique(posts$guid)
    authornames <- authors$userName
    names(authornames) = unique(authors$guid)
    #print(logs)
    
    readlogs = subset(logs, operationType=="READ" & entityType=="POST")
    readlogs = data.frame("from"=readlogs$userName, 
                          "to"=authornames[primaryAuthorIds[readlogs$entityId]], 
                          timestamp=kf.sna.time(readlogs$accessTime))
    df = data.frame("source"=readlogs$from, "target"=readlogs$to)
    
    df
  })
  
})