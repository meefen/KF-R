### Server

# require(rChartsCalendar)
require(tm)
require(xtable)

source("utils/utils.R") # Load utility functions
source("lib/kf-api-lib.R") # Load KF API library

shinyServer(function(input, output, session) {
  
  ## Create a curl handle that will be shared among API calls
  curl = NULL
  ## Cache authorId
  authorId = NULL
  ## Cache sectionId
  sectionId = NULL
  
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
  
  getSectionViews <- reactive({
    ### Get section views
    
    if (input$doSelect == 0) {
      return(list())
    }
    
    isolate({
      GetSectionViews(input$host, input$sectionId, curl)
    })
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
                     "<li># of words: ", sum(CountWords(myPosts$body)), "</li></ul>")
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
  
  output$myPostsTerms <- renderTable({
    
    withProgress(session, {
      setProgress(message = "May take a while... please wait", 
                  detail = "Getting your posts...")
      myPosts <- getMySectionPosts()
      
      setProgress(detail = "Training corpus...")
      myNotes = Corpus(VectorSource(myPosts$body))
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
                     "<li># of words: ", sum(CountWords(posts$body)), "</li></ul>")
      HTML(html)
    })
    
  })
  
  output$scaffoldTracker <- renderChart({
    
    
  })
  
  getGroupPostTS <- reactive({
    
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
        tmp[idx, 4] = tmp[idx, 4] + CountWords(post$body)
        next
      }
      ## else
      idx = which(tmp$author == post$primaryAuthorId)
      x = 1; y = CountWords(post$body)
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
      d$xAxis(axisLabel = "Date")
      
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
      d$xAxis(axisLabel = "Date")
      
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
  
  
  
  output$socialNetwork <- renderPlot({
    
    
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
  
})