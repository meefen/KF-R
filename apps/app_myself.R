### Myself

output$myInfo <- renderText({

  updateSelectedViews() # update selected views

  html <- paste0("<h3>", paste(communityInfo$currentAuthor$firstName,
                               communityInfo$currentAuthor$lastName),
                 "</h3><span>", communityInfo$userRole, "</span>")
  HTML(html)
})

output$myOverviewStats <- renderText({

  withProgress(session, {
    setProgress(message = "May take a while... please wait",
                detail = "Getting your posts...")
    myPosts <- getMySectionPosts()

    if(length(myPosts) == 0)
      return(HTML("<h4>Please select a community first!</h4>"))

    coAuthors = do.call("rbind", myPosts$authors)
    pIdeas = do.call("rbind", myPosts$promisingIdeas)

    html <- paste0("<p></p><pre><ul><li>", nrow(myPosts), " posts</li>",
                   "<li>", length(unique(coAuthors$guid))-1, " coauthors</li>",
                   "<li>", nrow(pIdeas), " promising ideas</li>",
                   "<li>", sum(CountWords(myPosts$body_text)), " words</li></ul></pre>")
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

output$readMe <- renderDataTable({
  ###

  myName = communityInfo$currentAuthor$userName
  readlogs = getReadLogs()
  print(readlogs)
  data.frame(
    readlogs %>%
      filter(to == myName) %>%
      group_by(from) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
  )
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
