### Group

output$groupInfo <- renderText({

  updateSelectedViews() # update selected views

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

    html <- paste0("<h4>",communityInfo$section$title, "</h4><pre><ul>",
                   "<li>", length(unique(authorIds)), " authors</li>",
                   "<li>", nrow(posts), " notes</li>",
                   "<li>", length(pIdeaIds), " promising ideas</li>",
                   "<li>", sum(CountWords(posts$body_text)), " words</li></ul></pre>")
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

# Make the wordcloud drawing predictable during a session
wordcloud_rep <- repeatable(wordcloud)

output$conceptCloud <- renderPlot({

  print(selectedViews)

  posts <- getSectionPosts()

  v <- getTermMatrix(posts$body_text)
  wordcloud_rep(names(v), v, scale=c(4,0.5), rot.per = 0,
                colors=brewer.pal(8, "Dark2"))
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

  return(NULL)
})

output$socialNetwork <- renderPlot({

  isolate({
    if(is.na(selectedViews))
      return(list())

    library(igraph)
    g <- graph.data.frame(kf.sna.data(), directed=TRUE)
    plot(g, layout=layout.kamada.kawai,
         vertex.size=15, vertex.label.color="black", vertex.color="red",
         edge.arrow.size=0.5, edge.curved=F)
  })
})

output$socialNetworkJS <- renderForceDirectedNetwork({

  if(is.na(selectedViews))
      return(list())

  library(igraph)
  g <- graph.data.frame(kf.sna.data(), directed=TRUE)
  #print(as.matrix(get.adjacency(g)))
  as.matrix(get.adjacency(g))
})
