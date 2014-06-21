### UI

require(markdown)

shinyUI(
  navbarPage(
    "KF API Demo",
    
    ### Login panel
    tabPanel(
      "Login",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            textInput("host", label = "Host", 
#                       value = "http://132.203.154.41:8080/kforum/"),
                      value = "http://kf.utoronto.ca:8080/kforum/"),
            textInput("username", label = "Username", value = "bodong"),
            textInput("password", label = "Password", value = "000000"),
            br(),
            actionButton("doLogin", label = "Login")
          ),
          uiOutput("selectSection")
        ),
        mainPanel(
          htmlOutput("sectionInfo")
        )
      )
    ),
    
    ### Group analytics panel
    tabPanel(
      "Group",
      conditionalPanel(
        "input.doLogin == 0", # TODO: add more strict control here
        h4("Please login first!")
      ),
      conditionalPanel(
        TRUE,#"input.doLogin > 0", # DEBUG
        navlistPanel(
          "Group Analytics",
          tabPanel(
            "Summary",
            htmlOutput("groupInfo")
          ),
          "-----",
          tabPanel(
            "ScaffoldTracker",
            showOutput("scaffoldTracker", "morris")
          ),
          "-----",
          tabPanel(
            "Writing",
            tabsetPanel(
              type = "pills", 
              tabPanel("Writing Activities", showOutput("groupWriting", "nvd3")),
              tabPanel("Vocabulary Growth", showOutput("groupWritingVocab", "nvd3"))
            )
          ),
          tabPanel(
            "Semantic Overlap",
            showOutput("semanticOverlap", "polycharts")
          ),
          "-----",
          tabPanel(
            "Social Network",
            plotOutput("socialNetwork")
          )
        )
      )
    ),
    
    ### Individual analytics panel
    tabPanel(
      "Myself",
      conditionalPanel(
        "input.doLogin == 0",
        h4("Please login first!")
      ),
      conditionalPanel(
        TRUE,#"input.doLogin > 0", # DEBUG
        navlistPanel(
          "My Contributions",
          
          tabPanel(
            "Posts",
            tabsetPanel(
              type = "pills", 
              tabPanel("Summary", htmlOutput("myPostsInfo")),
              tabPanel("How many did I post?",
                       showOutput("myPostsCompare", "polycharts")),
              tabPanel("When did I post?",
                       h4("Time Series"),
                       showOutput("myPostsTS", "nvd3"),
                       h4("Calendar view"),
                       plotOutput("myPostsCalendar")),
#               tabPanel("Posting Activities",
#                        h4("Compare with community average"),
#                        showOutput("myPostsCompare", "polycharts"),
#                        h4("Time Series"),
#                        showOutput("myPostsTS", "nvd3"),
#                        h4("Calendar view"),
#                        plotOutput("myPostsCalendar")
#               ),
              # chartOutput("myPostsCalendar", lib = "calmap", package = "rChartsCalendar"),
              tabPanel("What did I post?",
                       h4("Vocabulary Growth"),
                       showOutput("myPostsVocabTS", "nvd3"),
                       h4("Top Terms"),
                       tableOutput("myPostsTerms")),
              tabPanel("Timeline",
                       showOutput("myPostsTimeline", "timeline"))
            )
          ),
          
          tabPanel(
            "Collaboration",
            h3("Collaboration with others...")
          )
        )
      )
    ),
    
    ### About panel
    tabPanel(
      "About",
      tabPanel(
        "About",
        fluidRow(
          column(
            5,
            includeMarkdown("about.md"),
            offset = 1
          ),
          column(
            3,
            img(class="img-polaroid",
                src="http://blog.theironyard.com/wp-content/uploads/2014/01/cute-unicorn.jpg"),
            tags$small(
              "Source: ",
              a(href="http://blog.theironyard.com/we-need-more-unicorns/",
                "The Iron Yard")
            )
          )
        )
      )
    ),
    conditionalPanel(FALSE, # to hide progressInit()
                     progressInit()
    )
  ))