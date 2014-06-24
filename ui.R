### UI

require(markdown)

## For debug: read default testing account info
auth = tryCatch(
  read.table("auth.txt"),
  error = function(e) data.frame(c("", "")),
  warning = function(w)  data.frame(c("", ""))
)

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
                      value = "http://132.203.154.41:8080/kforum/")
            #                       value = "http://kf.utoronto.ca:8080/kforum/")
            ,textInput("username", label = "Username", value = auth[1, 1])
            ,textInput("password", label = "Password", value = auth[2, 1])
            ,br()
            ,actionButton("doLogin", label = "Login")
          )
          ,uiOutput("selectSection")
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
        "input.doLogin > 0", # DEBUG
        navlistPanel(
          "Group Analytics",
          tabPanel(
            "Summary",
            htmlOutput("groupInfo")
          ),
          "-----",
          tabPanel(
            "ScaffoldTracker",
            showOutput("scaffoldTracker", "nvd3")
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
            h3("TODO"),
            showOutput("semanticOverlap", "polycharts")
          ),
          "-----",
          tabPanel(
            "Social Network",
            h3("TODO"),
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
        "input.doLogin > 0", # DEBUG
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
                       showOutput("myPostsTS", "morris"),
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
                       showOutput("myPostsVocabTS", "morris"),
                       h4("Top Terms"),
                       tableOutput("myPostsTerms")),
              tabPanel("Timeline",
                       showOutput("myPostsTimeline", "timeline"))
            )
          ),
          
          tabPanel(
            "Collaboration",
            h3("TODO")
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
            includeMarkdown("www/about.md"),
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