### UI

require(markdown)
require(ShinyDash)

## For debug: read default testing account info from a file
auth = tryCatch(
  read.table("auth.txt"),
  error = function(e) data.frame(c("", "")),
  warning = function(w)  data.frame(c("", ""))
)

passwdInput <- function(inputId, label, value="") {
  tagList(
    tags$label(label),
    tags$input(id = inputId, type="password", value=value)
  )
}

shinyUI(
  navbarPage(
    "KF Dash (v 0.01)",
    
    
    
    ### Login panel
    tabPanel(
      "Login",
      sidebarLayout(
        sidebarPanel(
          wellPanel(
            textInput("host", label = "Host", 
                      value = auth[1, 1]), # value = "http://kf.utoronto.ca:8080/kforum/"
            textInput("username", label = "Username", value = auth[2, 1]),
            passwdInput("password", label = "Password", value = auth[3, 1]),
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
            tags$head(tags$script(src="js/network.force.js")),
            uiOutput("selectView"),
            h3("Reading Network"),
            plotOutput("socialNetwork"),
            forceDirectedNetworkOutput("socialNetworkJS")
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
      # Add custom CSS & Javascript;
      tagList(
        tags$head(
          tags$link(rel="stylesheet", type="text/css", href="style.css"),
          tags$script(type="text/javascript", src = "passwdInputBinding.js"),
          tags$head(includeScript("www/js/google-analytics.js"))
        )
      ),
      tabPanel(
        "About",
        column(
          5,
          includeMarkdown("www/about.md"),
          offset = 1
        )
      )
    ),

    ### progressInit
    conditionalPanel(FALSE, # to hide progressInit()
                     progressInit()
    )
  ))