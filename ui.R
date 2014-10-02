### UI

require(markdown)
require(shiny)
require(shinyIncubator)
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
            selectInput("host",
                        label = "Host",
                        choices = c("Test server" = auth[1, 1], "Production server" = "http://kf.utoronto.ca:8080/kforum/")),
#             textInput("host", label = "Host", 
#                       value = auth[1, 1]), # value = "http://kf.utoronto.ca:8080/kforum/"
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
        sidebarPanel(
          htmlOutput("groupInfo"),
          uiOutput("selectView"),
          # uiOutput("selectView"),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            type = "pills",
            tabPanel(
              "Epistemic",
              h4("Scaffold Tracker"),
              showOutput("scaffoldTracker", "nvd3")
            ),
            tabPanel(
              "Social",
              includeHTML("www/js/network.force.js"),
              # tags$head(tags$script(src="js/network.force.js")),
              h3("Reading Network"),
              h4("Circle layout"),
              plotOutput("socialNetwork"),
              h4("Force-directed layout"),
              forceDirectedNetworkOutput("socialNetworkJS")
            ),
            tabPanel(
              "Temporal",
              h3("Writing Activities"), 
              showOutput("groupWriting", "nvd3"),
              h3("Vocabulary Growth"), 
              showOutput("groupWritingVocab", "nvd3")
            ),
            tabPanel(
              "Semantic",
              h4("Concept Cloud"),
              plotOutput("conceptCloud")
              #showOutput("semanticOverlap", "polycharts")
            )
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
        sidebarPanel(
          htmlOutput("myInfo"),
          htmlOutput("myOverviewStats"),
          uiOutput("selectView2"),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            type = "pills",
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
            tabPanel("Collaboration",
                     showOutput("myPostsTimeline", "timeline"))
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
          tags$script(type="text/javascript", src = "js/passwdInputBinding.js"),
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