library(markdown)

shinyUI(navbarPage("KF API Demo",
                   tabPanel("Login",
                            sidebarLayout(
                              sidebarPanel(
                                wellPanel(
                                  textInput("host", label = "Host", value = "http://kf.utoronto.ca:8080/kforum/"),
                                  textInput("username", label = "Username", value = "bodong"),
                                  textInput("password", label = "Password", value = "000000"),
                                  br(),
                                  actionButton("doLogin", label = "Login")
                                ),
                                uiOutput("selectSection")
                              ),
                              mainPanel(
                                htmlOutput("sectionInfo"),
                                hr(),
                                verbatimTextOutput("debug")
                              )
                            )
                   ),
                   navbarMenu("My Group",
                              tabPanel("Tool 1",
                                       h2("blabla")
                              )
                   ),
                   navbarMenu("Me",
                              tabPanel("Tool 2",
                                       h2("blabla")
                              )
                   ),
                   tabPanel("About",
                            tabPanel("About",
                                     fluidRow(
                                       column(5,
                                              includeMarkdown("about.md"),
                                              offset = 1
                                       ),
                                       column(3,
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
                   )
))