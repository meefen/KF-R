
source("utils/utils.R") # Load utility functions
source("lib/kf-api-lib.R") # Load KF API library

## Create a curl handle that will be shared among API calls
curl = NULL

## Cache sectionId
sectionId = NULL

shinyServer(function(input, output) {
  
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
  
  output$debug <- renderPrint({
    sectionId
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
                    label = "Select Section", 
                    choices = tmp),
        actionButton("doSelect", label = "Go!")
      )
    }
  })
  
  getSectionViews <- reactive({
    ### Get section views
    
    if (input$doSelect == 0) {
      return("")
    }
    
    isolate({
      sectionId <<- input$sectionId
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
        if(input$doSelect == 0) {
          HTML("Login success! Please choose a section on the left-side panel.")
        } else {
          views <- getSectionViews()
          html <- paste0("<h3>", nrow(views), " views in the section</h2>", 
                         HTMLUL(views$title))
          HTML(html)
        }
      }
    }
#     wellPanel("",
#               h2("Views:"),
#               tags$small(views$title)
#     )
  })
  
  
})