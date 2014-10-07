## Login

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
          communityInfo <<- SelectCommunity(input$host, regs[regs$sectionId == sectionId, "guid"], curl)
          views <- tbl_df(getSectionViews()) %>% filter(active == TRUE)
          newviews = arrange(views, desc(created)) %>% head(10)
          html <- paste0("<h2>Welcome ", regs$authorInfo.firstName[1], "!</h2><p>",
                         nrow(views), " views (below) have been created in this community ",
                         "from ", substr(min(views$created), 1, 10),
                         " to ", substr(max(views$created), 1, 10), ".</p><h5>Please select views you want to focus on:</h5>")
          HTML(html)
        }
      }
    }
  }
})

## Select views
output$selectView <- renderUI({
  ### Update KF view selection UI

  if (is.null(input$doSelect) || input$doSelect == 0) {
    return(NULL)
  }

  views <- getSectionViews()
  tmp <- list()
  tmp[ views$title ] <- views$guid

  conditionalPanel(
    input$doSelect > 0,
    verticalLayout(
      selectInput(inputId = "viewIds",
                  label = "Select views:",
                  choices = tmp,
                  selected = tmp,
                  multiple = TRUE,
                  selectize = TRUE,
                  width = '500px'),
      actionButton("doSelectView", label = "Okay!")
    )
  )
})

updateSelectedViews <- reactive({

  if (is.null(input$doSelectView) || input$doSelectView == 0) {
    return(NULL)
  }

  selectedViews <<- input$viewIds
})
