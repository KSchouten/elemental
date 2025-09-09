#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
source("utils.R")
source("page.R")
source("module.R")
source("modules/mod_slider.R")
source("modules/mod_histogram.R")
source("modules/mod_text.R")

# Define UI for application
ui <- tagList(
  
  # Use shinyjs
  shinyjs::useShinyjs(),
  # Use introjs
  rintrojs::introjsUI(),
  # Use shinyFeedback
  shinyFeedback::useShinyFeedback(),
  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    # Custom JS
    tags$head(tags$script(src = "script.js"))
  ),
  
  # Show loading screen when app is loading
  waiter::use_waiter(),
  waiter::waiter_show_on_load(html = waiter::spin_3k(), color = waiter::transparent(alpha = 0)),
  
  page_navbar(
    title = "Elemental",
    id = "page",
    lang = "en",
    gap = "5px",
    fillable = FALSE,
    
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(tags$a(shiny::icon("github"), "Shiny", href = "https://github.com/rstudio/shiny", target = "_blank")),
      nav_item(tags$a(shiny::icon("r-project"), "Posit", href = "https://posit.co", target = "_blank"))
    )
  )
  
)

# Define server logic
server <- function(input, output, session) {
  
  # Global variables
  #  database connection
  #  currently logged in user
  globals <- reactiveValues()
  
  # Open database connection
  
  # Check url for params
  
  # Autologin user (locally or with url) and load page setup
  # Currently loads default page setup from pages.json
  globals$user <- "test"
  globals$pages <- purrr::imap(jsonlite::read_json("pages.json"), function(page, id){
    Page$new(id, page$title, page$icon, globals, reactiveValues(
      !!!purrr::imap(page$modules, function(module, id){
        list(class = get(module$class), title=module$title, imports = module$imports)
      })
    ), page$layout)
  }) 
  
  # Preload allowed modules to save time on a lookup per module
  
  # Add pages to main UI
  observe({

    purrr::walk(rev(globals$pages), function(page){
      nav_insert("page", page$get_ui(), NULL, "before")
    })

  })
  

  # Obs. change in selected page -- start module for newly clicked pages
  observe({
    req(input$page)
    print(input$page)
    
    globals$current_page <- input$page
    
    
    # If not started yet, start the server function for the selected page
    # Should be one-time only
    if (!globals$pages[[input$page]]$get_active()){
      
      globals$pages[[input$page]]$start_server()

    }
    
  }) %>% bindEvent(input$page)
  
  # Select first page
  shinydashboard::updateTabItems(inputId = "page", selected = "id_page1")
  waiter::waiter_hide()
}

# Run the application 
shinyApp(ui = ui, server = server)
