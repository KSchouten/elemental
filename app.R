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
source("page.R")
source("module.R")
source("modules/mod_slider.R")
source("modules/mod_histogram.R")

# Define UI for application
ui <- tagList(
  
  # Use shinyjs
  shinyjs::useShinyjs(),
  # Use introjs
  rintrojs::introjsUI(),
  # Use shinyFeedback
  shinyFeedback::useShinyFeedback(),
  
  # Show loading screen when app is loading
  waiter::use_waiter(),
  waiter::waiter_show_on_load(html = waiter::spin_3k(), color = waiter::transparent(alpha = 0)),
  
  # Define this page as a dashboard page to signal we're using the dashboard page plus format
  shinydashboardPlus::dashboardPage(
    title = "Elemental Demo",
    # Create dashboard header on top  
    header = shinydashboardPlus::dashboardHeader(
      tags$li(class = "dropdown",
              "Logo"
      ),
      tags$li(class = "dropdown",
              actionLink("account", label = "Inloggen", icon = icon("user", style = "padding-right: 10px;")))
    ),
    
    # Create our navigation menu
    sidebar = shinydashboardPlus::dashboardSidebar(
      width = 248,
      uiOutput("menu"),
      actionLink("toggle_sidebar",
                 label = "Klap het menu in",
                 icon = icon("angles-left"))
    ),
    
    # Create right sidebar (controlbar - contains the page and box settings)
    controlbar = shinydashboardPlus::dashboardControlbar(
      id = "controlbar",
      width = 300,
      overlay = FALSE,
      shinydashboardPlus::controlbarMenu(
        id = "sidemenu",
        shinydashboardPlus::controlbarItem(
          "Pagina",
          uiOutput("customize")
        ),
        shinydashboardPlus::controlbarItem(
          "Tegel",
          uiOutput("box_settings")
        ),
        shinydashboardPlus::controlbarItem(
          "Module",
          uiOutput("module_settings")
        ),
        shinydashboardPlus::controlbarItem(
          "",
          icon = icon("circle-info", id = "start_introtour") %>% tagAppendAttributes(class = "page-settings-introtour-icon")
        )
      )
    ),
    
    # Main body of dashboard
    body = shinydashboard::dashboardBody(
      
      uiOutput("tabs")
      
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
  # Currently loads defaulty page setup from pages.json
  globals$pages <- purrr::imap(jsonlite::read_json("pages.json", simplifyVector = TRUE), function(page, id){
    Page$new(id, page$title, page$icon, globals, reactiveValues(
      !!!purrr::imap(page$modules, function(module, id){
        list(class = get(module$class), imports = module$imports)
      })
    ))
  }) 
  
  # Preload allowed modules to save time on a lookup per module
  
  # Render main menu UI -- each page becomes a menuItem
  # Generate menuItems from the spec in pages
  output$menu <- renderUI({
    shinydashboard::sidebarMenu(
      id = "page",
      .list = purrr::map(globals$pages, ~.$get_menu_item()) %>% setNames(NULL)
    )
  }) 
  
  # Render main content UI -- each page becomes a tabItem, linked to the menuItems
  # Generate tabItems, linked to the menuItems, from the spec in page_spec
  output$tabs <- renderUI({
    # This is the shinydashboard::tabItems function, but without its built-in check
    #   This check fails on lists, even though the div function within can perfectly handle it
    div(class  = "tab-content",
        # Notice the hard-coded mod_page_ui here
        purrr::map(globals$pages, ~.$get_ui())
    )
  })
  
  # Render controlbar UI
  
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
