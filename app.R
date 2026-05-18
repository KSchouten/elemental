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
library(bslib)
source("utils.R")
source("module.R")
list.files("modules", full.names = TRUE) %>% purrr::walk(source)
list.files("ui", full.names = TRUE) %>% purrr::walk(source)

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
    tags$head(tags$script(src = "split.js")),
    tags$head(tags$script(src = "script.js"))
  ),
  
  # Show loading screen when app is loading
  waiter::use_waiter(),
  waiter::waiter_show_on_load(html = waiter::spin_3k(), color = waiter::transparent(alpha = 0)),
  
  elemental_page_navbar(
    title = "Elemental Experiment",
    id = "page",
    lang = "en",
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
  config <- jsonlite::read_json("pages.json")
  globals$modules <- purrr::imap(config$modules, function(module, id){
    get_class(module$class)$new(id, module$title, globals, purrr::map(module$imports, unlist))
  })  
  
  globals$elements <- list()
  globals$pages <- purrr::map(config$pages, function(layout){
    ElementalPage$new(layout, globals)
  }) %>% setNames(purrr::map_chr(., ~.$get_id()))

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
      
      globals$pages[[input$page]]$complete_ui_reactive(input, output, session)
      
    }
    
  }) %>% bindEvent(input$page)

  #handling moving tiles
  observe({
    req(input$move_tile)
    print(input$move_tile)
    
    from_column <- globals$elements[[input$move_tile$from_column]]
    to_column <- globals$elements[[input$move_tile$to_column]]
    
    tile <- from_column$remove_tile(input$move_tile$from_index+1)
    to_column$add_tile(tile, input$move_tile$to_index)
    tile$set_parent(to_column)
    
    # serialize!
  })
  
  # Handling moving modules
  observe({
    req(input$move_module)
    print(input$move_module)
    
    if (input$move_module$from_tile != input$move_module$to_tile){
      # need extra code to move modules between tiles
      
      # in JS the dragged tab-head is already removed, otherwise nav_insert won't work properly
      #   we cannot use the select parameter (it doesn't work), probably because the inserted tab does not yet fully exist until we run the JS code below
      nav_insert(id = input$move_module$to_tile, position = "before", nav = nav_panel(
        title = input$move_module$tab_title,
        value = input$move_module$tab_id
      ))
      
      # tabs are always placed at the start of the list (because "before" in the previous command)
      # this script moves the tab to its proper place
      if (input$move_module$to_index > 0){
        shinyjs::runjs(stringr::str_c("setTimeout(function(){ var loc = $('#", input$move_module$to_tile, " :nth(",input$move_module$to_index+1,")'); $('#", input$move_module$to_tile, "').children().first().insertAfter($(loc))},100);"))
      }
      
      # - detach the module from its parent tab-panel and store it in a variable
      # - append the module to the new parent tab-panel
      # - move the new tab-panel in the right position so the order of the content matches the tab headers
      # - remove the old tab-panel itself
      shinyjs::runjs(stringr::str_c("setTimeout(function(){
        var mod = $('#", input$move_module$from_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$from_index,").children().detach();
        $('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().last().append($(mod));
        $('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$to_index,").before($('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().last());
        $('#", input$move_module$from_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$from_index,").remove();
      }, 100);"))
      
      # if moving the selected tab away, then select the next tab in that panel
      if (is.null(input[[input$move_module$from_tile]])){
        nav_select(input$move_module$from_tile, input$move_module$sibling_tab_id)
      }
      
      # if moving a tab to an empty panel, select it automatically
      if (is.null(input[[input$move_module$to_tile]])){
        nav_select(input$move_module$to_tile, input$move_module$tab_id)
        # ensure that Shiny know this panel is visible so it will actually render outputs
        print("Trigger visibility")
        shinyjs::runjs(stringr::str_c("
          $('[data-value=\"", input$move_module$tab_id,"\"]').show();
          $('[data-value=\"", input$move_module$tab_id,"\"]').trigger('shown');
        "))
      }
      
    } else {
      # we switch only the order between tabs on a single tile
      # only thing needed is to update the order of the tabpanel divs as well so moving them outside the panel later will still work
      # because moving within a list updates the ordering, we need to place with .before when moving tabs to the left and with .after when moving to the right
      if (input$move_module$from_index > input$move_module$to_index){
        shinyjs::runjs(stringr::str_c("setTimeout(function(){
          $('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$to_index,").before($('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$from_index,"));
        }, 100);"))
      } else {
        shinyjs::runjs(stringr::str_c("setTimeout(function(){
          $('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$to_index,").after($('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$from_index,"));
        }, 100);"))
      }
    }
    
    # store in layout
    from_tile <- globals$elements[[input$move_module$from_tile]]
    to_tile <- globals$elements[[input$move_module$to_tile]]
    
    module_id <- from_tile$remove_module(input$move_module$from_index+1)
    to_tile$add_module(module_id, input$move_module$to_index)
    # no set_parent here because modules don't know in which tile they are
    
    # serialize!
  }) %>% bindEvent(input$move_module)
  
  # Select first page
  shinydashboard::updateTabItems(inputId = "page", selected = isolate(globals$pages[[1]]$get_id()))
  waiter::waiter_hide()
}

# Run the application 
shinyApp(ui = ui, server = server)
