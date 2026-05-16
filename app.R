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
source("page.R")
source("module.R")
source("modules/mod_slider.R")
source("modules/mod_histogram.R")
source("modules/mod_text.R")

source("bslib/elemental_tile.R")
source("bslib/elemental_column.R")
source("bslib/elemental_row.R")
source("bslib/elemental_page_navbar.R")

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
  globals$pages <- purrr::imap(jsonlite::read_json("pages.json"), function(page, id){
    Page$new(id, page$title, page$icon, globals, reactiveValues(
      !!!purrr::imap(page$modules, function(module, id){
        list(class = get(module$class), title=module$title, imports = module$imports)
      })
    ), page$layout, session)
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

  #handling moving tiles
  observe({
    print(input$move_tile)
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
    
  }) %>% bindEvent(input$move_module)
  
  # Select first page
  shinydashboard::updateTabItems(inputId = "page", selected = "id_page1")
  waiter::waiter_hide()
}

# Run the application 
shinyApp(ui = ui, server = server)
