Page <- R6::R6Class(
  "Page", 
  
  private = list(
    id = NA_character_,
    title = NA_character_,
    icon = NA_character_,
    
    globals = list(),
    modules = list(),
    
    active = FALSE
  ),
  
  public = list(
    initialize = function(id, title, icon, globals, modules){
      private$id <- id
      private$title <- title
      private$icon <- icon
      private$globals <- globals
      private$modules <- modules
    },
    
    get_id = function(){
      return(private$id)
    },
    get_title = function(){
      return(private$title)
    },
    get_active = function(){
      return(private$active)
    },
    get_module = function(id){
      return(private$modules[[id]])
    },
    
    get_ui = function(){
      ns <- NS(private$id)
      shinydashboard::tabItem(
        private$id, 
        uiOutput(ns("page"))
      )
    },
    
    get_menu_item = function(){
      shinydashboard::menuItem(private$title, tabName = private$id, icon = icon(private$icon))
    },
    
    start_server = function(){
      private$active <- TRUE
      moduleServer(private$id, function(input, output, session){
        ns <- session$ns
        
        print(stringr::str_c("start server function for ", private$id))
        
        # Initialize module objects
        purrr::walk(names(private$modules), function(id){
          private$modules[[id]] <- private$modules[[id]]$class$new(id, ns(id), private$globals, private$modules[[id]]$imports)
        })
        # Start server functions of modules
        purrr::walk(names(private$modules), ~private$modules[[.]]$start_server())
        

        output$page <- renderUI({
          div(
            h1(private$title),
            p("Test page"),
            !!!purrr::map(names(private$modules), ~private$modules[[.]]$get_ui())
          )
        })
        
        
        
        return(NULL)
      })
    }
  )
)