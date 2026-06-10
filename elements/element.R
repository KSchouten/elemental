Element <- R6::R6Class(
  "Element", 
  
  private = list(
    id = NA_character_,
    title = NA_character_,
    globals = list(),
    active = FALSE,
    introtour = list(),
    
    ui = function(){
      ns <- NS(private$id)
      tagList(
        uiOutput(ns("element"))
      )
    },
    
    server = function(input, output, session){
      output$element <- renderUI({
        div(
          h1(private$title),
          p("Dit element gebruikt nog de standaard server ui/server functie. Schrijf je eigen ui() en server() functie in de subclass van Element.")
        )
      })
    }
    
    
  ),
  
  public = list(
    initialize = function(id, title, globals){
      private$id <- id
      if (!is.null(title)){
        private$title <- title
      }
      private$globals <- globals
    },
    
    get_id = function(){
      return(private$id)
    },
    get_title = function(){
      return(private$title)
    },
    is_active = function(){
      return(private$active)
    },
    get_introtour = function(){
      return(private$introtour)
    },
    
    get_ui = function(){
      private$ui()
    },

    remove = NULL, 
    
    start_server = function(){
      if (!private$active){
        private$active <- TRUE
        
        # Override the observe function so we can automatically keep them in a list so we can properly destroy them when the module is removed
        observers <- list()
        observe <- function(x, env = parent.frame(), ...){
          print("custom observe")
          force(env)
          obs <- shiny::observe(x, env, ...)
          observers <- append(observers, obs)
          return(obs)
        }
        
        moduleServer(private$id, function(input, output, session){
          ns <- session$ns
          
          print(stringr::str_c("start server function for ", private$id))
          
          # This calls each unique module's server function
          private$server(input, output, session)
          
          # # Update the database when the user has changed some aspect of this module, like the imports
          # module_outputs$notify_spec_change <- function(){
          #   
          # }
          # 
          
          # Clean up and remove this module
          self$remove <- function(){
            purrr::walk(observers, ~.$destroy())
            observers <<- NULL

            # Use some internal trickery to remove input values
            #   If we do not do this, then creating a new instance of this module might reuse the old values for buttons
            #   and immediately trigger any related observers
            purrr::walk(ns(names(input)), .subset2(input, "impl")$.values$remove)
          }
          
          return(NULL)
        })
      } else {
        stop("An element can only be started once.")
      }
    }
  )
)