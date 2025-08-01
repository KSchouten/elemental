Module <- R6::R6Class(
  "Module", 
  
  private = list(
    id = NA_character_,
    full_id = NA_character_,
    
    default_name = NA_character_,
    default_page = NA_character_,
    imports = list(),
    exports = list(),
    params = list(),
    group = NA_character_,
    singleton = FALSE,
    
    globals = list(),
    module_inputs = list(),
    module_outputs = list(),
    
    active = FALSE,

    ui = function(){
      ns <- NS(private$full_id)
      tagList(
        uiOutput(ns("module"))
      )
    },
    
    server = function(input, output, session, module_inputs, module_outputs){
      output$page <- renderUI({
        div(
          h1("Test module")
        )
      })
    }
  ),
  
  public = list(
    initialize = function(id, full_id, globals, module_inputs){
      private$id <- id
      private$full_id <- full_id
      private$globals <- globals
      private$module_inputs <- module_inputs
    },
    
    get_id = function(){
      return(private$id)
    },
    get_full_id = function(){
      return(private$full_id)
    },
    get_active = function(){
      return(private$active)
    },
    get_output = function(output_var){
      return(private$module_outputs[[output_var]])
    },
    
    get_ui = function(){
      private$ui()
    },
    
    start_server = function(){
      private$active <- TRUE
      
      # Override the observe function so we can automatically keep them in a list so we can properly destroy them when the module is removed
      observers <- list()
      observe = function(x, env = parent.frame(), ...){
        print("custom observe")
        force(env)
        obs <- shiny::observe(x, env, ...)
        observers <- append(observers, obs)
        return(obs)
      }
      
      private$module_outputs <- moduleServer(private$id, function(input, output, session){
        ns <- session$ns
        
        print(stringr::str_c("start server function for ", private$id))

        module_inputs <- reactiveValues()
        module_inputs_observers <- purrr::imap(private$module_inputs, function(input_path, varname){
          observe({
            quote({
              print(stringr::str_c("[", session$ns(""), "] execute observer for: ", stringr::str_c(input_path, collapse = ", ")))
              
              if ("Module" %in% class(private$globals$pages[[input_path[1]]]$get_module(input_path[2]))){
                # this page has been load and its modules have been initialized so they can be referred to
                module_inputs[[varname]] <- private$globals$pages[[input_path[1]]]$get_module(input_path[2])$get_output(input_path[3])
                print(stringr::str_c("New value: ", module_inputs[[varname]]))
              } else {
                # this page has not been loaded so we cannot yet refer to its modules and their exported variables
                module_inputs[[varname]] <- NULL
                print("New value: NULL")
              }
              
            })
          }, quoted = TRUE)            
        })
        
        module_outputs <- reactiveValues()
        
        # This calls each unique module's server function
        private$server(input, output, session, module_inputs, module_outputs)

        
        # Update the database when the user has changed some aspect of this module, like the imports
        module_outputs$notify_spec_change <- function(){
          
        }
        
        # Clean up and remove this module
        module_outputs$remove <- function(){
          purrr::walk(module_inputs_observers, ~.$destroy())
          module_inputs_observers <<- NULL
          module_inputs <<- NULL
          purrr::walk(observers, ~.$destroy())
          observers <<- NULL
          module_data <<- NULL
          
          # Use some internal trickery to remove input values
          #   If we do not do this, then creating a new sort on the same column will reuse the old values for buttons
          #   and immediately trigger observers (incl. the remove one)
          purrr::walk(ns(names(input)), .subset2(input, "impl")$.values$remove)
          module_outputs <<- NULL
        }
        
        return(module_outputs)
      })
    }
  )
)