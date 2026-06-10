Module <- R6::R6Class(
  "Module", 
  
  private = list(
    id = NA_character_,
    
    imports = list(),
    exports = list(),
    params = list(),
    
    introtour = list(),
    state = list(),
    
    title = NA_character_,
    globals = list(),
    module_inputs = list(),
    module_outputs = list(),

    active = FALSE,
    fullscreen = FALSE, # this will be a reactiveVal

    ui = function(){
      ns <- NS(private$id)
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
    initialize = function(id, title, globals, module_inputs, state){
      private$id <- id
      if (!is.null(title)){
        private$title <- title
      } else {
        private$title <- get_class(class(self)[1])$name
      }
      private$globals <- globals
      if (!is.null(module_inputs)){
        private$module_inputs <- module_inputs
        browser()
        # add empty import statements based on private/static info
      }
      if (!is.null(module_inputs)){
        private$state = state
        browser()
        # add empty params based on private/static info
      }
      
    },
    
    get_id = function(){
      return(private$id)
    },
    get_default_name = function(){
      return(private$default_name)
    },
    get_title = function(){
      return(private$title)
    },
    set_title = function(title){
      private$title <- title
    },
    is_active = function(){
      return(private$active)
    },
    get_inputs = function(){
      return(names(private$module_inputs))
    },
    get_input = function(input_var){
      return(private$module_inputs[[input_var]])
    },
    get_outputs = function(){
      return(names(private$module_outputs))
    },
    get_output = function(output_var){
      return(private$module_outputs[[output_var]])
    },
    get_introtour = function(){
      return(private$introtour)
    },

    serialize = function(){
      list(class = class(self)[1], title = private$title, imports = private$module_inputs, params = as.list(private$params))
    },
    
    get_state = function(){
      return(private$state)
    },
    get_params = function(){
      return(names(private$params))
    },
    get_param = function(param){
      return(private$params[[param]])
    },
    
    stateful = function(varname, defaultvalue){
      if (!varname %in% names(private$state)){
        private$state[[varname]] <- defaultvalue
        return(defaultvalue)
      } else {
        return(private$state[[varname]])
      }
    },
    
    is_fullscreen = function(){
      return(private$fullscreen())
    },
    set_fullscreen = function(value){
      private$fullscreen(value)
    },
    
    get_ui = function(){
      private$ui()
    },
    # these functions are defined inside the server function because they need reactivity
    # calling these before start_server() is called will result in an error
    set_input = NULL, 
    set_param = NULL,
    remove = NULL, 
    
    start_server = function(){
      if (!private$active){
        private$active <- TRUE
        private$fullscreen <- reactiveVal(FALSE)
        
        # Override the observe function so we can automatically keep them in a list so we can properly destroy them when the module is removed
        observers <- list()
        observe <- function(x, env = parent.frame(), ...){
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
          module_inputs_observers <- purrr::map(names(private$module_inputs), function(varname){
            observe({
              quote({
                input_path <- private$module_inputs[[varname]]
                print(stringr::str_c("[", session$ns(""), "] execute observer for: ", stringr::str_c(input_path, collapse = ", ")))
                # Old requirement, this should now always be true: "Module" %in% class(private$globals$modules[[input_path[1]]])
                
                # register this dependency with the module that exports so it knows which modules depend on its output
                # when a non-active module is activated it can trigger its dependent modules to re-register their inputs
                
                if (private$globals$modules[[input_path[1]]]$is_active()){
                  # this page has been loaded and its modules have been initialized so they can be referred to
                  module_inputs[[varname]] <- private$globals$modules[[input_path[1]]]$get_output(input_path[2])
                  print(stringr::str_c("New value: ", module_inputs[[varname]]))
                } else {
                  # this page has not been loaded so we cannot yet refer to its modules and their exported variables
                  module_inputs[[varname]] <- NULL
                  print("New value: NULL")
                  
                }
                
              })
            }, quoted = TRUE)            
          })
          
          private$params <- reactiveValues(!!!private$params)
          
          module_outputs <- reactiveValues()

          # This calls each unique module's server function
          private$server(input, output, session, module_inputs, module_outputs)
  
          # Create observers for changes to stateful inputs
          module_states_observers <- purrr::imap(private$state, function(value, varname){
            observe({
              quote({
                print(stringr::str_c("[", session$ns(""), "] execute state observer for ", varname, ": ", value))
                if (private$state[[varname]] != input[[varname]]){
                  private$state[[varname]] <- input[[varname]]
                  serialize(state = private$globals$modules)
                }
              })
            }, quoted = TRUE) %>% bindEvent(input[[varname]])
          })
          
          # # Update the database when the user has changed some aspect of this module, like the imports
          # module_outputs$notify_spec_change <- function(){
          #   
          # }
          # 
          
          self$set_input <- function(input_var, input_path){
            if (all(private$module_inputs[[input_var]] == input_path)){
              return(NULL)
            } else {
              private$module_inputs[[input_var]] <- input_path
              
              if ("Module" %in% class(private$globals$modules[[input_path[1]]])){
                # this page has been loaded and its modules have been initialized so they can be referred to
                module_inputs[[input_var]] <- private$globals$modules[[input_path[1]]]$get_output(input_path[2])
                print(stringr::str_c("New value: ", module_inputs[[input_var]]))
              } else {
                # this page has not been loaded so we cannot yet refer to its modules and their exported variables
                module_inputs[[input_var]] <- NULL
                print("New value: NULL")
              }
              serialize(modules = private$globals$modules)
            }
          }
          
          self$set_param <- function(param_name, param_value){
            if (private$params[[param_name]] == param_value){
              return(NULL)
            } else {
              
              private$params[[param_name]] <- param_value
              
            }
          }
          
          # Clean up and remove this module
          self$remove <- function(){
            purrr::walk(module_inputs_observers, ~.$destroy())
            module_inputs_observers <<- NULL
            module_inputs <<- NULL
            purrr::walk(observers, ~.$destroy())
            observers <<- NULL
            module_data <<- NULL

            # Use some internal trickery to remove input values
            #   If we do not do this, then creating a new instance of this module might reuse the old values for buttons
            #   and immediately trigger any related observers
            purrr::walk(ns(names(input)), .subset2(input, "impl")$.values$remove)
            module_outputs <<- NULL
          }
          
          return(module_outputs)
        })
      } else {
        stop("A module can only be started once.")
      }
    }
  )
)