#' Base class for all modules supported by the framework
#' @export
Module <- R6::R6Class(
  "Module", 
  
  private = list(
    id = NA_character_,
    
    imports = list(),
    exports = list(),
    params = list(),
    
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
    },
    
    # placeholders for reactive functions
    reactive_set_input = NULL,
    reactive_set_param = NULL,
    reactive_remove = NULL
    
  ),
  
  public = list(
    #' @description
    #' Default constructor for Module
    #' 
    #' Do not call directly, but subclass Module instead
    #' This constructor will be called automatically when no constructor is present in child class
    #' If necessary, create a constructor in child class, then don't forget to call super$initialize()
    #'
    #' @param id An id for this module, can be generated with generate_id()
    #' @param title A optional title for this module, uses classname if not provided
    #' @param globals A reference to a set of global, reactive, variables. Don't use these directly in your own modules
    #' @param module_inputs A names list of character vectors that map output variables of other modules to input variables for this module
    #' @param state A list of variables that describe the current state of the module. Don't put datasets in here, just simple variables.
    #'
    #' @returns A Module object
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
      } else {
        # add empty import statements based on private/static info
        private$module_inputs <- purrr::map(private$imports, ~c()) %>% setNames(private$imports)
      }
      if (!is.null(module_inputs)){
        private$state = state
        # add empty params based on private/static info?
      }
      
    },
    
    #' @description
    #' Get id
    #'
    #' @returns The id of this module
    get_id = function(){
      return(private$id)
    },
    #' @description
    #' Get title
    #'
    #' @returns The title of this module
    get_title = function(){
      return(private$title)
    },
    #' @description
    #' Set title
    #' 
    #' @param title The new title for this module
    set_title = function(title){
      private$title <- title
    },
    #' @description
    #' Is this module active
    #'
    #' @returns TRUE or FALSE to denote whether the server function of this module has been started yet
    is_active = function(){
      return(private$active)
    },
    #' @description
    #' Get names of inputs
    #'
    #' @returns A character vector of names of the necessary input variables
    get_inputs = function(){
      return(names(private$module_inputs))
    },
    #' @description
    #' Get an input value
    #'
    #' @param input_var The name of the input variable
    #'
    #' @returns The current value of this input variable
    get_input = function(input_var){
      return(private$module_inputs[[input_var]])
    },
    #' @description
    #' Get names of outputs
    #'
    #' @returns A character vector of names of the exported output variables
    get_outputs = function(){
      return(names(private$module_outputs))
    },
    #' @description
    #' Get an output value
    #'
    #' @param output_var The name of the output variable
    #'
    #' @returns The current value of this output variable
    get_output = function(output_var){
      return(private$module_outputs[[output_var]])
    },
    #' @description
    #' Get the content of the introtour
    #'
    #' This function is called when clicking the introtour button
    #' Subclasses should generally override this function to provide a specific introtour for that module,
    #'   otherwise this generic message will appear saying that there is no introtour yet,
    #' @returns A list of steps, as required by the rintrojs package
    get_intro_tour = function(){
      return(list(list(title = "Helaas!", intro = "Voor deze module is (nog) geen intro tour beschikbaar.")))
    },
    
    #' @description
    #' Serialize this module
    #' 
    #' When making changes to the modules section of the config, all modules provide a list of values that are enough to recreate the module next time
    #' This does not contain state, so the current selection or current value of input fields are not serialized here, but are stored in state instead.
    #'
    #' @returns A list of values that can be transformed to JSON
    serialize = function(){
      list(class = class(self)[1], title = private$title, imports = isolate(reactiveValuesToList(private$module_inputs)), params = as.list(private$params))
    },
    #' @description
    #' Get the state of this module
    #'
    #' This is not the configuration of the module, just the current state, like selection, slider value etc.
    #' 
    #' Be aware that state and config need to align to prevent errors.
    #'
    #' @returns A named list of values that together describe the current state
    get_state = function(){
      return(private$state)
    },
    #' @description
    #' Get names of static parameters
    #'
    #' Static parameters provide the user a way to setup a module differently. They can only be used inside a module, not between modules.
    #' Parameters are part of the modules' config, not its state.
    #'
    #' @returns A character vector of names of static parameters
    get_params = function(){
      return(names(private$params))
    },
    #' @description
    #' Get a parameter value
    #'
    #' @param param Name of the parameter
    #'
    #' @returns The current value of the static parameter
    get_param = function(param){
      return(private$params[[param]])
    },
    #' @description
    #' Convenience function to make a value stateful in the UI definition of a module
    #'
    #' Wrap a value, for instance a slider's value or a textbox's value inside stateful and provide a default value.
    #' This function will retrieve a value of it exists or register the default value as a state value otherwise.
    #' It will automatically serialize in that case.
    #'
    #' @param varname Name of the state variable
    #' @param defaultvalue The default value in case it does not exist yet
    #'
    #' @returns The stored state value or the default value in case it doesn't exist
    stateful = function(varname, defaultvalue){
      if (!varname %in% names(private$state)){
        private$state[[varname]] <- defaultvalue
        return(defaultvalue)
      } else {
        return(private$state[[varname]])
      }
    },
    #' @description
    #' Is this module currently in fullscreen mode?
    #'
    #' @returns A reactive value that you can observe() that is TRUE or FALSE
    is_fullscreen = function(){
      return(private$fullscreen())
    },
    #' @description
    #' Change the fullscreen value
    #'
    #' When a user clicks the fullscreen button or closes the fullscreen mode, 
    #' this function is automatically called from the Tile object to set this 
    #' reactive value that can be observed in your subclass.
    #'
    #' @param value TRUE or FALSE
    set_fullscreen = function(value){
      private$fullscreen(value)
    },
    #' @description
    #' Public wrapper function to retrieve the private ui
    #' 
    #' This ensures each Module has the same interface. 
    #' Override the standard private$ui() function in your subclass
    #' Otherwise a default message will appear as the UI.
    #'
    #' @returns The UI for this module, usually calls the ui from the child module
    get_ui = function(){
      private$ui()
    },
    
    # these functions are defined inside the server function because they need reactivity
    # calling these before start_server() is called will result in an error
    
    #' @description
    #' Reactively change the definition of an input variable
    #'
    #' @param input_var The name of the input variable
    #' @param input_path  The new path where this module will look for a value to associate with this input variable. 
    #' Always consists of a 2-long character vector with a module id and output variable name.
    #'
    #' This method is defined within the server function so it operates in a reactive context.
    set_input = function(input_var, input_path){private$reactive_set_input(input_var, input_path)}, 
    #' @description
    #' Reactively change the value of a parameter
    #'
    #' @param param_name The name of the parameter
    #' @param param_value  The new value of this parameter
    #'
    #' This method is defined within the server function so it operates in a reactive context.
    set_param = function(param_name, param_value){private$reactive_set_param(param_name, param_value)},
    
    #' @description
    #' Remove this module
    #'
    #' Tries to remove all content, observers etc. to prevent memory leakage.
    remove = function(){private$reactive_remove()}, 
    
    #' @description
    #' Start the server function of this module
    #'
    #' @returns A reactiveValues object with the output variables, even though that is not used anymore to access the output variables
    start_server = function(){
      if (!private$active){
        private$active <- TRUE
        private$fullscreen <- reactiveVal(FALSE)
        
        private$module_inputs <- reactiveValues(!!!private$module_inputs)
        
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
                
                if (length(input_path)==2 && input_path[1] %in% names(private$globals$modules) && private$globals$modules[[input_path[1]]]$is_active()){
                  # this page has been loaded and its modules have been initialized so they can be referred to
                  module_inputs[[varname]] <- private$globals$modules[[input_path[1]]]$get_output(input_path[2])
                  print(stringr::str_c("New value: ", module_inputs[[varname]]))
                } else {
                  # this module is newly added so no dependencies are defined yet
                  module_inputs[[varname]] <- NULL
                  print("New value: NULL")
                  
                }
                
              })
            }, quoted = TRUE)            
          }) %>% setNames(names(private$module_inputs))
          
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
          
          private$reactive_set_input <- function(input_var, input_path){
            
            if (!is.null(private$module_inputs[[input_var]]) && all(private$module_inputs[[input_var]] == input_path)){
              return(NULL)
            } else {
              private$module_inputs[[input_var]] <- input_path
              
              serialize(modules = private$globals$modules)
            }
          }
          
          private$reactive_set_param <- function(param_name, param_value){
            if (private$params[[param_name]] == param_value){
              return(NULL)
            } else {
              
              private$params[[param_name]] <- param_value
              
            }
          }
          
          # Clean up and remove this module
          private$reactive_remove <- function(){
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