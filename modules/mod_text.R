Text <- R6::R6Class(
  "Text", 
  inherit = Module,
  
  private = list(
    
    default_name = "text",
    default_page = NA_character_,
    imports = list(),
    exports = list(),
    params = list(),
    group = NA_character_,
    singleton = FALSE,
    
    # Override this for module-specific UI
    ui = function(){
      ns <- NS(private$full_id)
      div(
        h1(private$default_name),
        p("Here is some text...")
      )
    },
    
    server = function(input, output, session, module_inputs, module_outputs){
      ns <- session$ns
      
    }
  ),
  
  public = list(
    
    
    
  )
)