Text <- R6::R6Class(
  "Text", 
  inherit = Module,
  
  private = list(
    
    imports = list(),
    exports = list(),
    params = list(),

    # Override this for module-specific UI
    ui = function(){
      ns <- NS(private$id)
      div(
        h1(private$default_name),
        verbatimTextOutput(ns("text"))
      )
    },
    
    server = function(input, output, session, module_inputs, module_outputs){
      ns <- session$ns
      
      output$text <- renderPrint({
        print(.subset2(input, "impl")$.values)
      })
    }
  ),
  
  public = list(
    
    
    
  )
)

#defaults
Text$name = "Text"
Text$page = NA_character_
Text$group = NA_character_
Text$singleton = FALSE
Text$category <- "static"