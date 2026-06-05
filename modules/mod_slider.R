Slider <- R6::R6Class(
  "Slider", 
  inherit = Module,
  
  private = list(
    
    default_name = "slider",
    default_page = NA_character_,
    imports = list(),
    exports = list("slider_value"),
    params = list(min = 5, max = 50),
    group = NA_character_,
    singleton = FALSE,

    # Override this for module-specific UI
    ui = function(){
      ns <- NS(private$id)
      div(
        h1(private$default_name),
        shiny::sliderInput(ns("slider"), label = "Choose value", min = private$params$min, max = private$params$max, value = self$stateful("slider", 25))
      )
    },
    
    server = function(input, output, session, module_inputs, module_outputs){
      ns <- session$ns

      # Add a variable as an export
      # Observer to update output value when UI is present and changed
      observe({
        print("slider change")
        module_outputs$slider_value <- input$slider
      }) %>% bindEvent(input$slider)
    }
  ),
  
  public = list(
    
    
    
  )
)