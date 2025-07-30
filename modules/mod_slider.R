Slider <- R6::R6Class(
  "Slider", 
  inherit = Module,
  
  private = list(
    
    default_name = "slider",
    default_page = NA_character_,
    imports = list(),
    exports = list("slider_value"),
    params = list(),
    group = NA_character_,
    singleton = FALSE,
    
    # Override this for module-specific UI
    ui = function(){
      ns <- NS(private$full_id)
      div(
        h1(private$default_name),
        shiny::sliderInput(ns("slider"), label = "Choose value", min = 0, max = 50, value = 25)
      )
    },
    
    server = function(input, output, session, module_inputs, module_outputs){
      ns <- session$ns
      
      # Add a variable as an export
      observe({
        module_outputs$slider_value <- input$slider
      }) %>% bindEvent(input$slider)
    }
  ),
  
  public = list(
    
    
    
  )
)