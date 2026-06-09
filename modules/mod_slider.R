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
        uiOutput(ns("fullscreenmessage")),
        shiny::sliderInput(ns("slider"), label = "Choose value", min = private$params$min, max = private$params$max, value = self$stateful("slider", 25), width = "100%")
      )
    },
    
    server = function(input, output, session, module_inputs, module_outputs){
      ns <- session$ns

      output$fullscreenmessage <- renderUI({
        if (self$is_fullscreen()){
          p("Je bekijkt deze module nu in full screen mode!")
        }
      })
      
      # Add a variable as an export
      # Observer to update output value when UI is present and changed
      observe({
        print("slider change")
        module_outputs$slider_value <- input$slider
      }) %>% bindEvent(input$slider)
      
      observe({
        print(stringr::str_c("Param {min: ",private$params$min, "}"))
        try({
          new_min <- as.numeric(private$params$min)
          private$params$min <- new_min
        })
        if (is.numeric(private$params$min)){
          updateSliderInput(session, inputId = "slider", min = private$params$min)
          serialize(modules = private$globals$modules)
        }
      }) %>% bindEvent(private$params$min, ignoreInit = TRUE)
      observe({
        print(stringr::str_c("Param {max: ",private$params$max, "}"))
        try({
          new_max <- as.numeric(private$params$max)
          private$params$max <- new_max
        })
        if (is.numeric(private$params$max)){
          updateSliderInput(session, inputId = "slider", max = private$params$max)
          serialize(modules = private$globals$modules)
        }
      }) %>% bindEvent(private$params$max, ignoreInit = TRUE)
    }
  ),
  
  public = list(
    
    
    
  )
)