Terminal <- R6::R6Class(
  "Terminal", 
  inherit = Module,
  
  private = list(
    
    
    imports = list(),
    exports = list(),
    params = list(),
    
    
    # Override this for module-specific UI
    ui = function(){
      ns <- NS(private$id)
      tagList(
        shiny::textAreaInput(ns("terminal_input"), "Terminal input"),
        actionButton(ns("execute"), "Run"),
        shiny::verbatimTextOutput(ns("terminal_output"))
      )
    },
    
    server = function(input, output, session, observe, module_inputs, module_outputs){
      ns <- session$ns
      
      # output_text = reactiveVal("")
      # 
      # observe({ 
      #   
      #   output_text(c(output_text(), system(input$terminal_input)))
      #   
      # }) %>% bindEvent(input$execute, ignoreInit = TRUE)
      
      output$terminal_output <- renderPrint({
        print(eval(parse(text = input$terminal_input)))
      })  %>% bindEvent(input$execute, ignoreInit = TRUE)
    }
  ),
  
  public = list(
    
    
    
  )
)
#defaults
Terminal$name = "Terminal"
Terminal$page = NA_character_
Terminal$group = NA_character_
Terminal$singleton = TRUE
Terminal$category <- "dev"