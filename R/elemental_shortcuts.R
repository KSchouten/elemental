ElementalShortcuts <- R6::R6Class(
  "ElementalShortcuts", 
  inherit = Element,
  
  private = list(
    
    title = "Sneltoetsen",
    
    # Override this for module-specific UI
    ui = function(){
      
      ns <- NS(private$id)
      div(id = ns("body"),
        h1(private$title),
        
        p(tags$kbd("?"), "Toon sneltoetsen"),
        
        actionButton(ns("done"), "Klaar"),
        uiOutput(ns("events"))
      )
    },
    
    server = function(input, output, session){
      ns <- session$ns
      
      key_event <- reactiveVal()
      
      output$events <- renderUI({

        
        
        key_event(
          shinyjs::onevent("keydown", "body", function(key){
            print(key)
            if(key$key == "Escape"){
              self$remove()
              removeModal()
            }
          })
        )
                
        NULL
      })
      
      observe({
        isolate(shinyjs::removeEvent("keydown", key_event()))
        self$remove()
        removeModal()
      }) %>% bindEvent(input$done)
      
    }
  ),
  
  public = list(
    
    
    
  )
)