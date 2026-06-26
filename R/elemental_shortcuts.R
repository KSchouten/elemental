ElementalShortcuts <- R6::R6Class(
  "ElementalShortcuts", 
  inherit = Element,
  
  private = list(

    # Override this for module-specific UI
    ui = function(){
      
      ns <- NS(private$id)
      div(id = ns("body"),
        h1(private$globals$i18n$t("Keyboard shortcuts")),
        
        p(tags$kbd("?"), private$globals$i18n$t("Show this overview of keyboard shortcuts")),
        p(tags$kbd("V"), private$globals$i18n$t("Open the 'Preferences' dialog")),
        
        actionButton(ns("done"), private$globals$i18n$t("Done")),
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