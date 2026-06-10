ElementalEditTitle <- R6::R6Class(
  "ElementalEditTitle", 
  inherit = Module,
  
  private = list(
    
    default_name = "Verander titel",
    default_page = NA_character_,
    imports = list(),
    params = list(),
    group = NA_character_,
    singleton = TRUE,
    
    ui_element = NULL,
    exports = list(),
    
    # Override this for module-specific UI
    ui = function(){
      
      ns <- NS(private$id)
      div(
        
        p(private$ui_element$get_id()),
        
        textInput(ns("title"), "Titel", private$ui_element$get_title()),
        
        actionButton(ns("done"), "Gereed")
      )
    },
    
    server = function(input, output, session, module_inputs, module_outputs){
      ns <- session$ns
      
      observe({
        self$remove()
        removeModal()
      }) %>% bindEvent(input$done)
      
      observe({
        req(input$title != private$ui_element$get_title())
        print(input$title)
        # update title
        private$ui_element$set_title(input$title)
        
        #shinyjs::runjs(stringr::str_c("$('a[data-value=", private$module$get_id(),"]').text('", input$title, "')"))
        
        serialize(pages = private$globals$pages)
      }) %>% bindEvent(input[["title"]], ignoreInit = TRUE)
      
      
    }
  ),
  
  public = list(
    
    initialize = function(id, title, globals, module_inputs, state, ui_element){
      super$initialize(id, title, globals, module_inputs, state)
      private$ui_element <- ui_element
      
    }
    
  )
)