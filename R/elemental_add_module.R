ElementalAddModule <- R6::R6Class(
  "ElementalAddModule", 
  inherit = Element,
  
  private = list(
    
    tile = NULL,
    
    # Override this for module-specific UI
    ui = function(){
      
      ns <- NS(private$id)
      div(
        h1(private$globals$t("Add module")),
        selectInput(ns("module"), "Module", purrr::map_chr(private$globals$all_modules, "classname") %>% setNames(purrr::map_chr(private$globals$all_modules, "name"))),
        
        p(private$globals$t("Module will be added to tile"), " ", strong(private$tile$get_title()), "."),
        p(private$globals$t("Parameters and dependencies on other modules can be adjusted with"), " ", em(private$globals$t("Module settings")), " ", private$globals$t("in the tile menu.")),
        div(actionButton(ns("ok"), private$globals$t("Ok")),
            actionButton(ns("cancel"), private$globals$t("Cancel")),
            style = "float: right")
        
      )
    },
    
    server = function(input, output, session){
      ns <- session$ns
      
      # cancel, just remove the modal
      observe({
        self$remove()
        removeModal()
      }) %>% bindEvent(input$cancel)
      
      # ok, add the module and remove the modal
      observe({
        print(stringr::str_c("Shiny.setInputValue('add_module', {'module': '", input$module, "', 'tile': '",private$tile$get_id(),"'})"))
        shinyjs::runjs(stringr::str_c("Shiny.setInputValue('add_module', {'module': '", input$module, "', 'tile': '",private$tile$get_id(),"'})"))
        #private$tile$add_module(input$module)

        self$remove()
        removeModal()
      }) %>% bindEvent(input$ok)
      
    }
  ),
  
  public = list(
    initialize = function(id, title, globals, tile){
      super$initialize(id, title, globals)
      private$tile <- tile
      
    }
    
    
  )
)

