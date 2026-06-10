ElementalAddModule <- R6::R6Class(
  "ElementalAddModule", 
  inherit = Element,
  
  private = list(
    
    title = "Module toevoegen",
    tile = NULL,
    
    # Override this for module-specific UI
    ui = function(){
      
      ns <- NS(private$id)
      div(
        h1(private$title),
        selectInput(ns("module"), "Module", purrr::map_chr(private$globals$all_modules, "classname") %>% setNames(purrr::map_chr(private$globals$all_modules, "name"))),
        
        p("Module wordt toegevoegd aan tegel ", strong(private$tile$get_title()), "."),
        p("Parameters en afhankelijkheden van andere modules kunnen via ", em("Module instellingen"), " in het tegelmenu worden aangepast."),
        div(actionButton(ns("ok"), "Akkoord"),
            actionButton(ns("cancel"), "Annuleren"),
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
  
        private$tile$add_module(input$module)

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

