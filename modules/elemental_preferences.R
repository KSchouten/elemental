ElementalPreferences <- R6::R6Class(
  "ElementalPreferences", 
  inherit = Module,
  
  private = list(
    
    default_name = "Voorkeuren",
    default_page = NA_character_,
    imports = list(),
    params = list(),
    group = NA_character_,
    singleton = TRUE,
    
    module = NULL,
    exports = list(),
    
    # Override this for module-specific UI
    ui = function(){
      
      ns <- NS(private$id)
      div(
        h1(private$default_name),

        selectInput(ns("theme"), "Thema", c("shiny", bslib::bootswatch_themes()), private$globals$preferences$theme), 
        selectInput(ns("tile_menu"), "Tegel acties", c("Samen in menu" = TRUE, "Los in titelbalk" = FALSE), selected = private$globals$preferences$tile_menu),
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
        req(private$globals$preferences$theme != input$theme)
        private$globals$preferences$theme <- input$theme
        serialize(preferences = private$globals$preferences)
        session$setCurrentTheme(create_theme(input$theme))
      }) %>% bindEvent(input$theme, ignoreInit = TRUE)

      observe({
        req(private$globals$preferences$tile_menu != as.logical(input$tile_menu))
        private$globals$preferences$tile_menu <- as.logical(input$tile_menu)
        serialize(preferences = private$globals$preferences)
        
        private$globals$elements %>% purrr::keep(~"ElementalTile" %in% class(.)) %>% purrr::map(~.$use_menu(private$globals$preferences$tile_menu))
        
        #browser()
      })
    }
  ),
  
  public = list(
    
    
    
  )
)