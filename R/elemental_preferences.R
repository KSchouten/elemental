ElementalPreferences <- R6::R6Class(
  "ElementalPreferences", 
  inherit = Element,
  
  private = list(
    
    # Override this for module-specific UI
    ui = function(){
      
      ns <- NS(private$id)
      div(
        h1(private$globals$i18n$ui_t("Preferences")),

        selectInput(ns("theme"), private$globals$i18n$ui_t("Theme"), c("shiny", bslib::bootswatch_themes()), private$globals$preferences$theme), 
        selectInput(ns("language"), private$globals$i18n$ui_t("Language"), choices = private$globals$i18n$get_all_languages(), selected = private$globals$i18n$get_current_language()),
        uiOutput(ns("tile_menu_ui")),
        actionButton(ns("done"), private$globals$i18n$ui_t("Done"))
      )
    },
    
    server = function(input, output, session){
      ns <- session$ns
      
      # this will reactively update when the language changes
      output$tile_menu_ui <- renderUI({
        selectInput(ns("tile_menu"), private$globals$i18n$reactive_t("Tile actions"), choices = c(TRUE, FALSE) %>% setNames(c(private$globals$i18n$reactive_t("Folded in tile menu"), private$globals$i18n$reactive_t("Separate buttons in tile header"))), selected = private$globals$preferences$tile_menu)
      }) 
      
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
        req(private$globals$preferences$language != input$language)
        private$globals$i18n$set_language(input$language)
        private$globals$preferences$language <- input$language
        serialize(preferences = private$globals$preferences)
      }) %>% bindEvent(input$language, ignoreInit = TRUE)
      
      observe({
        req(private$globals$preferences$tile_menu != as.logical(input$tile_menu))
        private$globals$preferences$tile_menu <- as.logical(input$tile_menu)
        serialize(preferences = private$globals$preferences)
        
        private$globals$elements %>% purrr::keep(~"ElementalTile" %in% class(.)) %>% purrr::map(~.$use_menu(private$globals$preferences$tile_menu))

      })
    }
  ),
  
  public = list(
    
    
    
  )
)