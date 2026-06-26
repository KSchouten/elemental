ElementalPreferences <- R6::R6Class(
  "ElementalPreferences", 
  inherit = Element,
  
  private = list(
    
    title = "Voorkeuren",

    # Override this for module-specific UI
    ui = function(){
      
      ns <- NS(private$id)
      div(
        h1(private$title),

        selectInput(ns("theme"), private$globals$t("Theme"), c("shiny", bslib::bootswatch_themes()), private$globals$preferences$theme), 
        selectInput(ns("language"), private$globals$t("Language"), choices = private$globals$i18n$get_languages(), selected = private$globals$i18n$get_translation_language()),
        uiOutput(ns("tile_menu_ui")),
        actionButton(ns("done"), "Gereed")
      )
    },
    
    server = function(input, output, session){
      ns <- session$ns
      
      output$tile_menu_ui <- renderUI({
        selectInput(ns("tile_menu"), private$globals$t("Tile actions"), choices = c(TRUE, FALSE) %>% setNames(c(private$globals$text["Folded in tile menu"], private$globals$text["Separate buttons in tile header"])), selected = private$globals$preferences$tile_menu)
      }) %>% bindEvent(private$globals$text)
      
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
        # this will update all the dynamic translations (inside span tags)
        shiny.i18n::update_lang(input$language)
        # this will update the translator object so any newly created translations are correct
        private$globals$i18n$set_translation_language(input$language)
        # this updates the set of reactive values with all texts for cases where a span tag cannot be used (such as title attributes on buttons)
        private$globals$text <- fromJSON(app_sys("app/translation.json"))$translation %>% purrr::map(function(x){list(x[[input$language]]) %>% setNames(x[[1]])}) %>% unlist(recursive = FALSE)
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