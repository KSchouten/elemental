#' The main App class that encapsulates the Shiny app
#'
#' @import shiny
#' @import bslib
#' @importFrom magrittr %>%
#' 
#' @export
App <- R6::R6Class(
  "App", 
  
  private = list(
    
    globals = reactiveValues(),
    theme = NULL,
    config = list(),
    i18n = NULL,
    
    # Define UI for application
    ui = function(){
      addResourcePath("static", app_sys("app/www"))
      
      tagList(
      
        # Use shinyjs
        shinyjs::useShinyjs(),
        # Use introjs
        rintrojs::introjsUI(),
        # Use shinyFeedback
        shinyFeedback::useShinyFeedback(),
        # Use i18n
        shiny.i18n::usei18n(private$i18n),
        
        # Show loading screen when app is loading
        waiter::use_waiter(),
        waiter::waiter_show_on_load(html = waiter::spin_3k(), color = waiter::transparent(alpha = 0)),
        
        elemental_page_navbar(
          title = "Elemental Experiment",
          id = "page",
          lang = "en",
          fillable = FALSE,
          nav_item(actionLink(inputId = stringr::str_c("new_page"), label = em(private$i18n$t("New"),"..."), icon = icon("plus"), onclick = htmlwidgets::JS("this.blur()")), class = "first_button button"),
          nav_spacer(),
          nav_menu(
            title = private$i18n$t("Settings"),
            icon = icon("cog"),
            align = "right",
            nav_item(actionLink(inputId = "change_page_title", label = span(private$i18n$t("Change page title")), icon = icon("pen-to-square"))),
            nav_item(actionLink(inputId = "print_page", label = span(private$i18n$t("Print page")), icon = icon("print"))),
            nav_item(actionLink(inputId = "preferences", label = span(private$i18n$t("Preferences"), HTML(" <kbd>V</kbd>")), icon = icon("sliders"))),
            nav_item(actionLink(inputId = "shortcuts", label = span(private$i18n$t("Keyboard shortcuts"),HTML(" <kbd>?</kbd>")), icon =icon("keyboard"))),
            nav_item(tags$a(shiny::icon("github"), span("Elemental @ GitHub"), href = "https://github.com/KSchouten/elemental", target = "_blank")),
            
          )
        ),
        
        tags$head(
          # Custom CSS
          tags$link(rel = "stylesheet", type = "text/css", href = "static/custom.css"),
          # Custom JS
          tags$head(tags$script(src = "static/split.js")),
          tags$head(tags$script(src = "static/script.js"))
        )
        
      )
    },
    
    
    # Define server logic
    server = function(input, output, session) {
      
      session$setCurrentTheme(private$theme)
      
      # Global variables
      #  database connection
      #  currently logged in user
      #globals <- reactiveValues()


      # Open database connection
      
      # Check url for params
      
            
      # Autologin user (locally or with url) and load page setup
      # Currently loads default page setup from pages.json
      private$globals$user <- "test"
      
      
      private$globals$preferences <- private$config$preferences
      
      private$globals$modules <- purrr::imap(private$config$modules, function(module, id){
        get_class(module$class)$new(id, module$title, private$globals, purrr::map(module$imports, unlist), module$params, private$config$state[[id]])
      })  
      
      private$globals$elements <- list()
      private$globals$pages <- purrr::map(private$config$pages, function(layout){
        ElementalPage$new(layout, private$globals)
      }) %>% setNames(purrr::map_chr(., ~.$get_id()))
      
      # Preload allowed modules to save time on a lookup per module
      
      # Add pages to main UI
      observe({
        purrr::walk(rev(private$globals$pages), function(page){
          # load UI of each page and its child components
          nav_insert("page", page$get_ui(), NULL, "before")
          
        })
      }) %>% bindEvent(private$globals$pages, once = TRUE)
      
      # Complete UI of each element and start server functions of modules
      observe({
        purrr::walk(rev(private$globals$pages), function(page){
          # start server function of each page and its child components
          page$complete_ui_reactive(input, output, session)
        })
      }) %>% bindEvent(input$page, once = TRUE)
      
      
      # Obs. change in selected page
      observe({
        req(input$page)
        print(input$page)
        
        private$globals$current_page <- input$page
        
        # If not started yet, start the server function for the selected page
        # Should be one-time only and only for newly-created pages
        if (!private$globals$pages[[input$page]]$is_active()){
          
          private$globals$pages[[input$page]]$complete_ui_reactive(input, output, session)
          
        }
        
        
      }) %>% bindEvent(input$page)
      
      # Handling new page ----
      observe({
        req(input$new_page)
        
        last_page_id <- private$globals$pages[[length(private$globals$pages)]]$get_id()
        new_page <- ElementalPage$new(
          list(class = "ElementalPage", title = private$globals$i18n$t("New page"), icon = "file-circle-plus", rows = list(
            list(class = "ElementalRow", column_sizes = c(40,60), columns = list(
              list(class = "ElementalColumn", tiles = list(
                list(class = "ElementalTile", title = "Tegel")
              )),
              list(class = "ElementalColumn", tiles = list())
            ))
          )), private$globals)
        private$globals$pages <- append(private$globals$pages, list(new_page) %>% setNames(new_page$get_id()))
        
        nav_insert("page", new_page$get_ui(), target = last_page_id, position = "after")
        
        
        # serialize!
        serialize(pages = private$globals$pages)
        
        nav_select("page", selected = new_page$get_id())
        
      }) %>% bindEvent(input$new_page)
      
      # Handling moving pages -----
      observe({
        req(input$move_page)
        print(input$move_page)
        
        idx <- input$move_page$from_index+1
        page <- private$globals$pages[idx]
        private$globals$pages <- append(private$globals$pages[-idx], page, input$move_page$to_index)
        
        # serialize!
        serialize(pages = private$globals$pages)
      }) %>% bindEvent(input$move_page)
      
      # Handling moving tiles ------
      observe({
        req(input$move_tile)
        print(input$move_tile)
        
        from_column <- private$globals$elements[[input$move_tile$from_column]]
        to_column <- private$globals$elements[[input$move_tile$to_column]]
        
        tile <- from_column$remove_tile(input$move_tile$from_index+1)
        to_column$add_tile(tile, input$move_tile$to_index)
        tile$set_parent(to_column)
        
        # serialize!
        serialize(pages = private$globals$pages)
      }) %>% bindEvent(input$move_tile)
      
      # Handling moving modules ------
      observe({
        req(input$move_module)
        print(input$move_module)
        
        if (input$move_module$from_tile != input$move_module$to_tile){
          # need extra code to move modules between tiles
          
          # in JS the dragged tab-head is already removed, otherwise nav_insert won't work properly
          #   we cannot use the select parameter (it doesn't work), probably because the inserted tab does not yet fully exist until we run the JS code below
          nav_insert(id = input$move_module$to_tile, position = "before", nav = nav_panel(
            title = input$move_module$tab_title,
            value = input$move_module$tab_id
          ))
          
          # tabs are always placed at the start of the list (because "before" in the previous command)
          # this script moves the tab to its proper place
          if (input$move_module$to_index > 0){
            shinyjs::runjs(stringr::str_c("setTimeout(function(){ var loc = $('#", input$move_module$to_tile, " :nth(",input$move_module$to_index+1,")'); $('#", input$move_module$to_tile, "').children().first().insertAfter($(loc))},100);"))
          }
          
          # - detach the module from its parent tab-panel and store it in a variable
          # - append the module to the new parent tab-panel
          # - move the new tab-panel in the right position so the order of the content matches the tab headers
          # - remove the old tab-panel itself
          shinyjs::runjs(stringr::str_c("setTimeout(function(){
        var mod = $('#", input$move_module$from_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$from_index,").children().detach();
        $('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().last().append($(mod));
        $('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$to_index,").before($('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().last());
        $('#", input$move_module$from_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$from_index,").remove();
      }, 100);"))
          
          # if moving the selected tab away, then select the next tab in that panel
          if (is.null(input[[input$move_module$from_tile]])){
            nav_select(input$move_module$from_tile, input$move_module$sibling_tab_id)
          }
          
          # if moving a tab to an empty panel, select it automatically
          if (is.null(input[[input$move_module$to_tile]])){
            nav_select(input$move_module$to_tile, input$move_module$tab_id)
            # ensure that Shiny know this panel is visible so it will actually render outputs
            print("Trigger visibility")
            shinyjs::runjs(stringr::str_c("
          $('[data-value=\"", input$move_module$tab_id,"\"]').show();
          $('[data-value=\"", input$move_module$tab_id,"\"]').trigger('shown');
        "))
          }
          
          # store in layout
          from_tile <- private$globals$elements[[input$move_module$from_tile]]
          to_tile <- private$globals$elements[[input$move_module$to_tile]]
          
          module_id <- from_tile$lose_module(input$move_module$from_index+1)
          to_tile$receive_module(module_id, input$move_module$to_index)
          # no set_parent here because modules don't know in which tile they are
          
        } else {
          # we switch only the order between tabs on a single tile
          # only thing needed is to update the order of the tabpanel divs as well so moving them outside the panel later will still work
          # because moving within a list updates the ordering, we need to place with .before when moving tabs to the left and with .after when moving to the right
          if (input$move_module$from_index > input$move_module$to_index){
            shinyjs::runjs(stringr::str_c("setTimeout(function(){
          $('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$to_index,").before($('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$from_index,"));
        }, 100);"))
          } else {
            shinyjs::runjs(stringr::str_c("setTimeout(function(){
          $('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$to_index,").after($('#", input$move_module$to_tile, "').parent().parent().children().eq(1).children().eq(",input$move_module$from_index,"));
        }, 100);"))
          }
        }
        
        
        
        # serialize!
        serialize(pages = private$globals$pages)
      }) %>% bindEvent(input$move_module)
      
      # Preferences ----
      observe({
        req(is.null(private$globals$modal))
        preferences <- ElementalPreferences$new(id = "app_preferences", globals = private$globals)
        preferences$start_server()
        showModal(modalDialog(preferences$get_ui(), footer = NULL, easyClose = TRUE))
        private$globals$modal <- preferences
      }) %>% bindEvent(input$preferences, input$key_v, ignoreInit = TRUE)
      
      # Keyboard shortcuts ----
      observe({
        req(is.null(private$globals$modal))
        shortcuts <- ElementalShortcuts$new(id = "shortcuts", globals = private$globals)
        shortcuts$start_server()
        showModal(modalDialog(shortcuts$get_ui(), footer = NULL, easyClose = TRUE))
        private$globals$modal <- shortcuts
      }) %>% bindEvent(input$shortcuts, input$key_questionmark, ignoreInit = TRUE)

      # Page title ----
      observe({
        req(is.null(private$globals$modal))
        edit_title <- ElementalEditTitle$new(id = stringr::str_c("edit-page-title"), globals = private$globals, ui_element = private$globals$pages[[input$page]])
        edit_title$start_server()
        showModal(modalDialog(edit_title$get_ui(), footer = NULL, easyClose = TRUE))
        private$globals$modal <- edit_title
      }) %>% bindEvent(input$change_page_title)
      
      # Modal flag
      observe({
        req(isFALSE(input$modal))
        if (!is.null(private$globals$modal) && "Element" %in% class(private$globals$modal)){
          private$globals$modal$remove()
          private$globals$modal <- NULL
        }
      }) %>% bindEvent(input$modal, ignoreInit =TRUE)
      
      # Add module ---
      # This needs to be done from the main session, otherwise it will be a child-module of the modal dialog module, which will mess with the namespace and hence the reactiveness
      observe({
        id = generate_id("mod")
        mod_class = get_class(input$add_module$module)
        mod <- mod_class$new(id, mod_class$name, private$globals, module_inputs = NULL, params = NULL, state = NULL)
        #mod$start_server() # this is done inside the tile for now
        private$globals$modules[[id]] <- mod
        private$globals$elements[[input$add_module$tile]]$add_module(mod)
        serialize(modules = private$globals$modules, pages = private$globals$pages, state = private$globals$state)
        
      }) %>% bindEvent(input$add_module)
      
      # Experiment with translations ---
      
      # purrr::walk(fromJSON(app_sys("app/translation.json"))$translation, function(entry){
      #   output[[stringr::str_c("t_",entry[[1]])]] <- renderText(quote({
      #     return(entry[[private$globals$language]])
      #   }), quoted = TRUE)
      # })
      
      
      # Select first page
      bslib::nav_select("page", isolate(private$globals$pages[[1]]$get_id()))
      waiter::waiter_hide()
    }
    
  ),
  
  public = list(
    
    #' @description
    #' Initialize an App 
    #'
    #' @param modules A list of R6Generators, result of sourcing files
    #' @param config A list, usually from reading json, that contains the specification of the dashboard
    #' @param language Default language the app needs to start in
    #'
    #' @returns An App object
    initialize = function(modules, config, language){
      private$theme <- create_theme()

      # Load i18n library
      private$i18n <- shiny.i18n::Translator$new(translation_json_path = app_sys("app/translation.json"))
      private$i18n$set_translation_language(language)
      # Link to it from globals so every module can use it
      private$globals$i18n <- private$i18n
      # Provide a shortcut method that always works because built-in function doesn't always trigger the dynamic translation
      private$globals$t <- function(text){
        shiny::span(class = "i18n", `data-key` = text, private$globals$i18n$t(text))
      }
      # The above generates a span tag, which does not work in all cases (like title tags on buttons, etc.)
      #  For those instances, a reactive value can be found in this list with the key language in the key
      #  and the target language in the value
      private$globals$text <- fromJSON(app_sys("app/translation.json"))$translation %>% purrr::map(function(x){list(x[[language]]) %>% setNames(x[[1]])}) %>% unlist(recursive = FALSE)
      
      # add all child objects of Module to the list of modules received from the user
      private$globals$all_modules <- c(modules, objects("package:elemental") %>% purrr::map(get) %>% purrr::keep(~all(class(.)=="R6ClassGenerator")) %>% purrr::keep(~!is.null(.$inherit) && .$inherit == "Module"))
      private$config <- config
    },
    
    #' @description
    #' Create a Shiny app that can be run
    #'
    #' @returns A Shiny app object that can be run with shiny::runApp()
    run = function(){
      # Run the application 
      
      return(shinyApp(ui = private$ui(), server = private$server))
    }
    
  )
)