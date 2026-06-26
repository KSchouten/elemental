ElementalModuleSettings <- R6::R6Class(
  "ElementalModuleSettings", 
  inherit = Element,
  
  private = list(
    
    tile = NULL,
    module = NULL,
    all_available_exports = list(),
    
    # Override this for module-specific UI
    ui = function(){
      
      ns <- NS(private$id)
      uiOutput(ns("modal_ui"))
    },
    
    server = function(input, output, session){
      ns <- session$ns

      output$modal_ui <- renderUI({
        tagList(
          h1(private$globals$t("Module settings")),
          p(private$module()$get_id()),
          
          textInput(ns("title"), private$globals$t("Title"), private$module()$get_title()),
          if(length(private$module()$get_params())>0){
            tagList(
              h4(private$globals$t("Parameters")),
              !!!purrr::map(private$module()$get_params(), function(name){
                textInput(ns(stringr::str_c("param-", name)), name, private$module()$get_param(name))
              })
            )
          },
          if(length(private$module()$get_inputs())>0){
            tagList(
              h4(private$globals$t("Dependencies")),
              !!!purrr::map(private$module()$get_inputs(), function(name){
                print(stringr::str_c(private$module()$get_input(name), collapse = " "))
                selectInput(ns(stringr::str_c("input-", name)), name, private$all_available_exports, selected = stringr::str_c(private$module()$get_input(name), collapse = " "))
              })
            )
          },
          actionButton(ns("done"), private$globals$t("Done"))
        )
      })
      
      observe({
        self$remove()
        removeModal()
        private$tile$close_settings_dialog()
      }) %>% bindEvent(input$done)

      observe({
        print(input$title)
        # update title
        private$module()$set_title(input$title)
       
        shinyjs::runjs(stringr::str_c("$('a[data-value=", private$module()$get_id(),"]').text('", input$title, "')"))
        
        serialize(modules = private$globals$modules)
      }) %>% bindEvent(input[["title"]], ignoreInit = TRUE)
      
      reactive_observers <- list()
      observe({
        reactive_observers %>% purrr::walk(~.$destroy())
        reactive_observers <- c(
          purrr::map(private$module()$get_params(), function(name){
            observe({
              quote({
                value <- input[[stringr::str_c("param-", name)]]
                req(value)
                
                # update param
                private$module()$set_param(name, value)
              })
            }, quoted = TRUE) %>% bindEvent(input[[stringr::str_c("param-", name)]], ignoreInit = TRUE)
          }),
              
          purrr::map(private$module()$get_inputs(), function(name){
            observe({
              quote({
                
                value <- input[[stringr::str_c("input-", name)]]
                req(value)
                print(value)
                # update input dependency
                private$module()$set_input(name, stringr::str_split_1(value, " "))
              })
            }, quoted = TRUE) %>% bindEvent(input[[stringr::str_c("input-", name)]], ignoreInit = TRUE)
          })
        )
      }) %>% bindEvent(private$module())
      
      self$update_module_selection <- function(module){
        private$module(module)
      }
    }
  ),
  
  public = list(
    
    initialize = function(id, title, globals, tile, module){
      super$initialize(id, title, globals)
      private$tile <- tile
      private$module <- reactiveVal(NULL)
      # When creating a new module, the settings dialog is called before the new module is properly initialized.
      # It is possible that at the moment of creation, the module does not exist yet and would give an error which is why a try({}) is necessary here
      try({
        private$module <- reactiveVal(module)
      })
      
      private$all_available_exports <- append(list("") %>% setNames(private$globals$i18n$t("Choose value from another module")), purrr::map(private$globals$modules, function(m){
        stringr::str_c(m$get_id(), " ", m$get_outputs()) %>% setNames(stringr::str_c(m$get_title(), " -> ", m$get_outputs()))
      }) %>% purrr::flatten())
    },
    
    update_module_selection = NULL # fill in with reactive function
    
    
    
  )
)