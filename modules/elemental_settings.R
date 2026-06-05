ElementalSettings <- R6::R6Class(
  "ElementalSettings", 
  inherit = Module,
  
  private = list(
    
    default_name = "Instellingen",
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
        p(private$module$get_id()),
        
        textInput(ns("title"), "Titel", private$module$get_title()),
        
        h4("Parameters"),
        !!!purrr::map(private$module$get_params(), function(name){
          textInput(ns(stringr::str_c("param-", name)), name, private$module$get_param(name))
        }),
        h4("Afhankelijkheden"),
        !!!purrr::map(private$module$get_inputs(), function(name){
          
          selectInput(ns(stringr::str_c("input-", name)), name, private$exports, selected = stringr::str_c(private$module$get_input(name), collapse = " "))
        }),
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
        print(input$title)
        # update title
        
      }) %>% bindEvent(input[["title"]])
            
      purrr::walk(private$module$get_inputs(), function(name){
        observe({
          quote({
            value <- input[[stringr::str_c("input-", name)]]
            req(value)
            print(value)
            # update input dependency
            private$module$set_input(name, stringr::str_split_1(value, " "))
          })
        }, quoted = TRUE) %>% bindEvent(input[[stringr::str_c("input-", name)]], ignoreInit = TRUE)
      })
      
      # purrr::map(private$module$get_inputs(), function(name){
      #   id <- stringr::str_c(private$id, "-", name)
      #   updateSelectInput(inputId = id, choices = exports, selected = input[[id]])
      # })
    }
  ),
  
  public = list(
    
    initialize = function(id, title, globals, module_inputs, state, module){
      super$initialize(id, title, globals, module_inputs, state)
      private$module <- module
      
      private$exports <- purrr::map(private$globals$modules, function(m){
        stringr::str_c(m$get_id(), " ", m$get_outputs()) %>% setNames(stringr::str_c(m$get_title(), " -> ", m$get_outputs()))
      }) %>% purrr::flatten()
    }
    
  )
)