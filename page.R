Page <- R6::R6Class(
  "Page", 
  
  private = list(
    id = NA_character_,
    title = NA_character_,
    icon = NA_character_,
    
    globals = list(),
    modules = list(),
    layout = list(),
    
    active = FALSE,
    
    global_session = NULL
  ),
  
  public = list(
    initialize = function(id, title, icon, globals, modules, layout, global_session){
      private$id <- id
      private$title <- title
      private$icon <- icon
      private$globals <- globals
      private$modules <- modules
      private$global_session <- global_session
      
      # init layout with row objects
      private$layout <- purrr::map(layout, ~ElementalRow$new(., self, NS(private$id), global_session)) %>% setNames(purrr::map_chr(., ~.$get_id()))
    },
    
    get_id = function(){
      return(private$id)
    },
    get_title = function(){
      return(private$title)
    },
    get_active = function(){
      return(private$active)
    },
    get_module = function(id){
      return(private$modules[[id]])
    },
    get_row = function(row_id){
      return(layout[[row_id]])
    },
    
    get_ui = function(){
      ns <- NS(private$id)
      print(stringr::str_c("get ui Page ", private$id))
      nav_panel(id = private$id, value = private$id, title = private$title, icon = icon(private$icon), 
                purrr::map(private$layout, ~.$get_ui())
      )
  
    },
    
    start_server = function(){
      private$active <- TRUE
      moduleServer(private$id, function(input, output, session){
        ns <- session$ns
        
        print(stringr::str_c("start server function for ", private$id))

        # Initialize module objects
        purrr::walk(names(private$modules), function(id){
          private$modules[[id]] <- private$modules[[id]]$class$new(id, ns(id), private$modules[[id]]$title, private$globals, purrr::map(private$modules[[id]]$imports, unlist))
        })
        
        # Start server functions of modules
        purrr::walk(names(private$modules), ~private$modules[[.]]$start_server())
        
        # UI observers for generated buttons
        observers <- list()
        
        # manipulate UI after rendering, do the UI things that we need a reactive context for
        # - initialize splitJS
        # - add column background controls
        # - add modules to tiles
        observe({
          print(stringr::str_c("complete UI for ", private$id))

          # intialize splitjs on each row
          purrr::walk(private$layout, function(row){
            row$complete_ui_reactive(input, output, session)
          })
            
        })
        
        return(NULL)
      })
    }
  )
)

