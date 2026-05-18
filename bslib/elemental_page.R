ElementalPage <- R6::R6Class(
  "ElementalPage", 
  
  private = list(
    id = NA_character_,
    title = NA_character_,
    icon = NA_character_,

    rows = list(),
    
    active = FALSE,
    
    globals = NULL
  ),
  
  public = list(
    initialize = function(layout, globals){
      private$id <- stringr::str_replace(stringr::str_c("page-",as.numeric(lubridate::now())), "[.]","")
      private$title <- layout$title
      private$icon <- layout$icon
      private$globals <- globals
      
      # init layout with row objects
      private$rows <- purrr::map(layout$rows, ~ElementalRow$new(., self, globals)) %>% setNames(purrr::map_chr(., ~.$get_id()))
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

    get_row = function(row_id){
      return(layout[[row_id]])
    },
    
    get_ui = function(){
      print(stringr::str_c("get ui Page ", private$id))
      nav_panel(id = private$id, value = private$id, title = private$title, icon = icon(private$icon), 
                purrr::map(private$rows, ~.$get_ui())
      )
  
    },
    
    complete_ui_reactive = function(input, output, session){
      if (!private$active){
        private$active <- TRUE
        print(stringr::str_c("complete UI for ", private$id))
        purrr::walk(private$rows, function(row){
          row$complete_ui_reactive(input, output, session)
        })
      } else {
        stop("ElementalPage object can only be activated once")
      }
    }
  )
)

