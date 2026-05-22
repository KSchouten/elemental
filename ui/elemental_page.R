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
      return(private$rows[[row_id]])
    },
    
    get_ui = function(){
      print(stringr::str_c("get ui Page ", private$id))
      nav_panel(id = private$id, value = private$id, title = private$title, icon = icon(private$icon), 
                purrr::map(private$rows, ~.$get_ui())
      )
  
    },
    
    add_row = function(row_id, position = c("before", "after"), input, output, session){
      
      row_idx <- which(names(private$rows) == row_id)
      # adjust row_idx when adding before the selected row because append only has an 'after' argument, it works with after=0 fortunately
      if (!position %in% c("before", "after")){
        stop("Incorrect value for 'position' parameter, should be either 'before' or 'after'")
      }
      adjustment = if_else(position == "before", -1, 0) 
      
      # add new row object
      new_row <- ElementalRow$new(
        list(class = "ElementalRow", column_sizes = c(40,60), columns = list(
          list(class = "ElementalColumn", tiles = list(
            list(class = "ElementalTile", title = "Tegel")
          )),
          list(class = "ElementalColumn", tiles = list())
        )), self, private$globals)
      private$rows <- append(private$rows, list(new_row) %>% setNames(new_row$get_id()), after = row_idx + adjustment)
      
      # insert new row UI
      insertUI(
        stringr::str_c("[data-value='",private$id, "'] > div:nth-child(",row_idx,")"), #data-value=id 
        if_else(position == "before", "beforeBegin", "afterEnd"),
        div(
          class = "bslib-grid-item bslib-gap-spacing html-fill-container",
          new_row$get_ui()
        ),
        immediate = TRUE,
        session = session
      )
      
      # update global list of UI elements
      private$globals$elements <- append(private$globals$elements, list(new_row) %>% setNames(new_row$get_id()))
      
      # complete UI stuff for this row
      new_row$complete_ui_reactive(input, output, session)
      
      # serialize!
      serialize(pages = private$globals$pages)
    },
    
    remove_row = function(row_id, input, output, session, serialize = TRUE){
      # need the position of the row to remove it because the div with the row id is wrapped in a div without an id
      row_idx <- which(names(private$rows) == row_id)
      
      # update UI:
      # remove gutters, remove div, reset grid-template-columns, re-init split
      shinyjs::runjs(stringr::str_c("
                    $('[data-value=\"",private$id, "\"]').children().eq(",row_idx-1,").remove()
                  "))
      
      # remove column from layout object
      private$rows[[row_id]] <- NULL
      
      # update global list of UI elements, remove the deleted element from list
      private$globals$elements <- private$globals$elements[which(names(private$globals$elements)!=row_id)]
      
      
      # if page is empty, also remove it
      if (length(private$rows)==0){
        self$remove_me(input, output, session, serialize = FALSE)
      }
      
      # serialize!
      if (serialize){
        serialize(pages = private$globals$pages)
      }
      
    },
    
    remove_me = function(input, output, session, serialize = TRUE){
      private$globals$pages[[private$id]] <- NULL
      
      nav_remove("page", private$id, session)
      
      # serialize!
      if (serialize){
        serialize(pages = private$globals$pages)
      }
    },
    
    complete_ui_reactive = function(input, output, session){
      if (!private$active){
        private$active <- TRUE
        print(stringr::str_c("complete UI for ", private$id))
        purrr::walk(private$rows, function(row){
          row$complete_ui_reactive(input, output, session)
        })
        private$globals$elements <- append(private$globals$elements, private$rows)
      } else {
        stop("ElementalPage object can only be activated once")
      }
    },
    
    serialize = function(){
      list(class = class(self)[1], title = private$title, icon = private$icon, rows = purrr::map(private$rows, ~.$serialize()) %>% setNames(NULL))
    }
  )
)

