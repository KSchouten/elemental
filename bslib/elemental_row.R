ElementalRow <- R6::R6Class(
  "ElementalRow",
  
  private = list(
    id = NA_character_,
    page_navbar_id = "page",
    column_sizes = list(),
    columns = list(),
    parent_page = NULL,
    global_session = NULL
  ),
  
  public = list(
    
    initialize = function(layout, parent, ns, global_session){
      # generate id
      private$id <- stringr::str_replace(ns(stringr::str_c("row-",as.numeric(lubridate::now()))), "[.]","")
      
      # column_sizes
      private$column_sizes <- layout$column_sizes
      
      private$parent_page <- parent
      private$global_session <- global_session
      
      # columns
      private$columns <- purrr::map(layout$columns, ~ElementalColumn$new(., self, ns, global_session)) %>% setNames(purrr::map_chr(., ~.$get_id()))
      
    },
    
    # # Set parent row object, only for initial filling, not for updating
    # set_parent_page = function(parent_page){
    #   if(is.null(private$parent_page)){
    #     private$parent_page <- parent_page
    #   }
    # },
    
    get_id = function(){
      return(private$id)
    },
    
    get_columns = function(){
      return(private$columns)
    },
    
    get_column_sizes = function(){
      return(private$column_sizes)
    },
    
    get_parent_page = function(){
      return(private$parent_page)
    },
    
    remove_column = function(column_id){
      # need the position of the column to remove it because the div with the column id is wrapped in a div without an id
      col_idx <- which(names(private$columns) == column_id)
      
      # compute new column sizes for remaining columns
      new_col_sizes <- unlist(private$column_sizes[-col_idx])
      private$column_sizes <- new_col_sizes / sum(new_col_sizes) * 100

      # update UI:
      # remove gutters, remove div, reset grid-template-columns, re-init split
      shinyjs::runjs(stringr::str_c("
                    $('#",private$id," > .gutter').remove()
                    $('#",private$id,"').children().eq(",col_idx-1,").remove()
                    $('#",private$id,"').css('grid-template-columns', '", stringr::str_c(private$column_sizes, "fr", collapse = " 10px "), "');
                    Split($('#",private$id, " > div'), {sizes: [",private$column_sizes %>% stringr::str_c(collapse = ", "),"], minSize: 250})
                  "))
      
      # remove column from layout object
      private$columns[[column_id]] <- NULL
    },
    
    get_ui = function(){
      print(stringr::str_c("get ui Row ", private$id))
      tagList(
        layout_column_wrap(
          id = private$id,
          class = "layout layout-row",
          style = css(grid_template_columns = stringr::str_c(private$column_sizes, "fr", collapse = " 10px "),
                      padding = "10px",
                      margin = "-10px"),
          !!!setNames(purrr::map(private$columns, ~.$get_ui()), NULL)
        )
        
      )
    },
    
    complete_ui_reactive = function(input, output, session){
      print(stringr::str_c("complete UI for ", private$id))
      
      col_sizes <- private$column_sizes %>% stringr::str_c(collapse = ", ")
      shinyjs::runjs(stringr::str_c("Split($('#",private$id, " > div'), {sizes: [",col_sizes,"], minSize: 250})"))
      
      purrr::walk(private$columns, ~.$complete_ui_reactive(input, output, session))
    }
    
    
    
    
  )
)