ElementalRow <- R6::R6Class(
  "ElementalRow",
  
  private = list(
    id = NA_character_,
    page_navbar_id = "page",
    column_sizes = list(),
    columns = list(),
    parent = NULL,
    globals = NULL,
    observers = list()
  ),
  
  public = list(
    
    initialize = function(layout, parent, globals){
      # generate id
      private$id <- stringr::str_replace(stringr::str_c("row-",as.numeric(lubridate::now())), "[.]","")
      
      # column_sizes
      private$column_sizes <- layout$column_sizes
      
      private$parent <- parent
      private$globals <- globals
      
      # columns
      private$columns <- purrr::map(layout$columns, ~ElementalColumn$new(., self, globals)) %>% setNames(purrr::map_chr(., ~.$get_id()))
      
    },
    
    get_id = function(){
      return(private$id)
    },
    
    get_column = function(column_id){
      return(private$columns[[column_id]])
    },
    
    get_column_sizes = function(){
      return(private$column_sizes)
    },
    
    get_parent = function(){
      return(private$parent)
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
      shinyjs::runjs(stringr::str_c("Split($('#",private$id, " > div'), {sizes: [",col_sizes,"], minSize: 270, onDragEnd: function(sizes){Shiny.setInputValue('",private$id,"-resize', sizes);}})"))
      
      private$observers$resize <- observe({
        print(stringr::str_c(private$id, "-resize"))
        print(input[[stringr::str_c(private$id, "-resize")]])
        private$column_sizes <- input[[stringr::str_c(private$id, "-resize")]]
        # serialize
        serialize(private$globals$modules, private$globals$pages)
      }) %>% bindEvent(input[[stringr::str_c(private$id, "-resize")]])
      
      purrr::walk(private$columns, ~.$complete_ui_reactive(input, output, session))
      private$globals$elements <- append(private$globals$elements, private$columns)
    },
    
    remove_column = function(column_id, input, output, session){
      # need the position of the column to remove it because the div with the column id is wrapped in a div without an id
      col_idx <- which(names(private$columns) == column_id)

      # compute new column sizes for remaining columns
      new_col_sizes <- unlist(private$column_sizes[-col_idx])
      private$column_sizes <- new_col_sizes / sum(new_col_sizes) * 100
      
      # remove column from layout object
      private$columns[[column_id]] <- NULL
      
      # update global list of UI elements, remove the deleted element from list
      private$globals$elements <- private$globals$elements[which(names(private$globals$elements)!=column_id)]
      
      # update UI: remove gutters, remove div 
      shinyjs::runjs(stringr::str_c("
                    $('#",private$id," > .gutter').remove()
                    $('#",private$id,"').children().eq(",col_idx-1,").remove()
                  "))
                  
      if (length(private$columns)==0){
        # if this was the last column, also remove the row
        private$parent$remove_row(private$id, input, output, session, serialize = FALSE)
      } else {
        # otherwise reset grid-template-columns, re-init split
        shinyjs::runjs(stringr::str_c("
                    $('#",private$id,"').css('grid-template-columns', '", stringr::str_c(private$column_sizes, "fr", collapse = " 10px "), "');
                    Split($('#",private$id, " > div'), {sizes: [",private$column_sizes %>% stringr::str_c(collapse = ", "),"], minSize: 270, onDragEnd: function(sizes){Shiny.setInputValue('",private$id,"-resize', sizes);}})
                  "))
      }
      
      # serialize!
      serialize(private$globals$modules, private$globals$pages)
    },
    
    add_column = function(column_id, position = c("before", "after"), input, output, session){
      
      col_idx <- which(names(private$columns) == column_id)
      # adjust col_idx when adding before the selected column because append only has an 'after' argument, it works with after=0 fortunately
      if (!position %in% c("before", "after")){
        stop("Incorrect value for 'position' parameter, should be either 'before' or 'after'")
      }
      adjustment = if_else(position == "before", -1, 0) 
      
      # compute new column sizes
      private$column_sizes <- unlist(private$column_sizes %>% append(mean(unlist(private$column_sizes)), after = col_idx + adjustment))
      private$column_sizes <- private$column_sizes / sum(private$column_sizes) * 100
      
      # add new column object
      new_col <- ElementalColumn$new(list(), self, private$globals)
      private$columns <- append(private$columns, list(new_col) %>% setNames(new_col$get_id()), after = col_idx + adjustment)
      
      # update UI
      # remove gutters
      shinyjs::runjs(stringr::str_c("$('#",private$id," > .gutter').remove()"))
      # insert new column UI
      insertUI(
        stringr::str_c("#",private$id, " > div:nth-child(",col_idx,")"),
        if_else(position == "before", "beforeBegin", "afterEnd"),
        div(
          class = "bslib-grid-item bslib-gap-spacing html-fill-container",
          new_col$get_ui()
        ),
        immediate = TRUE,
        session = session
      )
      # re-init splitjs              
      shinyjs::runjs(stringr::str_c("
        $('#",private$id,"').css('grid-template-columns', '", stringr::str_c(private$column_sizes, "fr", collapse = " 10px "), "');
        Split($('#",private$id, " > div'), {sizes: [",private$column_sizes %>% stringr::str_c(collapse = ", "),"], minSize: 270, onDragEnd: function(sizes){Shiny.setInputValue('",private$id,"-resize', sizes);}})
      "))
      
      # use same class for new column as for existing column (this helps with the layout-visible thing)
      shinyjs::runjs(stringr::str_c("
        $('#",new_col$get_id(),"').attr('class', $('#",column_id, "').attr('class'))
      "))
      
      # update global list of UI elements
      private$globals$elements <- append(private$globals$elements, list(new_col) %>% setNames(new_col$get_id()))
      
      # complete UI stuff for this column (new background controls + observers)
      new_col$complete_ui_reactive(input, output, session)
      
      # serialize!
      serialize(private$globals$modules, private$globals$pages)
    },
    
    serialize = function(){
      list(class = class(self)[1], column_sizes = private$column_sizes, columns = purrr::map(private$columns, ~.$serialize()) %>% setNames(NULL))
    }

  )
)