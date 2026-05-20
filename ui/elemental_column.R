ElementalColumn <- R6::R6Class(
  "ElementalColumn",
  
  private = list(
    id = NA_character_,
    page_navbar_id = "page",
    group = "elemental_column",
    tiles = list(),
    observers = list(),
    parent = NULL,
    globals = NULL
  ),
  
  public = list(
    
    initialize = function(layout, parent, globals){
      # generate id
      private$id <- stringr::str_replace(stringr::str_c("col-",as.numeric(lubridate::now())), "[.]","")
      
      private$parent <- parent
      private$globals <- globals
      
      # tiles
      private$tiles <- purrr::map(layout$tiles, ~ElementalTile$new(., self, globals)) %>% setNames(purrr::map_chr(., ~.$get_id()))
      
    },
    
    get_id = function(){
      return(private$id)
    },
    
    get_parent = function(){
      return(private$parent)
    },
    
    # just the object operation, not the UI part
    remove_tile = function(index){
      tile <- private$tiles[[index]] # need 1-index here
      private$tiles <- private$tiles[-index]
      return(tile)
    },
    
    # just the object operation, not the UI part
    add_tile = function(tile, index){
      private$tiles <- append(private$tiles, tile, index) # can use 0-index here
    },
    
    get_tile = function(tile_id){
      return(tiles[[tile_id]])
    },
    
    get_ui = function(){
      print(stringr::str_c("get ui Column ", private$id))
      
      tagList(
        if(length(private$tiles) == 0){
          layout_column_wrap(
            id = private$id,
            min_height = "100px",
            width = 1, heights_equal = "row",
            class = "layout layout-column",
            style = css(padding = "0px"),
          )
        } else {
          layout_column_wrap(
            id = private$id,
            min_height = "100px",
            width = 1, heights_equal = "row",
            class = "layout layout-column",
            style = css(padding = "0px"),
            !!!setNames(purrr::map(private$tiles, ~.$get_ui()), NULL)
          )
        },
        sortable::sortable_js(private$id, options = sortable::sortable_options(
          group = private$group, handle = ".card-header",
          onMove = htmlwidgets::JS(stringr::str_c(
            "function(evt){
              console.log(evt); 
              if (evt.related.parentElement.id === '", private$page_navbar_id,"'){
                $(evt.related.children[0]).click()
                return false
              } else { 
                return true
              }
            }"
          )),
          onEnd = htmlwidgets::JS(stringr::str_c(
            "function(evt){
              console.log(evt); 
              
              if (evt.newIndex !== evt.oldIndex | evt.from.id !== evt.to.id | evt.from.getAttribute('row_id') !== evt.to.getAttribute('row_id')){
                Shiny.setInputValue('move_tile', {'from_column': evt.from.id, 'from_index': evt.oldIndex, 'to_column': evt.to.id, 'to_index': evt.newIndex})
              }
            }"
          ))
        ))
      )
    },
    
    complete_ui_reactive = function(input, output, session){
      print(stringr::str_c("complete UI for ", private$id))
      
      # insert the background controls that are only visible when there is no tile in a column
      # they have to be inserted afterwards because otherwise they will be wrapped in a div that will take up space like a tile in the layout
      insertUI(stringr::str_c("#", private$id), "beforeBegin",
               div(
                 style = css(
                   position = "relative",
                   "margin-bottom" = "-8px"
                 ),
                 div(
                   class = "btn-group",
                   style = css(
                     position = "absolute",
                     float = "left",
                     top = "20px",
                     left = "calc(50% - 104px)",
                   ),
                   actionButton(stringr::str_c(private$id,"-addcolumnbefore"), "", icon = icon("arrow-right-to-bracket", class = "fa-rotate-180"), class = "btn-lg", onclick = htmlwidgets::JS("this.blur()")),
                   actionButton(stringr::str_c(private$id,"-addtile"), "", icon = icon("square-plus"), class = "btn-lg", onclick = htmlwidgets::JS("this.blur()")),
                   actionButton(stringr::str_c(private$id,"-removecolumn"), "", icon = icon("trash-can"), class = "btn-lg", onclick = htmlwidgets::JS("this.blur()")),
                   actionButton(stringr::str_c(private$id,"-addcolumnafter"), "", icon = icon("arrow-right-to-bracket"), class = "btn-lg", onclick = htmlwidgets::JS("this.blur()"))
                 )
               ), multiple = FALSE, immediate = TRUE, session = session)
      
      
      private$observers$addtile <- observe({
        addtile <- stringr::str_c(private$id, "-addtile")
        req(input[[addtile]])
        print(addtile)
        
        new_tile <- ElementalTile$new(list(title = "Nieuwe Tegel"), self, private$globals)
        private$tiles <- append(private$tiles, list(new_tile) %>% setNames(new_tile$get_id()))
        
        insertUI(stringr::str_c("#", private$id), "beforeEnd", 
                 div(
                   class = "bslib-grid-item bslib-gap-spacing html-fill-container",
                   new_tile$get_ui()
                 ), immediate = TRUE, session = session)
        
        new_tile$complete_ui_reactive(input, output, session)
        private$globals$elements <- append(private$globals$elements, list(new_tile) %>% setNames(new_tile$get_id()))
                 
        # serialize!
        serialize(private$globals$modules, private$globals$pages)
      }) %>% bindEvent(input[[stringr::str_c(private$id, "-addtile")]])
      
      private$observers$removecolumn <- observe({
        removecolumn <- stringr::str_c(private$id, "-removecolumn")
        req(input[[removecolumn]])
        print(removecolumn)
        
        # remove column
        private$parent$remove_column(private$id)

        # remove observers for this column
        purrr::walk(private$observers, ~.$destroy())
        
        # serialize!
        serialize(private$globals$modules, private$globals$pages)
      }) %>% bindEvent(input[[stringr::str_c(private$id, "-removecolumn")]])
      
      private$observers$addcolumnbefore <- observe({
        addcolumnbefore <- stringr::str_c(private$id, "-addcolumnbefore")
        req(input[[addcolumnbefore]])
        private$parent$add_column(private$id, "before", input, output, session)
        print(addcolumnbefore)
        
        # serialize included in parent$add_column
        
      }) %>% bindEvent(input[[stringr::str_c(private$id, "-addcolumnbefore")]])
      
      private$observers$addcolumnafter <- observe({
        addcolumnafter <- stringr::str_c(private$id, "-addcolumnafter")
        req(input[[addcolumnafter]])
        private$parent$add_column(private$id, "after", input, output, session)
        print(addcolumnafter)
        
        # serialize included in parent$add_column
      }) %>% bindEvent(input[[stringr::str_c(private$id, "-addcolumnafter")]])
      
      purrr::walk(private$tiles, ~.$complete_ui_reactive(input, output, session))
      private$globals$elements <- append(private$globals$elements, private$tiles)
    },
    
    serialize = function(){
      list(class = class(self)[1], tiles = purrr::map(private$tiles, ~.$serialize()) %>% setNames(NULL))
    }
  
      
  )
)