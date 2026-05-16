ElementalColumn <- R6::R6Class(
  "ElementalColumn",
  
  private = list(
    id = NA_character_,
    ns_id = NA_character_,
    page_navbar_id = "page",
    group = "elemental_column",
    tiles = list(),
    observers = list(),
    parent_row = NULL,
    global_session = NULL,
    ns = NULL
  ),
  
  public = list(
    
    initialize = function(layout, parent, ns, global_session){
      # generate id
      private$id <- stringr::str_replace(stringr::str_c("col-",as.numeric(lubridate::now())), "[.]","")
      private$ns_id <- ns(private$id)
      
      private$parent_row <- parent
      private$global_session <- global_session
      private$ns <- ns
      
      # tiles
      private$tiles <- purrr::map(layout$tiles, ~ElementalTile$new(., self, ns, global_session)) %>% setNames(purrr::map_chr(., ~.$get_id()))
    },
    
    # # Set parent row object, only for initial filling, not for updating
    # set_parent_row = function(parent_row){
    #   if(is.null(private$parent_row)){
    #     private$parent_row <- parent_row
    #   }
    # },
    
    get_id = function(){
      return(private$ns_id)
    },
    
    get_parent_row = function(){
      return(private$parent_row)
    },
    
    get_ui = function(){
      print(stringr::str_c("get ui Column ", private$ns_id))
      
      tagList(
        if(length(private$tiles) == 0){
          layout_column_wrap(
            id = private$ns_id,
            width = 1, heights_equal = "row",
            class = "layout layout-column",
            style = css(padding = "0px"),
          )
        } else {
          layout_column_wrap(
            id = private$ns_id,
            width = 1, heights_equal = "row",
            class = "layout layout-column",
            style = css(padding = "0px"),
            !!!setNames(purrr::map(private$tiles, ~.$get_ui()), NULL)
          )
        },
        sortable::sortable_js(private$ns_id, options = sortable::sortable_options(
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
    
              if (evt.newIndex !== evt.oldIndex | evt.from.id !== evt.to.id){
                Shiny.setInputValue('move_tile', {'from_column': evt.from.id, 'from_index': evt.oldIndex, 'to_column': evt.to.id, 'to_index': evt.newIndex})
              }
            }"
          ))
        ))
      )
    },
    
    complete_ui_reactive = function(input, output, session){
      print(stringr::str_c("complete UI for ", private$ns_id))
      
      # insert the background controls that are only visible when there is no tile in a column
      # they have to be inserted afterwards because otherwise they will be wrapped in a div that will take up space like a tile in the layout
      insertUI(stringr::str_c("#", private$ns_id), "beforeBegin",
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
                     top = "50px",
                     left = "calc(50% - 104px)",
                   ),
                   actionButton(stringr::str_c(private$ns_id,"-addcolumnbefore"), "", icon = icon("arrow-right-to-bracket", class = "fa-rotate-180"), class = "btn-lg"),
                   actionButton(stringr::str_c(private$ns_id,"-addtile"), "", icon = icon("square-plus"), class = "btn-lg"),
                   actionButton(stringr::str_c(private$ns_id,"-removecolumn"), "", icon = icon("trash-can"), class = "btn-lg"),
                   actionButton(stringr::str_c(private$ns_id,"-addcolumnafter"), "", icon = icon("arrow-right-to-bracket"), class = "btn-lg")
                 )
               ), multiple = FALSE, immediate = TRUE, session = private$global_session)
      
      
      private$observers$addtile <- observe({
        addtile <- stringr::str_c(private$id, "-addtile")
        req(input[[addtile]])
        print(addtile)
      })
      
      private$observers$removetile <- observe({
        removecolumn <- stringr::str_c(private$id, "-removecolumn")
        req(input[[removecolumn]])
        print(removecolumn)
        
        # remove column
        private$parent_row$remove_column(private$ns_id)

        # remove observers for this column
        purrr::walk(private$observers, ~.$destroy())
      })
      
      private$observers$addcolumnbefore <- observe({
        addcolumnbefore <- stringr::str_c(private$id, "-addcolumnbefore")
        req(input[[addcolumnbefore]])
        private$parent_row$add_column(private$ns_id, "before", input, output, session)
        print(addcolumnbefore)
      })
      
      private$observers$addcolumnafter <- observe({
        addcolumnafter <- stringr::str_c(private$id, "-addcolumnafter")
        req(input[[addcolumnafter]])
        private$parent_row$add_column(private$ns_id, "after", input, output, session)
        print(addcolumnafter)
      })
      
      purrr::walk(private$tiles, ~.$complete_ui_reactive(input, output, session))
    }
  
      
  )
)