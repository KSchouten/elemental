elemental_column <- function(...,
                             id,
                             group = "elemental_column",
                             width = 1,
                             fixed_width = FALSE,
                             heights_equal = "row",
                             fill = TRUE,
                             fillable = TRUE,
                             height = NULL,
                             height_mobile = NULL,
                             min_height = "100px",
                             max_height = NULL,
                             gap = NULL,
                             class = "layout layout-column",
                             style = css(padding = "0px"),
                             page_navbar_id = "page"){
  
  tagList(
    layout_column_wrap(
      id = id,
      width = width,
      fixed_width = fixed_width,
      heights_equal = heights_equal,
      fill = fill,
      fillable = fillable,
      height = height,
      height_mobile = height_mobile,
      min_height = min_height,
      max_height = max_height,
      gap = gap,
      class = class,
      style = style,
      ...
    ),
    sortable::sortable_js(id, options = sortable::sortable_options(
      group = group, handle = ".card-header",
      onMove = htmlwidgets::JS(stringr::str_c(
        "function(evt){
        console.log(evt); 
        if (evt.related.parentElement.id === '", page_navbar_id,"'){
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
  
}

# insert the background controls that are only visible when there is no tile in a column
# they have to be inserted afterwards because otherwise they will be wrapped in a div that will take up space like a tile in the layout
insert_background_controls <- function(column_id){
  insertUI(stringr::str_c("#", column_id), "beforeBegin",
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
               actionButton(stringr::str_c(column_id,"-addcolumnbefore"), "", icon = icon("arrow-right-to-bracket", class = "fa-rotate-180"), class = "btn-lg"),
               actionButton(stringr::str_c(column_id,"-addtile"), "", icon = icon("square-plus"), class = "btn-lg"),
               actionButton(stringr::str_c(column_id,"-removecolumn"), "", icon = icon("trash-can"), class = "btn-lg"),
               actionButton(stringr::str_c(column_id,"-addcolumnafter"), "", icon = icon("arrow-right-to-bracket"), class = "btn-lg")
             )
           ), multiple = FALSE, immediate = TRUE)
}