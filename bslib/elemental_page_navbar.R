elemental_page_navbar <- function(..., title = NULL, id = "page", selected = NULL, sidebar = NULL, 
                                  fillable = FALSE, fillable_mobile = FALSE, gap = NULL, padding = NULL, 
                                  header = NULL, footer = NULL, navbar_options = NULL, fluid = TRUE, 
                                  theme = bs_theme(), window_title = NA, lang = NULL, draggable_groups = c("elemental_tile", "elemental_column")){
  
  tagList(
    page_navbar(
      title = title,
      id = id,
      selected = selected,
      sidebar = sidebar,
      fillable = fillable,
      fillable_mobile = fillable_mobile,
      gap = gap,
      padding = padding,
      header = header,
      footer = footer,
      navbar_options = navbar_options,
      fluid = fluid,
      theme = theme,
      window_title = window_title,
      lang = lang,
      ...
    ),
    purrr::map(draggable_groups, function(group){
      sortable::sortable_js(id, options = sortable::sortable_options(
        group = group,
        onMove = htmlwidgets::JS(stringr::str_c(
          "function(evt){console.log(evt.related);return evt.related.parentElement.id === '", id, "';}"
        )),
        onEnd = htmlwidgets::JS(stringr::str_c("
          function(evt){
            if (evt.newIndex !== evt.oldIndex){
              Shiny.setInputValue('move_page', {'from_index': evt.oldIndex, 'to_index': evt.newIndex})
            }
          }
          "
        ))
      ))
    })
  )
}