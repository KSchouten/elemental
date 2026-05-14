elemental_tile <- function(..., id, group = "elemental_tile", selected = NULL, title = NULL, sidebar = NULL, header = NULL, footer = NULL, height = NULL, full_screen = TRUE, wrapper = card_body, page_navbar_id = "page"){
  
  tagList(
    
    navset_card_tab(
      id = id,
      selected = selected,
      title = title,
      sidebar = sidebar,
      header = header,
      footer = footer,
      height = height,
      full_screen = full_screen,
      wrapper = wrapper,
      ...,
      nav_item(actionLink(inputId = stringr::str_c(id,"-cog"), label = "", icon = icon("cog")), class = "first_button button"),
      nav_item(actionLink(inputId = stringr::str_c(id,"-info"), label = "", icon = icon("info")), class = "button"),
      
    ),
    
    sortable::sortable_js(id, options = sortable::sortable_options(
      group = group,
      onEnd = htmlwidgets::JS(stringr::str_c(
        "function(evt){
          console.log(evt); 
          var tab_title = $(evt.item).children().html();
          var tab_id = $(evt.item).children().attr('data-value')
          var sibling_tab_id = $($($('#'+evt.from.id).children()[0]).children()[0]).attr('data-value')
          var place_before_id = $($($(evt.to).children()[evt.newIndex+1]).children()[0]).attr('data-value')
        
          if (evt.newIndex !== evt.oldIndex | evt.from.id !== evt.to.id){
            Shiny.setInputValue('move_module', {'from_tile': evt.from.id, 'from_index': evt.oldIndex, 'to_tile': evt.to.id, 'to_index': evt.newIndex, 'tab_title': tab_title, 'tab_id': tab_id, 'sibling_tab_id': sibling_tab_id, 'place_before_id': place_before_id})
          }
          if (evt.from.id !== evt.to.id){
            evt.item.remove()
          }
        
        }"
      )),
      onMove = htmlwidgets::JS(stringr::str_c(
        "function(evt){
          console.log(evt); 
          if (evt.related.parentElement.id === '", page_navbar_id, "'){
            $(evt.related.children[0]).click()
            return false
          } else { 
            return !evt.dragged.classList.contains('button') && (evt.related.className === 'nav-item' || (evt.related.classList.contains('first_button') && !evt.willInsertAfter));
          }
        }"
      ))
    )),
   
    # sortable::sortable_js(page_navbar_id, options = sortable::sortable_options(
    #   group = group,
    #   onMove = htmlwidgets::JS(stringr::str_c(
    #     "function(evt){console.log(evt.related);return evt.related.parentElement.id === '", page_navbar_id, "';}"
    #   ))
    # )), 
  )
}