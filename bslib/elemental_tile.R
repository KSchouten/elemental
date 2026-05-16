ElementalTile <- R6::R6Class(
  "ElementalTile",
  
  private = list(
    id = NA_character_,
    page_navbar_id = "page",
    group = "elemental_tile",
    title = NA_character_,
    modules = list(),
    parent_column = NULL,
    global_session = NULL,
    ns = NULL
  ),
  
  public = list(
    
    initialize = function(layout, parent, ns, global_session){
      # generate id, no ns as we want to be able to move these between pages
      private$id <- stringr::str_replace((stringr::str_c("tile-",as.numeric(lubridate::now()))), "[.]","")
      
      private$title <- layout$title
      
      private$parent_column <- parent

      private$global_session <- global_session
      private$ns <- ns
                  
      private$modules <- layout$modules
      
    },
    
    # # Set parent row object, only for initial filling, not for updating
    # set_parent_column = function(parent_column){
    #   if(is.null(private$parent_column)){
    #     private$parent_column <- parent_column
    #   }
    # },
    
    get_id = function(){
      return(private$id)
    },
    
    get_title = function(){
      return(private$title)
    },
    
    get_parent_column = function(){
      return(private$parent_column)
    },
    
    get_ui = function(){
      print(stringr::str_c("get ui Tile ", private$id))
      tagList(
        
        navset_card_tab(
          id = private$id,
          title = private$title,
          full_screen = TRUE,
          # modules go here later
          nav_item(actionLink(inputId = stringr::str_c(private$id,"-cog"), label = "", icon = icon("cog")), class = "first_button button"),
          nav_item(actionLink(inputId = stringr::str_c(private$id,"-info"), label = "", icon = icon("info")), class = "button"),
          
        ),
        
        sortable::sortable_js(private$id, options = sortable::sortable_options(
          group = private$group,
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
          if (evt.related.parentElement.id === '", private$page_navbar_id, "'){
            $(evt.related.children[0]).click()
            return false
          } else { 
            return !evt.dragged.classList.contains('button') && (evt.related.className === 'nav-item' || (evt.related.classList.contains('first_button') && !evt.willInsertAfter));
          }
        }"
          ))
        )),
      )
    },
    
    complete_ui_reactive = function(input, output, session){
      
      # insert module UIs
      purrr::iwalk(rev(private$modules), function(mod_id, mod_idx){
        
        mod <- self$get_parent_column()$get_parent_row()$get_parent_page()$get_module(mod_id)
        
        # easiest thing is to insert in reverse order so we can always add the newest tab at the front
        #   if we use "after" they appear after the buttons as well which is not what we want
        nav_insert(private$id, # works without namespace?
                   nav_panel(
                     id = mod$get_full_id(),
                     title = mod$get_title(),
                     value = mod$get_id(),
                     mod$get_ui()
                   ), position = "before", select = TRUE, session = private$global_session
        )
        # tabcontent divs are always inserted at the end, regardless of the "before" setting of nav_insert
        # this javascript swaps the order of the content div so it matches with the tab order
        if (mod_idx > 1){
          shinyjs::runjs(stringr::str_c("setTimeout(function(){
            $('#", private$id, "').parent().parent().children().eq(1).children().eq(",0,").before($('#", private$id, "').parent().parent().children().eq(1).children().eq(",mod_idx-1,"));
          }, 100);"))
        }
      })
    }
  )
)
  
