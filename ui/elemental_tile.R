ElementalTile <- R6::R6Class(
  "ElementalTile",
  
  private = list(
    id = NA_character_,
    page_navbar_id = "page",
    group = "elemental_tile",
    title = NA_character_,
    modules = list(),
    parent = NULL,
    globals = NULL,
    observers = list()
    
    
  ),
  
  public = list(
    
    initialize = function(layout, parent, globals){
      # generate id, no ns as we want to be able to move these between pages
      private$id <- stringr::str_replace((stringr::str_c("tile-",as.numeric(lubridate::now()))), "[.]","")
      
      private$title <- layout$title
      
      private$parent <- parent

      private$globals <- globals
                  
      private$modules <- layout$modules # just a character vector of module id's
      
    },
    
    get_id = function(){
      return(private$id)
    },
    
    get_title = function(){
      return(private$title)
    },
    
    get_parent = function(){
      return(private$parent)
    },
    
    set_parent = function(column){
      private$parent <- column
    },
    
    remove_module = function(index){
      module_id <- private$modules[[index]] # need 1-index here
      private$modules <- private$modules[-index]
      return(module_id)
    },
    
    add_module = function(module_id, index){
      private$modules <- append(private$modules, module_id, index) # can use 0-index here
    },
    
    
    
    use_menu = NULL, # fill in with reactive function
    
    get_ui = function(){
      print(stringr::str_c("get ui Tile ", private$id))
      tagList(
        
        navset_card_tab(
          id = private$id,
          title = private$title,
          full_screen = TRUE,
          # modules go here later
          
          nav_item((actionLink(inputId = stringr::str_c(private$id,"-header-info"), label = "", icon = icon("info", style = "padding-left: 5px; padding-right: 5px;"))), class = "first_button button"),
          nav_item((actionLink(inputId = stringr::str_c(private$id,"-header-title"), label = "", icon = icon("pen-to-square"))), class = "button"),
          nav_item((actionLink(inputId = stringr::str_c(private$id,"-header-add"), label = "", icon = icon("plus", style = "padding-left: 1px; padding-right: 1px;"))), class = "button"),
          nav_item((actionLink(inputId = stringr::str_c(private$id,"-header-maximize"), label = "", icon = icon("up-right-and-down-left-from-center"))), class = "button"),
          nav_item((actionLink(inputId = stringr::str_c(private$id,"-header-settings"), label = "", icon = icon("cog"))), class = "button"),
          nav_item((actionLink(inputId = stringr::str_c(private$id,"-header-remove"), label = "", icon = icon("trash-can"))), class = "button"),
          
          nav_menu("", value = "_menu_", icon = icon("bars"),
                   nav_item(actionLink(inputId = stringr::str_c(private$id,"-menu-info"), label = "Start info tour", icon = icon("info", style = "padding-left: 5px; padding-right: 5px;"))),
                   nav_item(actionLink(inputId = stringr::str_c(private$id,"-menu-title"), label = "Verander tegel titel", icon = icon("pen-to-square"))),
                   nav_item(actionLink(inputId = stringr::str_c(private$id,"-menu-add"), label = "Module toevoegen", icon = icon("plus", style = "padding-left: 1px; padding-right: 1px;"))),
                   nav_item(shinyjs::hidden(actionLink(inputId = stringr::str_c(private$id,"-menu-maximize"), label = "Volledig scherm", icon = icon("up-right-and-down-left-from-center")))),
                   nav_item(shinyjs::hidden(actionLink(inputId = stringr::str_c(private$id,"-menu-settings"), label = "Module instellingen", icon = icon("cog")))),
                   nav_item(shinyjs::hidden(actionLink(inputId = stringr::str_c(private$id,"-menu-remove"), label = "Verwijder deze tegel", icon = icon("trash-can"))))
                   ),        
          
          
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
            if (!evt.related.classList.contains('button')){
              $(evt.related.children[0]).click()
            }
            return false
          } else { 
            return !evt.dragged.classList.contains('dropdown') && !evt.dragged.classList.contains('button') && (evt.related.className === 'nav-item' || (evt.related.classList.contains('first_button') && !evt.willInsertAfter));
          }
        }"
          ))
        )),
      )
    },
    
    complete_ui_reactive = function(input, output, session){
      print(stringr::str_c("complete UI for ", private$id))
      
      # add extra class to tablist ul element to ensure tabs are floating right also on Edge
      shinyjs::runjs(stringr::str_c("$('#",private$id,"').addClass('justify-content-end')"))
      
      # add tabIndex to tile so we can select the tile by tab and do tile actions directly with keyboard
      shinyjs::runjs(stringr::str_c("$('#",private$id,"').parent().parent().attr('tabindex',0)"))
      
      # insert module UIs
      purrr::iwalk(rev(private$modules), function(mod_id, mod_idx){

        mod <- private$globals$modules[[mod_id]]
        
        # easiest thing is to insert in reverse order so we can always add the newest tab at the front
        #   if we use "after" they appear after the buttons as well which is not what we want
        nav_insert(private$id, # works without namespace?
                   nav_panel(
                     id = mod$get_id(),
                     title = mod$get_title(),
                     value = mod$get_id(),
                     mod$get_ui()
                   ), position = "before", select = TRUE, session = session
        )
        # tabcontent divs are always inserted at the end, regardless of the "before" setting of nav_insert
        # this javascript swaps the order of the content div so it matches with the tab order
        if (mod_idx > 1){
          shinyjs::runjs(stringr::str_c("setTimeout(function(){
            $('#", private$id, "').parent().parent().children().eq(1).children().eq(",0,").before($('#", private$id, "').parent().parent().children().eq(1).children().eq(",mod_idx-1,"));
          }, 100);"))
        }
        
        if (!mod$is_active()){
          mod$start_server()
        }
      })
      
      
      
      private$observers$maximize <- observe({
        # we leverage the hidden fullscreen tooltip button (because of fullscreen=TRUE in the navset_card_tab) and just click it programmatically
        shinyjs::runjs(stringr::str_c("$('#", private$id, "').parent().parent().children().eq(2).children().click()"))
        
        
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-maximize")]], input[[stringr::str_c(private$id,"-header-maximize")]], ignoreInit = TRUE)
      
      private$observers$title <- observe({
        print("update title observer")
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-title")]], input[[stringr::str_c(private$id,"-header-title")]], ignoreInit = TRUE)
      
      private$observers$add <- observe({
        print("add module observer")
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-add")]], input[[stringr::str_c(private$id,"-header-add")]], ignoreInit = TRUE)
      
      private$observers$remove <- observe({
        print("remove tile observer")
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-remove")]], input[[stringr::str_c(private$id,"-header-remove")]], ignoreInit = TRUE)
      
      private$observers$settings <- observe({
        # show settings
        print(stringr::str_c(private$id,"-menu-settings", "  ", input[[private$id]]))
        
        settings <- ElementalModuleSettings$new(id = stringr::str_c(private$id,"-settings"), title = "Instellingen", globals = private$globals, module_inputs = list(), state = list(), module = private$globals$modules[[input[[private$id]]]])
        settings$start_server()
        showModal(modalDialog(settings$get_ui(), footer = NULL))
        
        
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-settings")]], input[[stringr::str_c(private$id,"-header-settings")]], ignoreInit = TRUE)
      
      private$observers$info <- observe({
        # start intro tour
        print(stringr::str_c(private$id,"-menu-info", "  ", input[[private$id]]))
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-info")]], input[[stringr::str_c(private$id,"-header-info")]], ignoreInit = TRUE)
      
      observe({
        print(stringr::str_c("Full screen: ",input[[stringr::str_c(private$id, "_full_screen")]]))
        private$globals$modules[[input[[private$id]]]]$set_fullscreen(input[[stringr::str_c(private$id, "_full_screen")]])
      }) %>% bindEvent(input[[stringr::str_c(private$id, "_full_screen")]], ignoreInit = TRUE)
      
      
      # toggle between having the tile actions in a menu or as separate icons in the tile header
      self$use_menu = function(tile_menu = TRUE){
        
        if (tile_menu){
          # show menu
          nav_show(private$id, "_menu_", session = session)
          # hide all header buttons
          shinyjs::runjs(stringr::str_c("$('#", private$id, " > .bslib-nav-item a').hide()"))
          
          # show/hide menu options depending on if there are modules shown in this tile
          if (length(private$modules) > 0){
            shinyjs::show(id = stringr::str_c(private$id,"-menu-settings"))
            shinyjs::show(id = stringr::str_c(private$id,"-menu-maximize"))
            
            shinyjs::hide(id = stringr::str_c(private$id,"-menu-remove"))
          } else {
            shinyjs::show(id = stringr::str_c(private$id,"-menu-remove"))
            
            shinyjs::hide(id = stringr::str_c(private$id,"-menu-settings"))
            shinyjs::hide(id = stringr::str_c(private$id,"-menu-maximize"))
          }
        } else {
          # hide the menu
          nav_hide(private$id, "_menu_", session = session)
          # show the default header buttons
          #shinyjs::runjs(stringr::str_c("$('#", private$id, " > .bslib-nav-item a').show()"))
          
          shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-info').show()"))
          shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-title').show()"))
          shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-add').show()"))
          
          # show/hide more header butons depending on if there are modules shown in this tile
          if (length(private$modules) > 0){
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-settings').show()"))
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-maximize').show()"))
            
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-remove').hide()"))
            
          } else {
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-settings').hide()"))
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-maximize').hide()"))
            

            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-remove').show()"))
            
          }
        }
      }
      self$use_menu(private$globals$preferences$tile_menu)
      
    },
    
    serialize = function(){
      list(class = class(self)[1], title = private$title, modules = setNames(private$modules, NULL))
    }
  )
)
  
