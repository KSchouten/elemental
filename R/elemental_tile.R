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
    observers = list(),
    
    # if there is a settings dialog open, this will reference it so we can have direct interaction between the tile and the dialog
    # if there is no settings dialog open, this will be NULL
    settings = NULL
    
  ),
  
  public = list(
    
    initialize = function(layout, parent, globals){
      # generate id, no ns as we want to be able to move these between pages
      private$id <- generate_id("tile")
      
      private$title <- layout$title
      
      private$parent <- parent

      private$globals <- globals
                  
      private$modules <- layout$modules # just a character vector of module id's
      
    },
    
    get_id = function(){
      return(private$id)
    },
    set_title = function(title){
      private$title <- title
      shinyjs::runjs(stringr::str_c("$('#",private$id,"').parent().children().eq(0).text('", title, "')"))
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
    
    close_settings_dialog = function(){
      private$settings <- NULL
    },
    
    # remove a still existing module (dragged to another tile)
    lose_module = function(index){
      module_id <- private$modules[[index]] # need 1-index here
      private$modules <- private$modules[-index]
      self$use_menu()
      return(module_id)
    },
    
    # adding an existing module (dragged from another tile)
    receive_module = function(module_id, index){
      private$modules <- append(private$modules, module_id, index) # can use 0-index here
      self$use_menu()
    },
    
    # fill in with reactive function
    add_module = NULL, 
    remove_module = NULL,
    use_menu = NULL, 
    
    get_ui = function(){
      print(stringr::str_c("get ui Tile ", private$id))
      
      #private$globals$i18n$use_js()
      # t <- function(text){
      #   private$globals$i18n$t(text)
      #   shiny::span(class = "i18n", `data-key` = text, private$globals$i18n$t(text))
      # }
      
      tagList(
        
        navset_card_tab(
          id = private$id,
          title = private$title,
          full_screen = TRUE,
          # modules go here later
          
          header = uiOutput(stringr::str_c(private$id,"-buttons")),
          
          nav_menu("", value = "_menu_", icon = icon("bars"),
                   nav_item(actionLink(inputId = stringr::str_c(private$id,"-menu-info"), label = private$globals$t("Start intro tour"), icon = icon("info", style = "padding-left: 5px; padding-right: 5px;"))),
                   nav_item(actionLink(inputId = stringr::str_c(private$id,"-menu-title"), label = private$globals$t("Change tile title"), icon = icon("pen-to-square"))),
                   nav_item(actionLink(inputId = stringr::str_c(private$id,"-menu-add"), label = private$globals$t("Add module"), icon = icon("plus", style = "padding-left: 1px; padding-right: 1px;"))),
                   nav_item(shinyjs::hidden(actionLink(inputId = stringr::str_c(private$id,"-menu-maximize"), label = private$globals$t("Full screen"), icon = icon("up-right-and-down-left-from-center")))),
                   nav_item(shinyjs::hidden(actionLink(inputId = stringr::str_c(private$id,"-menu-settings"), label = private$globals$t("Module settings"), icon = icon("cog")))),
                   nav_item(shinyjs::hidden(actionLink(inputId = stringr::str_c(private$id,"-menu-remove-tile"), label = private$globals$t("Remove this tile"), icon = icon("trash-can")))),
                   nav_item(shinyjs::hidden(actionLink(inputId = stringr::str_c(private$id,"-menu-remove-module"), label = private$globals$t("Remove this module"), icon = icon("trash-can"))))
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
      
      # buttons in header
      output[[stringr::str_c(private$id, "-buttons")]] <- renderUI({
        div(class="btn-group btn-group-sm",
          actionButton(inputId = stringr::str_c(private$id,"-header-info"), title=private$globals$text["Start intro tour"], label = "", icon = icon("info", style = "padding-left: 5px; padding-right: 5px;")),
          actionButton(inputId = stringr::str_c(private$id,"-header-title"), title=private$globals$text["Change tile title"], label = "", icon = icon("pen-to-square")),
          actionButton(inputId = stringr::str_c(private$id,"-header-add"), title = private$globals$text["Add module"], label = "", icon = icon("plus", style = "padding-left: 1px; padding-right: 1px;")),
          actionButton(inputId = stringr::str_c(private$id,"-header-maximize"), title = private$globals$text["Full screen"], label = "", icon = icon("up-right-and-down-left-from-center")),
          actionButton(inputId = stringr::str_c(private$id,"-header-settings"), title = private$globals$text["Module settings"], label = "", icon = icon("cog")),
          actionButton(inputId = stringr::str_c(private$id,"-header-remove-tile"), title = private$globals$text["Remove this tile"], label = "", icon = icon("trash-can")),
          actionButton(inputId = stringr::str_c(private$id,"-header-remove-module"), title = private$globals$text["Remove this module"], label = "", icon = icon("trash-can"))
        )
      })
      
      # add extra class to tablist ul element to ensure tabs are floating right also on Edge
      shinyjs::runjs(stringr::str_c("$('#",private$id,"').addClass('justify-content-end')"))
      
      # add tabIndex to tile so we can select the tile by tab and do tile actions directly with keyboard
      shinyjs::runjs(stringr::str_c("$('#",private$id,"').parent().parent().attr('tabindex',0)"))
      
      # insert module UIs
      insert_module <- function(mod_id, mod_idx){
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
      }
      purrr::iwalk(rev(private$modules), insert_module)

      # Action observer: Make full screen
      private$observers$maximize <- observe({
        req(input[[stringr::str_c(private$id,"-menu-maximize")]] + input[[stringr::str_c(private$id,"-header-maximize")]] > 0)
        # we leverage the hidden fullscreen tooltip button (because of fullscreen=TRUE in the navset_card_tab) and just click it programmatically
        shinyjs::runjs(stringr::str_c("$('#", private$id, "').parent().parent().children().eq(2).children().click()"))
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-maximize")]], input[[stringr::str_c(private$id,"-header-maximize")]], ignoreInit = TRUE)
      
      # Action observer: Change title
      private$observers$title <- observe({
        req(input[[stringr::str_c(private$id,"-menu-title")]] + input[[stringr::str_c(private$id,"-header-title")]] > 0)
        print("update title observer")
        edit_title <- ElementalEditTitle$new(id = stringr::str_c(private$id,"-title"), title = "Verander titel", globals = private$globals, ui_element = self)
        edit_title$start_server()
        showModal(modalDialog(edit_title$get_ui(), footer = NULL, easyClose = TRUE))
        private$globals$modal <- edit_title
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-title")]], input[[stringr::str_c(private$id,"-header-title")]], ignoreInit = TRUE)
      
      # Action observer: Add module
      private$observers$add <- observe({
        req(input[[stringr::str_c(private$id,"-menu-add")]] + input[[stringr::str_c(private$id,"-header-add")]] > 0)
        print("add module observer")
        add_module <- ElementalAddModule$new(id = stringr::str_c(private$id,"-title"), title = "Module toevoegen", globals = private$globals, tile = self)
        add_module$start_server()
        showModal(modalDialog(add_module$get_ui(), footer = NULL, easyClose = TRUE))
        private$globals$modal <- add_module
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-add")]], input[[stringr::str_c(private$id,"-header-add")]], ignoreInit = TRUE)
      
      # Action observer: Remove tile
      private$observers$remove <- observe({
        req(input[[stringr::str_c(private$id,"-menu-remove-tile")]] + input[[stringr::str_c(private$id,"-header-remove-tile")]] > 0)
        print("remove tile observer")
        private$parent$remove_tile(private$id)
        shinyjs::runjs(stringr::str_c("$('#", private$id, "').parent().parent().parent().remove()"))
        
        serialize(pages = private$globals$pages)
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-remove-tile")]], input[[stringr::str_c(private$id,"-header-remove-tile")]], ignoreInit = TRUE)

        
      show_settings <- function(){
        # show settings
        print(stringr::str_c(private$id,"-menu-settings", "  ", input[[private$id]]))
        
        private$settings <- ElementalModuleSettings$new(id = stringr::str_c(private$id,"-settings"), title = "Instellingen", globals = private$globals, tile = self, module = private$globals$modules[[input[[private$id]]]])
        private$settings$start_server()
        showModal(modalDialog(private$settings$get_ui(), footer = NULL, easyClose = TRUE))
        private$globals$modal <- private$settings
      }
      # Action observer: Show settings dialog    
      private$observers$settings <- observe({
        req(input[[stringr::str_c(private$id,"-menu-settings")]] + input[[stringr::str_c(private$id,"-header-settings")]] > 0)
        show_settings()
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-settings")]], input[[stringr::str_c(private$id,"-header-settings")]], ignoreInit = TRUE)
      
      # Action observer: Start info tour
      private$observers$info <- observe({
        req(input[[stringr::str_c(private$id,"-menu-info")]] + input[[stringr::str_c(private$id,"-header-info")]] > 0)
        # start intro tour
        print(stringr::str_c(private$id,"-menu-info", "  ", input[[private$id]]))
        mod <- private$globals$modules[[input[[private$id]]]]
        tour <- mod$get_intro_tour()
        if (!is.null(tour) && length(tour) > 0){
          rintrojs::introjs(
            session,
            options = list(
              steps = tour,
              showBullets = FALSE,
              showProgress = TRUE,
              nextLabel = private$globals$i18n$t("Next"),
              prevLabel = private$globals$i18n$t("Previous"),
              doneLabel = private$globals$i18n$t("Close"),
              # tooltipClass is needed here to override the default styling that hides .introjs-tooltipReferenceLayer.
              # The tooltipReferenceLayer is hidden because of the switch_tab steps but those are not used here.
              tooltipClass = "page-settings-tour",
              positionPrecedence = c("right", "left", "bottom", "top"),
              scrollToElement = TRUE,
              scrollTo = "tooltip"
            ),
            events = list()
          )
        }
        
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-info")]], input[[stringr::str_c(private$id,"-header-info")]], ignoreInit = TRUE)
      
      # Action observer: remove module
      private$observers$remove_module <- observe({
        req(input[[stringr::str_c(private$id,"-menu-remove-module")]] + input[[stringr::str_c(private$id,"-header-remove-module")]] > 0)
        mod_id <- input[[private$id]]
        mod <- private$globals$modules[[mod_id]]
        
        # remove from tile list
        private$modules[[which(mod_id == private$modules)]] <- NULL
        # remove from global list
        private$globals$modules[[mod_id]] <- NULL
        
        
        # remove from UI
        nav_remove(private$id, mod_id)
        # kill the object
        mod$remove()
        
        # update menu if tile now contains no modules
        
        if (length(private$modules) == 0){
          self$use_menu(private$globals$preferences$tile_menu)
        }
      }) %>% bindEvent(input[[stringr::str_c(private$id,"-menu-remove-module")]], input[[stringr::str_c(private$id,"-header-remove-module")]], ignoreInit = TRUE)
      
      # Full screen observer
      private$observers$fullscreen <- observe({
        print(stringr::str_c("Full screen: ",input[[stringr::str_c(private$id, "_full_screen")]]))
        private$globals$modules[[input[[private$id]]]]$set_fullscreen(input[[stringr::str_c(private$id, "_full_screen")]])
      }) %>% bindEvent(input[[stringr::str_c(private$id, "_full_screen")]], ignoreInit = TRUE)
      
      # Tab observer
      private$observers$select_tab <- observe({
        if (!is.null(private$settings)){
          private$settings$update_module_selection(private$globals$modules[[input[[private$id]]]])
        }
      }) %>% bindEvent(input[[private$id]])
      
      # toggle between having the tile actions in a menu or as separate icons in the tile header
      self$use_menu = function(tile_menu = TRUE){
        
        if (tile_menu){
          # show menu
          nav_show(private$id, "_menu_", session = session)
          # hide all header buttons
          #shinyjs::runjs(stringr::str_c("$('#", private$id, " > .bslib-nav-item a').hide()"))
          shinyjs::runjs(stringr::str_c("$('#", private$id, "-buttons').parent().hide()"))
          
          # show/hide menu options depending on if there are modules shown in this tile
          if (length(private$modules) > 0){
            shinyjs::show(id = stringr::str_c(private$id,"-menu-settings"))
            shinyjs::show(id = stringr::str_c(private$id,"-menu-maximize"))
            shinyjs::show(id = stringr::str_c(private$id,"-menu-remove-module"))
            shinyjs::show(id = stringr::str_c(private$id,"-menu-info"))
            
            shinyjs::hide(id = stringr::str_c(private$id,"-menu-remove-tile"))
          } else {
            shinyjs::show(id = stringr::str_c(private$id,"-menu-remove-tile"))
            
            shinyjs::hide(id = stringr::str_c(private$id,"-menu-info"))
            shinyjs::hide(id = stringr::str_c(private$id,"-menu-settings"))
            shinyjs::hide(id = stringr::str_c(private$id,"-menu-maximize"))
            shinyjs::hide(id = stringr::str_c(private$id,"-menu-remove-module"))
          }
        } else {
          # hide the menu
          nav_hide(private$id, "_menu_", session = session)
          # show the default header buttons div
          #shinyjs::runjs(stringr::str_c("$('#", private$id, " > .bslib-nav-item a').show()"))
          shinyjs::runjs(stringr::str_c("$('#", private$id, "-buttons').parent().show()"))
          
          shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-title').show()"))
          shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-add').show()"))
          
          # show/hide more header butons depending on if there are modules shown in this tile
          if (length(private$modules) > 0){
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-settings').show()"))
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-maximize').show()"))
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-remove-module').show()"))
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-info').show()"))
            
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-remove-tile').hide()"))
            
          } else {
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-settings').hide()"))
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-maximize').hide()"))
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-remove-module').hide()"))
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-info').hide()"))
            
            shinyjs::runjs(stringr::str_c("$('#", private$id, "-header-remove-tile').show()"))
            
          }
        }
      }
      self$use_menu(private$globals$preferences$tile_menu)
      
      # add a new module
      self$add_module = function(module){
        # add module id to list of modules in this tile
        private$modules <- append(private$modules, module$get_id(), after = 0)
        # reuse code to add the modules at startup: insert module in the UI
        insert_module(module$get_id(), length(private$modules))
        # trigger the settings menu if this module has inputs to set
        if (length(module$get_inputs) > 0){
          show_settings()
        }
        if (length(private$modules) == 1){
          # tile was empty before, update tile menu to reflect this
          self$use_menu(private$globals$preferences$tile_menu)
        }
      }
      
      # remove a module from tile (and dashboard)
      self$remove_module = function(module_id){
        
      }
    },
    
    serialize = function(){
      list(class = class(self)[1], title = private$title, modules = setNames(private$modules, NULL))
    }
  )
)
  
