Page <- R6::R6Class(
  "Page", 
  
  private = list(
    id = NA_character_,
    title = NA_character_,
    icon = NA_character_,
    
    globals = list(),
    modules = list(),
    layout = list(),
    
    active = FALSE
  ),
  
  public = list(
    initialize = function(id, title, icon, globals, modules, layout){
      private$id <- id
      private$title <- title
      private$icon <- icon
      private$globals <- globals
      private$modules <- modules
      private$layout <- layout
    },
    
    get_id = function(){
      return(private$id)
    },
    get_title = function(){
      return(private$title)
    },
    get_active = function(){
      return(private$active)
    },
    get_module = function(id){
      return(private$modules[[id]])
    },
    
    get_ui = function(){
      ns <- NS(private$id)
      
      nav_panel(id = private$id, value = private$id, title = private$title, uiOutput(ns("page")))
  
    },
    
    get_menu_item = function(){
      shinydashboard::menuItem(private$title, tabName = private$id, icon = icon(private$icon))
    },
    
    start_server = function(){
      private$active <- TRUE
      moduleServer(private$id, function(input, output, session){
        ns <- session$ns
        
        print(stringr::str_c("start server function for ", private$id))
        
        # Initialize module objects
        purrr::walk(names(private$modules), function(id){
          private$modules[[id]] <- private$modules[[id]]$class$new(id, ns(id), private$modules[[id]]$title, private$globals, purrr::map(private$modules[[id]]$imports, unlist))
        })
        # Start server functions of modules
        purrr::walk(names(private$modules), ~private$modules[[.]]$start_server())
        

        output$page <- renderUI({
          div(
            id = ns("page_container"),
            class = "page-container",
            !!!purrr::imap(private$layout, function(row, row_idx){
              row_id <- ns(stringr::str_c("row", row_idx, sep = "-"))
              layout_column_wrap(
                heights_equal = "row",
                style = css(grid_template_columns = row[[1]]),
                !!!purrr::imap(row[-1], function(col, col_idx){
                  col_id <- ns(stringr::str_c("col", row_idx, col_idx, sep = "-"))
                
                  layout_column_wrap(
                    width = 1,
                    heights_equal = "row",
                    !!!purrr::imap(col, function(tile, tile_idx){
                      tile_id <- ns(stringr::str_c("tile", row_idx, col_idx, tile_idx, sep = "-"))
                      mods <- unlist(tile)
                      
                      navset_card_tab(
                        full_screen = TRUE,
                        title = mods[[1]],
                        #height = "800px",
                        id = tile_id,
                        !!!purrr::map(mods[-1], function(mod_id){
                          mod <- private$modules[[mod_id]]
                          nav_panel(
                            id = mod$get_full_id(),
                            title = mod$get_title(),
                            value = mod$get_id(),
                            
                            #card_title("card title"),
                            mod$get_ui()
                          )
                          
                          
                        }),
                        nav_item(actionLink(inputId = "btn1", label = "", icon = icon("cog"))),
                        nav_item(actionLink(inputId = "btn1", label = "", icon = icon("info")))
                      )
                    })
                  )
                })
              )
            })
          )
            
        })

        return(NULL)
      })
    }
  )
)

