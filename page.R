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
      
      nav_panel(id = private$id, value = private$id, title = private$title, icon = icon(private$icon), 
                purrr::imap(private$layout, function(row, row_idx){
                  row_id <- ns(stringr::str_c("row", row_idx, sep = "-"))
                  elemental_row(
                    id = row_id, columns = row$column_sizes,
                    !!!purrr::imap(row$columns, function(col, col_idx){
                      col_id <- ns(stringr::str_c("col", row_idx, col_idx, sep = "-"))
                      elemental_column(
                        id = col_id,
                        !!!purrr::imap(col$tiles, function(tile, tile_idx){
                          #tile_id = stringr::str_c("tile-", as.numeric(lubridate::now()), tile_idx)
                          tile_id = ns(stringr::str_c("tile", row_idx, col_idx, tile_idx, sep = "-"))
                          elemental_tile(
                            id = tile_id,
                            title = tile$title
                          )
                        })
                      )
                    })
                  )
                })
                #uiOutput(ns("page"))
      )
  
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
        
        # manipulate UI after rendering, do the UI things that we need a reactive context for
        # - initialize splitJS
        # - add column background controls
        # - add modules to tiles
        observe({
          print(stringr::str_c("complete UI for ", private$id))

          # intialize splitjs on each row
          purrr::iwalk(private$layout, function(row, row_idx){
            row_id <- ns(stringr::str_c("row", row_idx, sep = "-"))
            col_sizes <- row$column_sizes %>% stringr::str_c(collapse = ", ")
            shinyjs::runjs(stringr::str_c("Split($('#",row_id, " > div'), {sizes: [",col_sizes,"], minSize: 250})"))
            
            #add background controls for each column
            purrr::iwalk(row$columns, function(col, col_idx){
              col_id <- ns(stringr::str_c("col", row_idx, col_idx, sep = "-"))
              insert_background_controls(col_id)
              
              # insert the module UI's as they are only loaded when the page is first visited
              
              purrr::iwalk(col$tiles, function(tile, tile_idx){
                tile_id = stringr::str_c("tile", row_idx, col_idx, tile_idx, sep = "-")
                purrr::iwalk(rev(tile$modules), function(mod_id, mod_idx){
                  mod <- private$modules[[mod_id]]
                  
                  # easiest thing is to insert in reverse order so we can always add the newest tab at the front
                  #   if we use "after" they appear after the buttons as well which is not what we want
                  nav_insert(tile_id,
                             nav_panel(
                               id = mod$get_full_id(),
                               title = mod$get_title(),
                               value = mod$get_id(),
                               mod$get_ui()
                             ), position = "before", select = (mod_idx == length(tile$modules))
                  )
                  # tabcontent divs are always inserted at the end, regardless of the "before" setting of nav_insert
                  # this javascript swaps the order of the content div so it matches with the tab order
                  if (mod_idx > 1){
                    shinyjs::runjs(stringr::str_c("setTimeout(function(){
                      $('#", ns(tile_id), "').parent().parent().children().eq(1).children().eq(",0,").before($('#", ns(tile_id), "').parent().parent().children().eq(1).children().eq(",mod_idx-1,"));
                    }, 100);"))
                  }
                })
              })
            })
          })

        })
        
        return(NULL)
      })
    }
  )
)

