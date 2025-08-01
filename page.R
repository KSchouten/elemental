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
      
      shinydashboard::tabItem(
        private$id, 
        uiOutput(ns("page"))
      )
  
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
          private$modules[[id]] <- private$modules[[id]]$class$new(id, ns(id), private$globals, purrr::map(private$modules[[id]]$imports, unlist))
        })
        # Start server functions of modules
        purrr::walk(names(private$modules), ~private$modules[[.]]$start_server())
        

        output$page <- renderUI({
          div(
            id = ns("page_container"),
            class = "page-container",
            if (is.null(private$globals$user)){
              tagList()
            } else {
              sortable::sortable_js(ns("page_container"), options = sortable::sortable_options(
                group = ns("page"), handle = ".row-layout-control", filter = ".page-layout-control",
                onEnd =  htmlwidgets::JS(stringr::str_c(
                  "function(evt){Shiny.setInputValue('", ns("layout_change") ,"', get_layout('", ns("page_container") ,"'))}"
                )),
                onMove = htmlwidgets::JS(stringr::str_c(
                  "function(evt){return evt.related.className !== 'page-layout-control'}"
                ))
              ))
            },
            !!!purrr::imap(private$layout, function(row, row_idx){
              row_id <- ns(stringr::str_c("row", row_idx, sep = "-"))
              shiny::fluidRow(
                id = row_id,
                class = "page-layout-row layout layout-invisible",
                if (is.null(private$globals$user)){
                  tagList()
                } else {
                  sortable::sortable_js(row_id,
                                        options = sortable::sortable_options(
                                          group = ns("rows"), draggable = ".page-layout-column", handle = ".col-layout-control",
                                          onEnd =  htmlwidgets::JS(stringr::str_c(
                                            "function(evt){Shiny.setInputValue('", ns("layout_change") ,"', get_layout('", ns("page_container") ,"'))}"
                                          )),
                                          onMove = htmlwidgets::JS(stringr::str_c(
                                            "function(evt){return evt.related.className !== 'row-layout-control'}"
                                          ))
                                        )
                  )
                },
                # row button: add column
                div(class = "row-layout-control layout layout-invisible",
                    
                      actionButton(ns(stringr::str_c("add", "col", row_idx, sep = "-")),
                                   label = div(class = "fa-stack icon-stack-plus",
                                               icon("table-columns", class = "fa-stack-2x"),
                                               icon("plus", class = "fa-stack-1x")),
                                   class = "col-sizer",
                                   title = "Voeg een kolom toe aan de layout", style="margin-left: calc(50% - 38px);",
                                   onclick = htmlwidgets::JS(stringr::str_c("Shiny.setInputValue('", ns("add_column") ,"', {row_position: $('#", row_id, "').index()-1, row_idx: '", row_idx ,"'}, {priority: 'event'})"))
                      ),
                    
                    
                      actionButton(ns(stringr::str_c("remove", "row", row_idx, sep = "-")),
                                   "", icon = icon("trash-can"), class = "col-sizer",
                                   title = "Verwijder deze rij",
                                   onclick = htmlwidgets::JS(stringr::str_c("remove_ui_element('", row_id ,"', '",ns("layout_change"),"', '",ns("page_container"),"')"))
                      )
                    
                ),
                # columns
                !!!purrr::imap(row, function(col, col_idx){
                  col_id <- ns(stringr::str_c("col", row_idx, col_idx, sep = "-"))
                  
                  shiny::column(
                    as.integer(col[[1]]), # size is the first element in the column layout
                    id = col_id,
                    class = "page-layout-column layout layout-invisible",
                    size = col[[1]],
                    if (is.null(private$globals$user)){
                      tagList()
                    } else {
                      sortable::sortable_js(col_id,
                                            options = sortable::sortable_options(
                                              group = ns("cols"), handle = ".box-header", draggable = ">div",
                                              onEnd =  htmlwidgets::JS(stringr::str_c(
                                                "function(evt){Shiny.setInputValue('", ns("layout_change") ,"', get_layout('", ns("page_container") ,"'))}"
                                              )),
                                              onMove = htmlwidgets::JS(stringr::str_c(
                                                "function(evt){return evt.related.className !== 'col-layout-control'}"
                                              ))
                                            )
                      )
                    },
                    # column width buttons
                    span(class = "col-layout-control layout layout-invisible", style="display: block;",
                         
                           actionButton(ns(stringr::str_c("col", row_idx, col_idx, "smaller", sep = "-")),
                                        "", icon = icon("down-left-and-up-right-to-center"), class = "col-sizer",
                                        title = "Maak kolom smaller", style="margin-left: calc(50% - 82px);",
                                        onclick = htmlwidgets::JS(stringr::str_c("update_column_size('", col_id  ,"', -1, '",ns("layout_change"),"', '",ns("page_container"),"')")))
                          %>% disable(col[[1]] == 3),
                         
                           actionButton(ns(stringr::str_c("col", row_idx, col_idx, "larger", sep = "-")),
                                        "", icon = icon("up-right-and-down-left-from-center"), class = "col-sizer",
                                        title = "Maak kolom breder",
                                        onclick = htmlwidgets::JS(stringr::str_c("update_column_size('", col_id  ,"', 1, '",ns("layout_change"),"', '",ns("page_container"),"')")))
                          %>% disable(col[[1]] == 12),
                         
                           actionButton(ns(stringr::str_c("add", "tile", row_idx, col_idx, sep = "-")),
                                        "", icon = icon("square-plus"), class = "col-sizer",
                                        title = "Voeg een tegel toe aan deze kolom",
                                        onclick = htmlwidgets::JS(stringr::str_c("Shiny.setInputValue('", ns("add_tile") ,"', {col_id: '", col_id ,"', row_position: $('#", row_id,"').index()-1, col_position: $('#", col_id,"').index()-2}, {priority: 'event'})"))
                           )
                         ,
                         
                           actionButton(ns(stringr::str_c("remove", "col", row_idx, col_idx, sep = "-")),
                                        "", icon = icon("trash-can"), class = "col-sizer",
                                        title = "Verwijder deze kolom",
                                        onclick = htmlwidgets::JS(stringr::str_c("remove_ui_element('", col_id  ,"', '",ns("layout_change"),"', '",ns("page_container"),"')")))
                         
                    ),
                    div(),
                    # modules (first element is column size)
                    !!!purrr::map(unlist(col[-1]), ~private$modules[[.]]$get_ui())
                    
                  )
                })
              )
            }),
            # page button: add row
            div(class = "page-layout-control layout layout-invisible",
                id = ns("page-layout-control"),
                
                  actionButton(ns("add-row"),
                               label = div(class = "fa-stack icon-stack-plus",
                                           icon(name = NULL, class = "table-rows-icon"),
                                           icon("plus", class = "fa-stack-1x")), class = "col-sizer",
                               title = "Voeg een rij toe aan de layout", style="margin-left: calc(50% - 19px); margin-top: 10px",
                               onclick = htmlwidgets::JS(stringr::str_c("Shiny.setInputValue('", ns("add_row") ,"', 0, {priority: 'event'})"))
                  )
                
            ),
            
          )
          # div(
          #   !!!purrr::map(names(private$modules), ~private$modules[[.]]$get_ui())
          # )
        })
        
        
        
        return(NULL)
      })
    }
  )
)

