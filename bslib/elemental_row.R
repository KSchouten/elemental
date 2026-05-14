elemental_row <- function(...,
                          id,
                          group = "elemental_row",
                          width = "200px",
                          fixed_width = FALSE,
                          heights_equal = "row",
                          fill = TRUE,
                          fillable = TRUE,
                          height = NULL,
                          height_mobile = NULL,
                          min_height = NULL,
                          max_height = NULL,
                          gap = NULL,
                          class = "layout layout-row",
                          columns = rep(1, length(list(...))),
                          style = css(grid_template_columns = stringr::str_c(columns, "fr", collapse = " 10px "),
                                      padding = "10px",
                                      margin = "-10px"),
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
    )
    
  )
}
