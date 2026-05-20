# Convenience wrapper for toJSON with automatic auto_unbox = TRUE
toJSON <- function(x){jsonlite::toJSON(x, auto_unbox = TRUE)}
# Convenience wrapper for fromJSON with simplify = FALSE and test for NA
fromJSON <- function(x){if (is.na(x)){list()}else{jsonlite::fromJSON(x, simplifyVector = FALSE)}}
# Convenience function to conditionally wrap a tag in shinyjs::disabled
disable <- function(tag, condition){if (condition) shinyjs::disabled(tag) else tag}

get_class <- function(classname){
  class_obj <- get(classname)
  if (class_obj$get_inherit()$classname == "Module"){
    return(class_obj)
  } else {
    return(NULL)
  }
}

serialize <- function(modules, pages){
  
  toJSON(list(
    modules = purrr::map(modules, ~.$serialize()),
    pages = purrr::map(pages, ~.$serialize()) %>% setNames(NULL)
  )) %>% jsonlite::prettify() %>% print()
  
}