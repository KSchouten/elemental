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

serialize <- function(modules = NULL, pages = NULL, state = NULL, preferences = NULL){
  params <- list()
  if (!is.null(modules)) {
    params$modules <- purrr::map(modules, ~.$serialize())
    params$modules %>% toJSON() %>% jsonlite::prettify() %>% print()
  }  
  if (!is.null(pages)) {
    params$pages <- purrr::map(pages, ~.$serialize()) %>% setNames(NULL)
    params$pages %>% toJSON %>% jsonlite::prettify() %>% print()
  }
  if (!is.null(state)) {
    params$state <- purrr::map(state, ~.$get_state())
    params$state %>% toJSON() %>% jsonlite::prettify() %>% print()
  }
  if (!is.null(preferences)) {
    params$preferences <- preferences %>% toJSON() %>% jsonlite::prettify() %>% print()
  }
  
  # serialize queryString
  # new_query <- purrr::imap_chr(params, function(value, name){
  #   stringr::str_c(name, "=", value)
  # }) %>% stringr::str_c(collapse = "&")
  
  if (!is.null(modules) && !is.null(pages) && !is.null(state)){
    new_query <- toJSON(params) %>% openssl::base64_encode()
    #new_query <- params %>% base::serialize(NULL) %>% openssl::base64_encode()
    return(stringr::str_c("?config=", new_query))
  } else {
    return(NULL)
  }
}

create_theme <- function(theme = "shiny"){
  bslib::bs_theme(preset = theme) %>%
    bslib::bs_add_variables(
      #"spacer" = "0.5rem",         # make site more compact
      "bslib-spacer" = "0.5rem",    # make column gaps smaller
      "grid-breakpoints" = "(
        xs: 0,
        sm: 768px,
        md: 768px, 
        lg: 992px,
        xl: 1200px,
        xxl: 1400px)", # 0, 576, 768, 992, 1200, 1400
      .where = "declarations"
    ) 
}


