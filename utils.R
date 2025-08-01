# Convenience wrapper for toJSON with automatic auto_unbox = TRUE
toJSON <- function(x){jsonlite::toJSON(x, auto_unbox = TRUE)}
# Convenience wrapper for fromJSON with simplify = FALSE and test for NA
fromJSON <- function(x){if (is.na(x)){list()}else{jsonlite::fromJSON(x, simplifyVector = FALSE)}}
# Convenience function to conditionally wrap a tag in shinyjs::disabled
disable <- function(tag, condition){if (condition) shinyjs::disabled(tag) else tag}
