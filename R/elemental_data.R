#' The Data interface class that any database connector module should subclass in order to work with the Elemental framework
#' 
#' Subclass it use a specific database backend for your dashboard
#' 
#' @export
ElementalData <- R6::R6Class(
  "ElementalData", 
  
  private = list(
    
  ),
  
  public = list(
    
    initialize = function(){
      
    }
    
    
    
  )
)

#' A basic SQLite-based implementation of the Data interface class
#' 
#' Use it for development but not for production purposes. SQLite is file-based so it will not properly work with multiple concurrent users
ElementalDataSqlite <- R6::R6Class(
  "ElementalDataSqlite",
  inherit = ElementalData,
  
  private = list(
    conn = NULL
  ),
  
  public = list(
    
    initialize = function(){
      private$conn <- DBI::dbConnect(RSQLite::SQLite(), "data.sqlite")
    }
    
    
    
  )
)