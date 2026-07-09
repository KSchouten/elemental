#' The Translator interface class that any i18n module should subclass in order to work with the Elemental framework
#' 
#' @export
ElementalTranslator <- R6::R6Class(
  "ElementalTranslator", 
  
  private = list(
    
  ),
  
  public = list(
    
    #' @description
    #' Initialize the Translator object
    #' 
    #' Always create a subclass of ElementalTranslator that implements the necessary functions
    initialize = function(){
      stop("Don't use this class directly, always create a subclass")      
    },
    
    #' @description
    #' Implement an init_js function that has to be called in the main ui to initialize the UI bindings.
    #' This is necessary to enable ui_t functionality
    init_js = function(){
      stop("Don't use this class directly, always create a subclass")
    },
    
    #' @description
    #' Set the translation language
    #' 
    #' Should be a language for which a translation exists
    #'
    #' @param language The language code, as supported by the translation file
    set_language = function(language){
      stop("Don't use this class directly, always create a subclass")
    },
    
    #' @description
    #' Get a list of all available languages
    #'
    #' @returns A list of all available languages
    get_all_languages = function(){
      stop("Don't use this class directly, always create a subclass")
    },
    
    #' @description
    #' Get the currently selected target language
    #'
    #' @returns The currently selected target language
    get_current_language = function(){
      stop("Don't use this class directly, always create a subclass")
    },
    
    #' @description
    #' Static translate function
    #' 
    #' Translates a given text to the chosen target language.
    #' This does not react to changes in target language.
    #' Useful for modals etc.
    #'
    #' @param text The text to translate
    #'
    #' @returns The translated text as a character vector
    static_t = function(text){
      stop("Don't use this class directly, always create a subclass")
    },
    
    #' @description
    #' UI translate function
    #' 
    #' Translates a given text into a span element that will automatically react to changes in the target language
    #' Can be used wherever span/html elements are allowed. Cannot be used as an attribute of an element.
    #'
    #' @param text The text to translate
    #'
    #' @returns A span element with attributes so that it can be updated automatically
    ui_t = function(text){
      stop("Don't use this class directly, always create a subclass")
    },
    
    #' @description
    #' Reactive translate function
    #' 
    #' When a span is not allowed but a static translation is not possible, you can use this reactive translation.
    #' It returns a reactive value with the translation, and it will automatically update when the target language changes.
    #' However, you can only use this inside a server function, not directly in the ui. This means you will need uiOutput() and renderUI() to work with this.
    #' It also means the whole renderUI block will be re-run when the language changes. Be careful of expensive computations.
    #'
    #' @param text The text to translate
    #'
    #' @returns A reactive value with the translation
    reactive_t = function(text){
      stop("Don't use this class directly, always create a subclass")
    }
    
  )
)

#' The default implementation of the Translator class that implements the shiny.i18n package 
ElementalTranslatorI18n <- R6::R6Class(
  "ElementalTranslatorI18n", 
  inherit = ElementalTranslator,
  
  private = list(
    i18n = NULL,
    reactive_texts = NULL
  ),
  
  public = list(

    initialize = function(initial_target_language){
      private$i18n <- shiny.i18n::Translator$new(translation_json_path = app_sys("app/translation.json"))
      
      self$set_language(initial_target_language)
    },
    
    init_js = function(){
      shiny.i18n::usei18n(private$i18n)
    },
    
    set_language = function(language){
      # this will update all ui_t translation
      if (!is.null(shiny::getDefaultReactiveDomain())){
        shiny.i18n::update_lang(language)
        # this will update all reactive_t translations, render_ blocks using them will re-render with the new translation(s)
        private$reactive_texts(fromJSON(app_sys("app/translation.json"))$translation %>% purrr::map(function(x){list(x[[language]]) %>% setNames(x[[1]])}) %>% unlist(recursive = FALSE))
      } else {
        # this will update all reactive_t translations, render_ blocks using them will re-render with the new translation(s)
        private$reactive_texts <- reactiveVal(fromJSON(app_sys("app/translation.json"))$translation %>% purrr::map(function(x){list(x[[language]]) %>% setNames(x[[1]])}) %>% unlist(recursive = FALSE))
      }
      # this will ensure new static_t translations are in the new language
      private$i18n$set_translation_language(language) 
      
    },
    
    get_all_languages = function(){
      private$i18n$get_languages()
    },
    
    get_current_language = function(){
      private$i18n$get_translation_language()
    },
    
    static_t = function(text){
      # This function can choose whether to use a browser-based dynamic translation or not
      # It will choose the dynamic one only when there is no active shiny context, which in this framework 
      #   is only the case in the main ui function of the app itself, but not for any of the modules.
      # This, this function is for practically all intenst and purposes a static translation
      private$i18n$t(text)
    },
    
    ui_t = function(text){
      # The built-in function doesn't always trigger the dynamic translation, so this explicitly makes it a dynamic translation
      # Code comes from the shiny.i18n package: R/translator.R
      shiny::span(class = "i18n", `data-key` = text, self$static_t(text))
    },
    
    reactive_t = function(text){
      # This just gives the translation from a pre-populated reactive value
      private$reactive_texts()[text]
    }
  )
)
