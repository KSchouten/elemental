Histogram <- R6::R6Class(
  "Histogram", 
  inherit = Module,
  
  private = list(
    
    default_name = "histogram",
    default_page = NA_character_,
    imports = list("number_bins"),
    exports = list(),
    params = list(),
    group = NA_character_,
    singleton = FALSE,
    
    # Override this for module-specific UI
    ui = function(){
      ns <- NS(private$full_id)
      plotOutput(ns("distPlot"))
    },
    
    server = function(input, output, session, module_inputs, module_outputs){
      ns <- session$ns
      
      output$distPlot <- renderPlot({
        req(module_inputs$number_bins)
        # generate bins based on module_inputs$number_bins from other module
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = module_inputs$number_bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    }
  ),
  
  public = list(
    
    
    
  )
)