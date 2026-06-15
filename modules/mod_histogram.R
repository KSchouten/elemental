Histogram <- R6::R6Class(
  "Histogram", 
  inherit = Module,
  
  private = list(
    

    imports = list("number_bins"),
    exports = list(),
    params = list(),
    
    
    # Override this for module-specific UI
    ui = function(){
      ns <- NS(private$id)
      tagList(
        textOutput(ns("nrbins")),
        plotly::plotlyOutput(ns("distPlot"))
      )
    },
    
    server = function(input, output, session, module_inputs, module_outputs){
      ns <- session$ns
      
      output$nrbins <- renderText({
        module_inputs$number_bins
      })
      
      output$distPlot <- plotly::renderPlotly({
        req(module_inputs$number_bins)
        # generate bins based on module_inputs$number_bins from other module
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = module_inputs$number_bins + 1)

        # draw the histogram with the specified number of bins
        plotly::plot_ly() %>%
          plotly::add_histogram(x = ~x, name = "Variable 1", nbinsx = module_inputs$number_bins + 1, opacity = 1) %>%
          plotly::layout(title = "Histogram of waiting times",
                         xaxis = list(title = "Waiting time to next eruption (in mins)"),
                         yaxis = list(title = "Frequency"))
        
    })
    }
  ),
  
  public = list(
    
    get_intro_tour = function(){
      ns <- NS(private$id)
      ns_id <- function(x){stringr::str_c("#", ns(x))}

      list(list(element = ns_id("distPlot"), title = "Grafiek", intro = "Dit is een grafiek, yeah!"),
           list(element = ns_id("nrbins"), title = "Grafiek", intro = "Dit is een getal"))
    }
    
  )
)
#defaults
Histogram$name = "Histogram"
Histogram$page = NA_character_
Histogram$group = NA_character_
Histogram$singleton = FALSE
Histogram$category <- "grafiek"