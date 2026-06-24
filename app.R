#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#install.packages(c("sortable"))

#library(shiny)
library(dplyr)
#library(bslib)
#source("utils.R")
#source("module.R")
list.files("modules", full.names = TRUE) %>% purrr::walk(source)
#list.files("elements", full.names = TRUE) %>% purrr::walk(source)

modules <- purrr::map(ls(.GlobalEnv), get) %>% purrr::keep(~all(class(.)=="R6ClassGenerator")) %>% purrr::keep(~!is.null(.$inherit) && .$inherit == "Module")

config <- jsonlite::read_json("pages.json")

devtools::document()
devtools::load_all()

app = App$new(modules, config)

runApp(app$run(), launch.browser = TRUE)
