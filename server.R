#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(lme4)
library(magrittr)
library(stringr)
library(readr)
library(bprodradata) # devtool::install_github('bdilday/bprodradata')

print("hey ho, let's go")
dra_runs <- list()
pit_ranef <- list()
model_ranef <- list()
print(.libPaths()[[1]])
print(list.files(.libPaths()[[1]]))

for (year in c(2007, 2016)) {
  key <- as.character(year)
  cat(sprintf("loading data for year: %d\n", year))
  tmp <- bprodradata::load_dra_runs(year)
  dra_runs[[key]] <- tmp$dra_runs
  pit_ranef[[key]] <- tmp$pit_ranef
  model_ranef[[key]] <- tmp$model_ranef
}

print("done")

id_from_key <- function(key) {
  x <- str_split(key, " ")[[1]]
  nl <- length(x)
  x[[nl]]
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$selectUI<-renderUI({
    pit_id_list <- unique(pit_ranef[[input$year]]$var_id)

    df1 <- data_frame(pit_id=pit_id_list)
    pit_key <- df1 %>%
      merge(Lahman::Master %>%
              mutate(FullName=paste(nameFirst, nameLast, sep=" ")),
            by.x="pit_id", by.y="retroID") %>%
      select(pit_id, FullName) %>%
      mutate(pit_key = paste(FullName, pit_id, sep=" ")) %$% pit_key
    selectInput("pit_key", "select pit_id", pit_key)
  })

  output$distPlot <- renderPlot({
    fit_var <- "value"
    year = input$year

    fit_df <- pit_ranef[[year]]
    fit_df$fit_var <- fit_df[[fit_var]]

    pit_df <- fit_df %>% filter(var_id == id_from_key(input$pit_key))

    fit_df %>%
      ggplot(aes(x=model_name, y=fit_var, group=model_name)) +
      geom_boxplot() +
      theme_minimal() +
      coord_flip() +
      labs(title=sprintf("DRA Components: %s", year),
           y=sprintf("%s",fit_var)) +
      geom_point(data=pit_df,
                   aes(x=model_name, y=fit_var, group=model_name), color='red', size=4) +
      theme(axis.text = element_text(face='bold', size=14),
            axis.title = element_text(size=22))
  })

  output$pitcher_components <- renderTable({
    tmp <- dra_runs[[input$year]]
    tmp <- subset(tmp, pit_id == id_from_key(input$pit_key))
    tmp
  })

  output$model_ranef <- renderTable({
    tmp <- subset(model_ranef[[input$year]], pit_id == id_from_key(input$pit_key))
    tmp$mean_value <- tmp$mean_value * 100
    names(tmp) <- c("pit_id", "ranef_name", "mean_valuex100", "model_name")
    tmp
  })

})
