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
library(BProDRA)


print("hey ho, let's go")
mods_list <- list()
pitcher_ranef_list <- list()
dra_events_list <- list()

for (year in c(2007, 2016)) {
  key <- as.character(year)
  cat(sprintf("loading data for year: %d\n", year))
  dra_events_list[[key]] <- BProDRA::load_events_data(year)
  mods_list[[key]] <- BProDRA::load_fitted_dra_models(year)
  #   pitcher_ranef_list[[key]] <- data.frame(x=1:10)
  pitcher_ranef_list[[key]] <- BProDRA::extract_pitcher_ranef(mods_list[[key]])
}

print("done")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$selectUI<-renderUI({
    pit_id_list <- unique(pitcher_ranef_list[[input$year]]$var_id)
    selectInput("pit_id", "select pit_id", pit_id_list)
  })

  output$pit_list_label <- renderText({
    pit_id_list <- unique(pitcher_ranef_list[[input$year]]$var_id)

    tmp <- ""
    for (i in seq_along(names(pitcher_ranef_list))) {
      v <- names(pitcher_ranef_list)[[i]]
      tmp <- paste(v, tmp, sep=" ")
    }

    # sprintf("year: %s nrow: %d %d %s ",
    #         input$year,
    #         length(pit_id_list),
    #         nrow(pitcher_ranef_list[[input$year]]), tmp
    #         )
    #
    sprintf("year: %s nrow: %d tmp %s ",
            input$year,
            length(pit_id_list), tmp
    )

  })

  output$distPlot <- renderPlot({
    fit_var <- "value"
    year = input$year
    if (! year %in% names(pitcher_ranef_list)) {
      pitcher_ranef_list[[year]] <- BProDRA::extract_pitcher_ranef(mods_list[[year]])
    }

    fit_df <- pitcher_ranef_list[[year]]
    fit_df$p <- BProDRA::logit_fun(fit_df$value)
    fit_df$fit_var <- fit_df[[fit_var]]

    pit_df <- fit_df %>% filter(var_id == input$pit_id)

    fit_df %>%
      ggplot(aes(x=model_name, y=fit_var, group=model_name)) +
      geom_boxplot() +
      theme_minimal() +
      coord_flip() +
      labs(title=sprintf("DRA Components: %s", year),
           y=sprintf("%s",input$fit_var)) +
      geom_point(data=pit_df,
                   aes(x=model_name, y=fit_var, group=model_name), color='red', size=4)

  })

  output$model_label <- renderText({
    year = input$year

    if (! year %in% names(pitcher_ranef_list)) {
      pitcher_ranef_list[[year]] <- BProDRA::extract_pitcher_ranef(mods_list[[year]])
    }

    fit_df <- pitcher_ranef_list[[year]]
 #   sprintf("year: %s month; ", year)
    sprintf("year: %s nrow: %d pit_id: %s ", year, nrow(fit_df),input$pit_id)
    # sprintf("year: %s nrow: d", year, nrow(fit_df))
  })

  output$pitcher_components <- renderTable({
    tmp <- BProDRA::get_dra_runs(dra_events_list[[input$year]],
                                 mods_list[[input$year]],
                                 input$pit_id)
    tmp
  })


  output$model_ranef <- renderTable({
    model_names <- names(mods_list[[input$year]])
    ll <- lapply(model_names, function(model_name) {
      tmp <- BProDRA::summarise_ranef(mods_list[[input$year]][[model_name]], input$pit_id)
      tmp$model_name <- model_name
      tmp$mean_value <- tmp$mean_value * 100
      names(tmp) <- c("ranef_name", "mean_valuex100", "model_name")
      tmp
    })
    tmp <- purrr::reduce(ll, rbind.data.frame)
    tmp

  })


})