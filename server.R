library(shiny)
library(DT)
library(lingtypology)
library(ggplot2)
library(dplyr)
library(markdown)
library(leaflet)

function(input, output) {
# section for word search -------------------------------------------------
  database <- read.csv("iconisity_SL.csv", sep = "\t", stringsAsFactors = FALSE)
  output$full_table <- DT::renderDataTable(
    database[, -c(5, 7)],
    filter = 'top',
    rownames = FALSE,
    options = list(pageLength = 20, autoWidth = FALSE, dom = 'tip'),
    escape = FALSE)
  output$word_map <- renderLeaflet({
  loc <- ifelse(input$loc == "yes", "1", "[^1]")
  if(input$loc == "all"){loc <- "[01-]"}
  person <- ifelse(input$person == "yes", "1", "[^1]")
  if(input$person == "all"){person <- "[01-]"}
  act <- ifelse(input$act == "yes", "1", "[^1]")
  if(input$act == "all"){act <- "[01-]"}
  part_wholes <- ifelse(input$part_wholes == "yes", "1", "[^1]")
  if(input$part_wholes == "all"){part_wholes <- "[01-]"}
  database %>%
    filter(word %in% input$word_search,
           #grepl(input$iconicity_pattern, form.image.assocaition.pattern),
           grepl(loc, Localization),
           grepl(person, Personification),
           grepl(act, Action),
           grepl(part_wholes, Parts.wholes)) ->
      database
    map.feature(database$languages,
                label = database$language,
                map.orientation = "Atlantic",
                features = database$form.image.assocaition.pattern,
                popup = paste("<video width='200' height='150' controls> <source src='",
                              as.character(database$urls),
                              "' type='video/mp4'></video>", sep = ""))})

# section for semantic field search ---------------------------------------
  output$field_map <- renderLeaflet({
    loc <- ifelse(input$loc_f == TRUE, "1", "[^1]")
    if(input$loc_f == "all"){loc <- "[01-]"}
    person <- ifelse(input$person_f == TRUE, "1", "[^1]")
    if(input$person_f == "all"){person <- "[01-]"}
    act <- ifelse(input$act_f == "yes", "1", "[^1]")
    if(input$act_f == "all"){act <- "[01-]"}
    part_wholes <- ifelse(input$part_wholes_f == TRUE, "1", "[^1]")
    if(input$part_wholes_f == "all"){part_wholes <- "[01-]"}
    database %>%
      filter(semantic.field %in% input$field_search,
             grepl(input$iconicity_pattern_f, form.image.assocaition.pattern),
             grepl(loc, Localization),
             grepl(person, Personification),
             grepl(act, Action),
             grepl(part_wholes, Parts.wholes))  ->
      database 
    database %>% 
      count(word, languages) ->
      database_count
    output$field_table <- DT::renderDataTable(
      database[,c(1, 6)],
      filter = 'top',
      rownames = FALSE,
      options = list(pageLength = 7, autoWidth = FALSE, dom = 'tip'),
      escape = FALSE)
    map.feature(database$languages,
                label = paste0(database$language, " (", database_count$n, ")"),
                map.orientation = "Atlantic",
                radius = database_count$n*5)})

# section for graphs ------------------------------------------------------
  output$graph_picture <- renderPlot({
    database %>%
      filter(semantic.field %in% input$graph_field) %>% 
      count(word, languages) %>%
      left_join(database) %>%
      mutate(pattern = ifelse(grepl(input$iconicity_pattern_graph, form.image.assocaition.pattern),
                              paste(input$iconicity_pattern_graph),
                              "other types")) ->
      database
    output$graph_table <- DT::renderDataTable(
      database[,c(1, 2, 6)],
      filter = 'top',
      rownames = FALSE,
      options = list(pageLength = 7, autoWidth = FALSE, dom = 'tip'),
      escape = FALSE)
    database %>% 
      ggplot(aes(languages, n, fill = pattern))+
      geom_bar(stat = "identity", position = "dodge")+
      coord_flip()+
      theme_bw()
    })
  }
