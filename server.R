library(shiny)
library(DT)
library(lingtypology)
library(ggplot2)
library(dplyr)
library(tidyr)
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
  database %>% 
    mutate(languages = gsub("Sign Language", "SL", languages)) %>% 
    mutate(object = ifelse(grepl("object", form.image.assocaition.pattern),
                           "object",
                           NA),
           handling = ifelse(grepl("handling", form.image.assocaition.pattern),
                             "handling",
                             NA),
           tracing = ifelse(grepl("tracing", form.image.assocaition.pattern),
                            "tracing",
                            NA),
           contour = ifelse(grepl("contour", form.image.assocaition.pattern),
                            "contour",
                            NA)) %>%
    gather(pattern_column, pattern, object:contour) %>%
    na.omit() %>% 
    select(-pattern_column) ->
    database_p
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
  validate(need(nrow(database) > 0,
                "This combination of features for this meaning is not attested."))
    map.feature(database$languages,
                label = database$language,
                map.orientation = "Atlantic",
                features = database$form.image.assocaition.pattern,
                popup = paste("<video width='200' height='150' controls> <source src='",
                              as.character(database$urls),
                              "' type='video/mp4'></video>", sep = ""),
                zoom.level = 2,
                zoom.control = TRUE)})

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
                radius = database_count$n*5,
                zoom.level = 2,
                zoom.control = TRUE)})

# section for graphs ------------------------------------------------------
  output$graph_picture <- renderPlot({
    title <- "Distribution of the Iconicity patterns in all semantic fields"
    if(input$graph_field != "all"){
      database_p %>%
        filter(semantic.field %in% input$graph_field) ->
        database_p
      title <- paste("Distribution of the Iconicity patterns in the semantic field", input$graph_field)
    }

    ifelse(input$graph_field != "all",
           database_t <- database_p[, c(1, 6, 4)],
           database_t <- database_p[, c(1, 2, 6, 4)])
    output$graph_table <- DT::renderDataTable(
      database_t,
      filter = 'top',
      rownames = FALSE,
      options = list(pageLength = 7, autoWidth = FALSE, dom = 'tip'),
      escape = FALSE)
    if(input$graph_type == "absolute values"){
      database_p %>% 
        ggplot(aes(pattern, fill = pattern))+
        geom_bar(aes(y = ..count.., fill = factor(..x..)), position = "dodge", show.legend = FALSE, stat = "count")+
        geom_text(aes(label = ..count..,
                      y= ..count..+..count..*0.15+1), stat= "count") +
        labs(x = "", y = "absolute values", title = title)+
        facet_wrap(~languages)+
        theme_bw()+
        coord_flip()
    } else {
      database_p %>% 
        ggplot(aes(pattern, group = languages)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", show.legend = FALSE) + 
        geom_text(aes(label = scales::percent(..prop..),
                      y= ..prop..+0.1), stat= "count") +
        scale_y_continuous(labels=scales::percent) +
        labs(y = "percentage", title = title, x = "") +
        facet_wrap(~languages)+
        theme_bw()+
        coord_flip()
      }})
  }
