library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)

shinyServer(function(input, output, session) {
  
  dam_df <- read.csv("DAM_S22024PE2.csv")
  
  # VIS1
  flr_type_summ <- dam_df %>% group_by(flr_type) %>%
    summarise(sum_metre_squared_floor = sum(flr_space)) %>%
    arrange(desc(sum_metre_squared_floor))
  
  average_floor_area <- mean(flr_type_summ$sum_metre_squared_floor)
  
  # Plot the bar plot
  vis1 <- ggplot(data = flr_type_summ, aes(x = reorder(flr_type, -sum_metre_squared_floor), y = sum_metre_squared_floor)) +
    geom_bar(stat = "identity", fill = "red") +  
    geom_text(aes(label = scales::comma(sum_metre_squared_floor)), vjust = -0.5, size = 3.5) +
    labs(
      x = "Floor Type",
      y = "Total Square Meters"
    ) +
    # Converts scientific notation to integer format
    scale_y_continuous(labels = scales::comma) +  
    # Adds an average line
    geom_hline(yintercept = average_floor_area, linetype = "dashed", color = "black") +  
    # Annotate the average line
    annotate("text", x = 1, y = average_floor_area + (max(flr_type_summ$sum_metre_squared_floor) * 0.05), 
             label = paste("Average:\n", scales::comma(round(average_floor_area, 0))), 
             vjust = -1, size = 3.5, color = "black") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  # Decrease size and rotate x-axis labels
      plot.margin = margin(10, 10, 10, 10)  # Increase plot margins
    )
  
  # Export vis1 to the UI
  output$vis1Plot <- renderPlot({
    vis1
  })
  
  # Create interpretation
  vis1_interpretation <- "
  This bar plot reveals a significant imbalance in Melbourne’s urban development, with office spaces at 2,814,136 m² far outpacing other types like retail (522,017 m²) and education (312,053 m²). 
  Urban planners and developers can focus on underrepresented areas, such as recreation (135,556 m²), to create a more balanced, livable city.
  "
  # Export vis1 interpretation to the UI
  output$vis1Text <- renderUI({
    div(style = "text-align: justify; font-size: 14px;", vis1_interpretation)
  })
  
  
  
  
  # VIS 2
  
  # Aggregate floor space by year and floor type
  aggregated_data <- dam_df %>%
    group_by(year_completed, flr_type) %>%
    summarise(total_flr_space = sum(flr_space, na.rm = TRUE))
  
  # Determine the top 3 floor types
  top_flr_types <- flr_type_summ %>% slice(1:3) %>% select(flr_type)
  top_flr_types <- top_flr_types$flr_type
  
  # Create a color palette, assigning specific colors to the top 3 floor types and gray to the rest
  palette <- setNames(rep("gray", length(flr_type_summ$flr_type)), unique(aggregated_data$flr_type))
  palette[top_flr_types] <- c("red", "blue", "green")
  
  # Plot the data
  vis2 <- ggplot(data = aggregated_data, aes(x = year_completed, y = total_flr_space, group = flr_type, color = flr_type)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    # add Overall Trend line color palatte
    scale_color_manual(values = c(palette, "Overall Trend" = "black")) +  
    # create the trend line
    geom_smooth(method = "lm", aes(group = 1, linetype = "Overall Trend"), 
                color = "black", linewidth = 1, se = FALSE) +  
    scale_linetype_manual(values = c("Overall Trend" = "dashed")) + 
    # Add text labels for only the top 1 floor type
    geom_text(data = aggregated_data %>% filter(flr_type == "office_flr"),
              aes(label = scales::comma(round(total_flr_space, 0))),
              vjust = -0.5, size = 3, color = "black") + 
    labs(
      x = "Year of Completion",
      y = "Total Lettable Floor Space (Square Meters)",
      color = "Floor Type",
      linetype = NULL
    ) +
    # fix the x and y axis label
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(min(aggregated_data$year_completed), max(aggregated_data$year_completed), by = 3)) +
    theme_minimal()
  
  
  # Export vis2 to the UI
  output$vis2Plot <- renderPlot({
    vis2
  })
  
  # Create interpretation
  vis2_interpretation <- "
  This line plot emphasizes the three most prominent floor types—office, retail, and education—since the trends for other categories are minimal. 
  Office space shows considerable variation, with a peak of 351,028 m², while retail and education remain more consistent. 
  Overall, lettable floor space has been declining over time, reflecting a reduction in new developments. 
  Focusing on these key categories offers a more precise view of the dominant trends, facilitating targeted analysis of the factors influencing these changes
  for example in economic analysis and business strategy.
  "
  
  # Export vis2 interpretation to the UI
  output$vis2Text <- renderUI({
    div(style = "text-align: justify; font-size: 14px;", vis2_interpretation)
  })
  
  
  
  
  # VIS 3: Leaflet Map with Interactive Features
  subset_df <- dam_df %>% filter(flr_type %in% top_flr_types)
  
  filtered_df <- reactive({
    subset_df %>%
      filter(flr_type %in% input$flr_type_filter,
             flr_space >= input$flr_space_range[1],
             flr_space <= input$flr_space_range[2])
  })
  
  # Define a color palette
  map_palette <- colorFactor(palette = "Set1", domain = subset_df$flr_type)
  
  # Initialize the slider and checklist choices
  observe({
    updateSliderInput(session, "flr_space_range", 
                      min = min(subset_df$flr_space), 
                      max = max(subset_df$flr_space), 
                      value = c(min(subset_df$flr_space), max(subset_df$flr_space)))
    updateCheckboxGroupInput(session, "flr_type_filter", choices = top_flr_types, selected = top_flr_types)
  })
  
  output$vis3Map <- renderLeaflet({
    leaflet(filtered_df()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = ~map_palette(flr_type),
        # Scale radius for better visualisation
        radius = ~sqrt(flr_space) / 20,
        stroke = FALSE,
        fillOpacity = 0.7,
        label = ~paste0(
          "Year Completed:", year_completed,
          " Development Key: ", development_key, 
          " Category: ", flr_type,
          " Lettable Floor Space: ", flr_space, " m2"
        )
      ) %>%
      # add legend
      addLegend(
        "bottomright",
        pal = map_palette,
        values = ~flr_type,
        title = "Floor Type",
        opacity = 1
      )
  })
  
  vis3_interpretation <- "
  The map displays lettable floor space in Melbourne, highlighting educational (red), office (green), and retail (blue) areas. 
  Students may prefer locations near Melbourne Central, Carlton, or Southern Cross for access to schools and train stations. 
  Office workers might favor the CBD for proximity to jobs, while those seeking shopping amenities should consider Melbourne Central or South Wharf. 
  Proximity to transportation hubs like Southern Cross offers easy commutes. 
  The map underscores the need to balance access to education, work, and amenities with personal preferences, such as quiet living near parks or away from busy developments.
  "
  
  # Export vis3 interpretation to the UI
  output$vis3Text <- renderUI({
    div(style = "text-align: justify; font-size: 14px;", vis3_interpretation)
  })

  data_desc <- "
  <p>
  Data Sourcea: <a href='https://data.melbourne.vic.gov.au/explore/dataset/development-activity-monitor/information/?disjunctive.status&disjunctive.clue_small_area&disjunctive.clue_block'>Development Activity Monitor (DAM)</a>
  <p>
  
  <p>
  Licensor: <a href='https://creativecommons.org/licenses/by/4.0/legalcode'>CC BY</a>
  <p>
  
  <p>
  Version: July 2024
  <p>
  "
  
  output$dataDesc <- renderUI({
    div(style = "text-align: justify; font-size: 14px;", HTML(data_desc))
  })
})