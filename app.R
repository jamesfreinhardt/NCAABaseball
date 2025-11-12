# --- 1. Load All Libraries ---
library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)         # For replace_na, coalesce
library(DT)            # For data tables
library(shinyjs)       # For enabling/disabling inputs
library(geosphere)     # For Haversine distance calculation
library(zipcodeR)      # For geocoding zip codes
library(plotly)        # For plots
library(bslib)         # For modern layout & accordion
library(shinyWidgets)  # For pickerInput & sliderTextInput

# ===================================================================
# --- GLOBAL SECTION (Runs only ONCE when app starts) ---
# ===================================================================

# --- 2. Load Data ---
merged_data2 <- read.csv("input.csv") # Load your full dataset

# --- 3. Define Constants (Column Names) ---
col_inst_name <- "inst_name"
col_wins <- "wins"
col_losses <- "losses"
col_lat <- "latitude"
col_long <- "longitude"
col_division <- "Division"
col_conference <- "Conference_Name"
col_sat_mean <- "sat_avg"
col_enrollment <- "est_fte"
col_unitid <- "unitid"
col_accept_rate <- "adm_rate"
col_tuition_in <- "tuitionfee_in"
col_tuition_out <- "tuitionfee_out"
col_npt4_pub <- "npt4_pub"
col_npt4_priv <- "npt4_priv"
col_npt45_pub <- "npt45_pub"
col_npt45_priv <- "npt45_priv"
col_nickname <- "Nickname"
col_total_players <- "total_players"
col_avg_p_height <- "avg_p_height_in"
col_avg_other_height <- "avg_other_height_in"
col_count_fr <- "count_Fr"
col_count_so <- "count_So"
col_count_jr <- "count_Jr"
col_count_sr <- "count_Sr"
col_count_other <- "count_Other"
col_state1 <- "top_state_1"
col_state2 <- "top_state_2"
col_state3 <- "top_state_3"
col_state1_count <- "top_state_1_count"
col_state2_count <- "top_state_2_count"
col_state3_count <- "top_state_3_count"
col_locale <- "locale"
col_udgs <- "ugds"


# --- 4. Define Color Palette ---
div_palette <- colorFactor(
  palette = c("blue", "purple", "darkblue"), # 1=blue, 2=purple, 3=darkblue
  domain = c(1, 2, 3),
  na.color = "grey"
)

# --- 5. Pre-process ALL Data ---

# --- Helper function for height conversion ---
inches_to_ft_in <- function(total_inches) {
  if (is.na(total_inches) || total_inches == 0) {
    return("N/A")
  }
  feet <- floor(total_inches / 12)
  inches <- round(total_inches %% 12, 1)
  return(paste0(feet, "' ", inches, "\""))
}

all_map_data <- merged_data2 %>%
  mutate(
    # --- Convert lat/long ---
    LAT_num = as.numeric(.data[[col_lat]]),
    LONG_num = as.numeric(.data[[col_long]]),
    
    # --- Create Win Pct ---
    wins_num = as.numeric(replace_na(.data[[col_wins]], 0)),
    losses_num = as.numeric(replace_na(.data[[col_losses]], 0)),
    total_games = wins_num + losses_num,
    win_pct = ifelse(total_games == 0, 0, wins_num / total_games),
    win_pct = round(win_pct * 100),
    
    # --- Create Acceptance Rate ---
    accept_rate_num = as.numeric(replace_na(.data[[col_accept_rate]], .99)),
    accept_rate_pct = round(accept_rate_num * 100),
    
    # --- Create SAT Score ---
    sat_score = as.numeric(replace_na(.data[[col_sat_mean]], 0)),
    
    # --- Create UG Enrollment ---
    udgs_size = as.numeric(replace_na(.data[[col_udgs]], 0)),
    
    # --- MODIFIED: Create UDGS Size Labels ---
    udgs_label = case_when(
      udgs_size < 1000 ~ "Extra-Small (< 1k)",
      udgs_size < 3000 ~ "Small (1k - 3k)",
      udgs_size < 7000 ~ "Small-Mid (3k - 7k)",
      udgs_size < 15000 ~ "Mid-size (7k - 15k)",
      udgs_size < 30000 ~ "Mid-Large (15k - 30k)",
      udgs_size >= 30000 ~ "Extra Large (30k+)",
      TRUE ~ "N/A"
    ),
    
    # --- Create Tuition & Net Price Columns ---
    tuition_in = as.numeric(replace_na(.data[[col_tuition_in]], 0)),
    tuition_out = as.numeric(replace_na(.data[[col_tuition_out]], 0)),
    
    net_price_avg = coalesce(
      as.numeric(.data[[col_npt4_pub]]), 
      as.numeric(.data[[col_npt4_priv]])
    ),
    net_price_avg = replace_na(net_price_avg, 0), 
    
    net_price_110k = coalesce(
      as.numeric(.data[[col_npt45_pub]]), 
      as.numeric(.data[[col_npt45_priv]])
    ),
    net_price_110k = replace_na(net_price_110k, 0),
    
    # --- Clean up Roster Data ---
    Nickname = replace_na(.data[[col_nickname]], "N/A"),
    total_players = as.numeric(replace_na(.data[[col_total_players]], 0)),
    avg_p_height = as.numeric(replace_na(.data[[col_avg_p_height]], 0)),
    avg_other_height = as.numeric(replace_na(.data[[col_avg_other_height]], 0)),
    
    count_Fr = as.numeric(replace_na(.data[[col_count_fr]], 0)),
    count_So = as.numeric(replace_na(.data[[col_count_so]], 0)),
    count_Jr = as.numeric(replace_na(.data[[col_count_jr]], 0)),
    count_Sr = as.numeric(replace_na(.data[[col_count_sr]], 0)),
    count_Other = as.numeric(replace_na(.data[[col_count_other]], 0)),
    
    top_state_1 = replace_na(.data[[col_state1]], "N/A"),
    top_state_2 = replace_na(.data[[col_state2]], "N/A"),
    top_state_3 = replace_na(.data[[col_state3]], "N/A"),
    
    top_state_1_count = as.numeric(replace_na(.data[[col_state1_count]], 0)),
    top_state_2_count = as.numeric(replace_na(.data[[col_state2_count]], 0)),
    top_state_3_count = as.numeric(replace_na(.data[[col_state3_count]], 0)),
    
    # --- Cleaned up Locale Labels ---
    locale_label = case_when(
      .data[[col_locale]] == 11 ~ "City (Large)",
      .data[[col_locale]] == 12 ~ "City (Midsize)",
      .data[[col_locale]] == 13 ~ "City (Small)",
      .data[[col_locale]] == 21 ~ "Suburb (Large)",
      .data[[col_locale]] == 22 ~ "Suburb (Midsize)",
      .data[[col_locale]] == 23 ~ "Suburb (Small)",
      .data[[col_locale]] == 31 ~ "Town (Fringe)",
      .data[[col_locale]] == 32 ~ "Town (Distant)",
      .data[[col_locale]] == 33 ~ "Town (Remote)",
      .data[[col_locale]] == 41 ~ "Rural (Fringe)",
      .data[[col_locale]] == 42 ~ "Rural (Distant)",
      .data[[col_locale]] == 43 ~ "Rural (Remote)",
      .data[[col_locale]] == -3 ~ "Not available",
      TRUE ~ "Not available"
    ),
    
    # --- Create Marker Color ---
    marker_color = div_palette(.data[[col_division]]),
    
    # --- Create Hover Label ---
    hover_label = paste0(.data[[col_inst_name]], " (D", .data[[col_division]], ")"),
    
    # --- Create Popup Content (with Hyperlink) ---
    popup_content = paste0(
      "<strong><a href='https://collegescorecard.ed.gov/school?", .data[[col_unitid]],
      "' target='_blank'>", .data[[col_inst_name]], "</a></strong>",
      "<br/>",
      "Record: ", .data[[col_wins]], "-", .data[[col_losses]], " (", win_pct, "%)",
      "<br/>",
      "Conference: ", .data[[col_conference]],
      "<br/>",
      "Acceptance Rate: ", accept_rate_pct, "%",
      "<br/>",
      "Total Enrollment: ", formatC(.data[[col_enrollment]], format = "d", big.mark = ","),
      "<br/>",
      "Undergrad. Enrollment: ", formatC(udgs_size, format = "d", big.mark = ","),
      "<br/>",
      "Avg. SAT: ", .data[[col_sat_mean]],
      "<br/>",
      "In-State Tuition: $", formatC(tuition_in, format = "d", big.mark = ","),
      "<br/>",
      "Avg. Net Price: $", formatC(net_price_avg, format = "d", big.mark = ",")
    )
  ) %>%
  # Filter out rows where location data is missing
  filter(
    !is.na(LAT_num) & !is.na(LONG_num)
  )

# --- Get Min/Max for SAT Slider ---
sat_scores_for_range <- all_map_data %>% filter(sat_score > 0)
min_sat <- min(sat_scores_for_range$sat_score, na.rm = TRUE)
max_sat <- max(sat_scores_for_range$sat_score, na.rm = TRUE)

# --- Get Min/Max for Net Price Slider ---
net_price_for_range <- all_map_data %>% filter(net_price_avg > 0)
min_net_price <- min(net_price_for_range$net_price_avg, na.rm = TRUE)
max_net_price <- max(net_price_for_range$net_price_avg, na.rm = TRUE)

# --- Get Unique Locale Choices for Filter ---
locale_choices <- sort(unique(all_map_data$locale_label))

# --- MODIFIED: Get Unique UDGS Choices for Filter ---
udgs_choices <- c(
  "Extra-Small (< 1k)",
  "Small (1k - 3k)",
  "Small-Mid (3k - 7k)",
  "Mid-size (7k - 15k)",
  "Mid-Large (15k - 30k)",
  "Extra Large (30k+)",
  "N/A"
)

# --- Get Unique Conference Choices for Filter ---
conference_choices <- sort(unique(all_map_data$Conference_Name))


# ===================================================================
# --- 6. Define the User Interface (UI) ---
# ===================================================================

ui <- bslib::page_sidebar(
  title = "NCAA Baseball Map",
  
  # --- Sidebar Definition with width ---
  sidebar = bslib::sidebar(
    width = 320, 
    
    shinyjs::useShinyjs(),
    
    # CSS for card style AND multi-column layout
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "digin-style.css"),
      tags$style(HTML("
        .roster-card {
          border: 1px solid #ddd;
          border-radius: 5px;
          padding: 15px;
          margin-bottom: 20px;
          box-shadow: 2px 2px 5px #f0f0f0;
        }
        
        .multicol {
          -webkit-column-count: 3;
          -moz-column-count: 3;
          column-count: 3;
          -webkit-column-fill: balance;
          -moz-column-fill: balance;
          column-fill: balance;
          margin-bottom: 0px; 
        }
        .multicol .shiny-options-group {
           margin-top: 0px;
        }
      "))
    ), # End tags$head
    
    h4(textOutput("school_counter")),
    hr(), 
    
    # --- Accordion Layout ---
    bslib::accordion(
      open = "Location", 
      
      # --- Panel 1: Location ---
      bslib::accordion_panel(
        title = "Location",
        icon = icon("location-dot"),
        textInput("home_zip", "Home Zip Code", value = "21703", placeholder = "e.g., 90210"),
        shinyjs::disabled(
          sliderInput(
            "distance_filter",
            "Max Distance (miles)",
            min = 0, max = 2500,
            value = 2500, step = 50
          )
        )
      ),
      
      # --- Panel 2: Team Attributes ---
      bslib::accordion_panel(
        title = "Team Attributes",
        icon = icon("trophy"),
        
        checkboxGroupInput(
          inputId = "division_filter",
          label = "Division",
          choices = c(1, 2, 3),
          selected = c(1, 2, 3) 
        ),
        
        # Win Pct
        sliderInput(
          inputId = "win_pct_filter",
          label = "Winning Pct (%)",
          min = 0, max = 100,
          value = c(0, 100)
        ),
        
        # Conference Filter
        shinyWidgets::pickerInput(
          inputId = "conference_filter",
          label = "Conference",
          choices = conference_choices,
          selected = conference_choices,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            liveSearch = TRUE
          )
        )
      ),
      
      # --- Panel 3: School Demographics ---
      bslib::accordion_panel(
        title = "School Demographics",
        icon = icon("school"),
        div(class = "multicol",
            checkboxGroupInput(
              inputId = "locale_filter",
              label = "School Locale",
              choices = locale_choices,
              selected = locale_choices
            )
        ),
        
        # --- Labeled Slider ---
        shinyWidgets::sliderTextInput(
          inputId = "udgs_filter",
          label = "Undergrad. Enrollment",
          choices = udgs_choices,
          # Set default range from first item to second-to-last item (ignores "N/A")
          selected = c(udgs_choices[1], udgs_choices[length(udgs_choices) - 2]),
          grid = TRUE,
          force_edges = TRUE
        )
      ),
      
      # --- Panel 4: Academic & Financial ---
      bslib::accordion_panel(
        title = "Academic & Financial",
        icon = icon("book-reader"),
        sliderInput(
          inputId = "accept_rate_filter",
          label = "Acceptance Rate (%)",
          min = 0, max = 100,
          value = c(0, 100)
        ),
        sliderInput(
          inputId = "sat_filter",
          label = "Avg. SAT Score",
          min = min_sat,
          max = max_sat,
          value = c(min_sat, max_sat) 
        ),
        sliderInput(
          inputId = "net_price_filter",
          label = "Avg. Net Price",
          min = min_net_price,
          max = max_net_price,
          value = c(min_net_price, max_net_price),
          step = 1000
        )
      )
    ) # --- End Accordion ---
  ), # --- End bslib::sidebar ---
  
  # --- Main Content Area ---
  bslib::navset_tab(
    id = "main_tabs",
    
    bslib::nav_panel(
      title = "Map",
      leafletOutput("baseball_map", height = "80vh")
    ),
    
    bslib::nav_panel(
      title = "Filtered School List",
      actionButton("add_to_saved", "Add Selected Rows to Saved List"),
      hr(),
      DTOutput("filtered_table")
    ),
    
    bslib::nav_panel(
      title = "Saved List", 
      actionButton("remove_from_saved", "Remove Selected Rows from Saved List"),
      actionButton("clear_saved", "Clear Entire Saved List"),
      hr(),
      DTOutput("saved_table")
    ),
    
    bslib::nav_panel(
      title = "Roster Metrics",
      fluidRow(
        column(12,
               uiOutput("roster_cards_ui")
        )
      )
    )
  ) # --- End bslib::navset_tab
) # --- End bslib::page_sidebar ---


# ===================================================================
# --- 7. Define the Server (The "Brain") ---
# ===================================================================
server <- function(input, output, session) {
  
  # --- ReactiveVal to store saved school IDs ---
  saved_school_ids <- reactiveVal(character(0))
  
  
  # --- Reactive for Home Location ---
  home_location <- reactive({
    req(input$home_zip, nchar(input$home_zip) == 5)
    
    zip_data <- suppressWarnings(
      tryCatch(
        geocode_zip(input$home_zip),
        error = function(e) NULL
      )
    )
    
    if (!is.null(zip_data) && !is.na(zip_data$lat) && !is.na(zip_data$lng)) {
      return(list(lat = zip_data$lat, lon = zip_data$lng))
    } else {
      showNotification(
        paste("Zip code", input$home_zip, "not found."),
        type = "warning",
        duration = 5
      )
      return(NULL)
    }
  })
  
  # --- Enable/Disable Distance Slider ---
  observe({
    home_loc <- home_location()
    
    if (is.null(home_loc)) {
      shinyjs::disable("distance_filter")
      updateSliderInput(session, "distance_filter", value = 2500)
    } else {
      shinyjs::enable("distance_filter")
    }
  })
  
  # --- Add Distance Column to Data ---
  data_with_distance <- reactive({
    home_loc <- home_location()
    
    if (is.null(home_loc)) {
      all_map_data %>%
        mutate(distance_miles = 9999) 
    } else {
      school_coords <- all_map_data %>% select(LONG_num, LAT_num)
      home_coords <- c(home_loc$lon, home_loc$lat)
      distances_m <- distHaversine(home_coords, school_coords)
      
      all_map_data %>%
        mutate(distance_miles = distances_m * 0.000621371)
    }
  })
  
  
  # --- MODIFIED: This is the "brain", now with labeled slider logic ---
  filtered_data <- reactive({
    
    # Require the conference filter to have a selection
    req(input$conference_filter, input$udgs_filter)
    
    # --- Logic for sliderTextInput range ---
    # 1. Find the numeric index of the selected labels
    selected_indices <- match(input$udgs_filter, udgs_choices)
    # 2. Get all labels that fall within that range
    labels_to_include <- udgs_choices[selected_indices[1]:selected_indices[2]]
    
    data_with_distance() %>%
      filter(
        # Standard filters
        .data[[col_division]] %in% input$division_filter &
          locale_label %in% input$locale_filter &
          
          # Conference Filter
          Conference_Name %in% input$conference_filter &
          
          # --- MODIFIED: UDGS Filter ---
          udgs_label %in% labels_to_include &
          
          win_pct >= input$win_pct_filter[1] &
          win_pct <= input$win_pct_filter[2] &
          accept_rate_pct >= input$accept_rate_filter[1] &
          accept_rate_pct <= input$accept_rate_filter[2] &
          (sat_score == 0 | (sat_score >= input$sat_filter[1] & sat_score <= input$sat_filter[2])) &
          
          # Net Price Filter
          (net_price_avg == 0 | (net_price_avg >= input$net_price_filter[1] & net_price_avg <= input$net_price_filter[2])) &
          
          # Distance filter
          distance_miles <= input$distance_filter
      )
  })
  
  # --- Output for the school counter ---
  output$school_counter <- renderText({
    sprintf(
      "Displaying %s of %s schools",
      nrow(filtered_data()),
      nrow(all_map_data)
    )
  })
  
  # --- This creates the *initial* map (runs only once) ---
  output$baseball_map <- renderLeaflet({
    leaflet(data = all_map_data) %>%
      addTiles() %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4) %>%
      addLegend(
        pal = div_palette,
        values = ~get(col_division),
        title = "Division",
        position = "bottomright"
      )
  })
  
  # --- This *updates* the map (runs when filters change) ---
  observe({
    data_to_show <- tryCatch(
      filtered_data(),
      error = function(e) data.frame() # Return empty data frame on error
    )
    
    proxy <- leafletProxy("baseball_map", data = data_to_show) %>%
      clearMarkers() %>%
      clearMarkerClusters()
    
    if (nrow(data_to_show) > 0) {
      proxy %>% 
        addCircleMarkers(
          lng = ~LONG_num,
          lat = ~LAT_num,
          popup = ~popup_content,
          label = ~hover_label,
          layerId = ~unitid, 
          radius = 6,
          weight = 1,
          opacity = 1,
          fillOpacity = 0.7,
          color = ~marker_color,
          fillColor = ~marker_color,
          clusterOptions = markerClusterOptions(disableClusteringAtZoom = 8) 
        )
    }
  })
  
  # --- MODIFIED: Output for the filtered data table ---
  output$filtered_table <- renderDT({
    data_to_show <- filtered_data() %>%
      select(
        "Name" = all_of(col_inst_name),
        "Division" = all_of(col_division),
        "Conf" = all_of(col_conference),
        "Locale" = locale_label,
        "Distance (mi)" = distance_miles,
        "Win %" = win_pct,
        "Accept %" = accept_rate_pct,
        "Size Category" = udgs_label, # --- MODIFIED ---
        "SAT" = sat_score,
        "Tuition (In)" = tuition_in,
        "Tuition (Out)" = tuition_out,
        "Avg. Net Price" = net_price_avg,
        "Net Price (110k+)" = net_price_110k
      ) %>%
      mutate(
        `Distance (mi)` = round(`Distance (mi)`, 1)
      )
    
    datatable(
      data_to_show, 
      selection = 'multiple', 
      options = list(pageLength = 100, scrollX = TRUE) 
    ) %>%
      formatCurrency(
        c("Tuition (In)", "Tuition (Out)", "Avg. Net Price", "Net Price (110k+)"), 
        digits = 0
      )
  })
  
  
  # --- Logic for the "Saved List" ---
  
  observeEvent(input$add_to_saved, {
    selected_rows <- input$filtered_table_rows_selected
    
    if (!is.null(selected_rows)) {
      data_to_add <- filtered_data()[selected_rows, ] %>%
        pull(!!sym(col_unitid))
      
      current_ids <- isolate(saved_school_ids())
      
      saved_school_ids(union(current_ids, data_to_add))
    }
  })
  
  observeEvent(input$remove_from_saved, {
    selected_rows <- input$saved_table_rows_selected
    
    if (!is.null(selected_rows)) {
      ids_to_remove <- saved_data()[selected_rows, ] %>%
        pull(!!sym(col_unitid))
      
      current_ids <- isolate(saved_school_ids())
      
      saved_school_ids(setdiff(current_ids, ids_to_remove))
    }
  })
  
  observeEvent(input$clear_saved, {
    saved_school_ids(character(0)) 
  })
  
  # --- Create a reactive data frame of saved schools ---
  saved_data <- reactive({
    current_ids <- saved_school_ids()
    
    data_with_distance() %>%
      filter(unitid %in% current_ids) %>%
      arrange(match(unitid, current_ids))
  })
  
  # --- MODIFIED: Output the saved table ---
  output$saved_table <- renderDT({
    data_to_show <- saved_data() %>%
      select(
        "Name" = all_of(col_inst_name),
        "Division" = all_of(col_division),
        "Conf" = all_of(col_conference),
        "Locale" = locale_label,
        "Distance (mi)" = distance_miles,
        "Win %" = win_pct,
        "Accept %" = accept_rate_pct,
        "Size Category" = udgs_label, # --- MODIFIED ---
        "SAT" = sat_score,
        "Tuition (In)" = tuition_in,
        "Tuition (Out)" = tuition_out,
        "Avg. Net Price" = net_price_avg,
        "Net Price (110k+)" = net_price_110k
      ) %>%
      mutate(
        `Distance (mi)` = round(`Distance (mi)`, 1)
      )
    
    datatable(
      data_to_show, 
      selection = 'multiple', 
      options = list(pageLength = 25, scrollX = TRUE)
    ) %>%
      formatCurrency(
        c("Tuition (In)", "Tuition (Out)", "Avg. Net Price", "Net Price (110k+)"), 
        digits = 0
      )
  })
  
  
  # --- Dynamic UI for Roster Cards ---
  
  # 1. This function builds the 'skeleton' of the cards
  output$roster_cards_ui <- renderUI({
    
    saved_schools <- saved_data()
    
    if (nrow(saved_schools) == 0) {
      return(h4("Add schools from the 'Filtered School List' to see roster metrics here."))
    }
    
    card_list <- lapply(1:nrow(saved_schools), function(i) {
      
      school_data <- saved_schools[i, ]
      class_plot_id <- paste0("class_plot_", school_data$unitid)
      state_plot_id <- paste0("state_plot_", school_data$unitid)
      
      div(
        class = "roster-card",
        h4(school_data$inst_name),
        h5(paste0("Record: ", school_data$wins, "-", school_data$losses, " (", school_data$win_pct, "%)")),
        p(strong("Nickname:"), school_data$Nickname),
        p(strong("Conference:"), school_data$Conference_Name),
        hr(),
        fluidRow(
          column(4,
                 h5("Roster Details"),
                 p(strong("Roster Size:"), school_data$total_players),
                 p(strong("Avg. Pitcher Ht:"), inches_to_ft_in(school_data$avg_p_height)),
                 p(strong("Avg. Position Ht:"), inches_to_ft_in(school_data$avg_other_height))
          ),
          column(4,
                 plotlyOutput(class_plot_id, height = "250px")
          ),
          column(4,
                 plotlyOutput(state_plot_id, height = "250px")
          )
        )
      ) 
    }) 
    
    tagList(card_list)
    
  }) # End renderUI
  
  
  # 2. This observer block 'fills in' the plots for the skeletons
  observe({
    
    saved_schools <- saved_data()
    
    if (nrow(saved_schools) > 0) {
      
      for (i in 1:nrow(saved_schools)) {
        
        local({
          
          school_data <- saved_schools[i, ]
          
          class_plot_id <- paste0("class_plot_", school_data$unitid)
          state_plot_id <- paste0("state_plot_", school_data$unitid)
          
          # --- Render Class Plot ---
          output[[class_plot_id]] <- renderPlotly({
            
            class_data <- data.frame(
              Class = factor(c("Fr", "So", "Jr", "Sr", "Other"), levels = c("Fr","So", "Jr", "Sr", "Other")),
              Count = c(school_data$count_Fr, school_data$count_So, school_data$count_Jr, school_data$count_Sr, school_data$count_Other)
            )
            
            plot_ly(class_data, x = ~Class, y = ~Count, type = 'bar') %>%
              layout(
                title = "Class Breakdown",
                xaxis = list(title = ""),
                yaxis = list(title = "Count")
              )
          })
          
          # --- Render State Plot ---
          output[[state_plot_id]] <- renderPlotly({
            
            state_data <- data.frame(
              State = c(school_data$top_state_1, school_data$top_state_2, school_data$top_state_3),
              Count = c(school_data$top_state_1_count, school_data$top_state_2_count, school_data$top_state_3_count)
            ) %>%
              filter(State != "N/A" & Count > 0)
            
            plot_ly(state_data, x = ~State, y = ~Count, type = 'bar') %>%
              layout(
                title = "Top 3 Recruiting States",
                xaxis = list(title = ""),
                yaxis = list(title = "Count")
              )
          })
          
        }) # End local()
      } # End for loop
    }
  }) # End observe
  
  
} # <-- This bracket closes the server function

# ===================================================================
# --- 8. Run the App ---
# ===================================================================
shinyApp(ui = ui, server = server)