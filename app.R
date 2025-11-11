# --- 1. Load All Libraries ---
library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)         # For replace_na, coalesce
library(DT)            # For data tables
library(shinyjs)       # For enabling/disabling inputs
library(geosphere)     # For Haversine distance calculation
library(zipcodeR)      # For geocoding zip codes

# ===================================================================
# --- GLOBAL SECTION (Runs only ONCE when app starts) ---
# ===================================================================

# --- 2. Load Data ---
# Make sure your input.csv has all the new tuition/net price columns
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


# --- 4. Define Color Palette ---
div_palette <- colorFactor(
  palette = c("blue", "purple", "darkblue"), # 1=blue, 2=purple, 3=darkblue
  domain = c(1, 2, 3),
  na.color = "grey"
)

# --- 5. Pre-process ALL Data ---
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

# --- !!! NEW: Get Min/Max for Net Price Slider !!! ---
net_price_for_range <- all_map_data %>% filter(net_price_avg > 0)
min_net_price <- min(net_price_for_range$net_price_avg, na.rm = TRUE)
max_net_price <- max(net_price_for_range$net_price_avg, na.rm = TRUE)


# ===================================================================
# --- 6. Define the User Interface (UI) ---
# ===================================================================
ui <- fluidPage(
  # Load shinyjs
  shinyjs::useShinyjs(),
  
  titlePanel("NCAA Baseball Map"),
  
  sidebarLayout(
    sidebarPanel(
      # Zip Code Input (with default)
      textInput("home_zip", "Home Zip Code", value = "21703", placeholder = "e.g., 90210"),
      
      # Distance Slider (wrapped in 'disabled')
      shinyjs::disabled(
        sliderInput(
          "distance_filter",
          "Max Distance (miles)",
          min = 0, max = 2500,
          value = 2500, step = 50
        )
      ),
      
      h4(textOutput("school_counter")),
      hr(), 
      
      checkboxGroupInput(
        inputId = "division_filter",
        label = "Division",
        choices = c(1, 2, 3),
        selected = c(1, 2, 3) 
      ),
      
      sliderInput(
        inputId = "win_pct_filter",
        label = "Winning Pct (%)",
        min = 0, max = 100,
        value = c(0, 100)
      ),
      
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
      
      # --- !!! NEW: Net Price Slider !!! ---
      sliderInput(
        inputId = "net_price_filter",
        label = "Avg. Net Price",
        min = min_net_price,
        max = max_net_price,
        value = c(min_net_price, max_net_price),
        step = 1000 # Set step to $1000 increments
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Map", 
                 leafletOutput("baseball_map", height = "80vh")
        ),
        
        tabPanel("Filtered School List",
                 actionButton("add_to_saved", "Add Selected Rows to Saved List"),
                 hr(),
                 DTOutput("filtered_table")
        ),
        
        tabPanel("Saved List", 
                 actionButton("remove_from_saved", "Remove Selected Rows from Saved List"),
                 actionButton("clear_saved", "Clear Entire Saved List"),
                 hr(),
                 DTOutput("saved_table")
        )
      )
    )
  )
)


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
  
  
  # --- MODIFIED: This is the "brain", now with net price filter ---
  filtered_data <- reactive({
    
    data_with_distance() %>%
      filter(
        # Standard filters
        .data[[col_division]] %in% input$division_filter &
          win_pct >= input$win_pct_filter[1] &
          win_pct <= input$win_pct_filter[2] &
          accept_rate_pct >= input$accept_rate_filter[1] &
          accept_rate_pct <= input$accept_rate_filter[2] &
          (sat_score == 0 | (sat_score >= input$sat_filter[1] & sat_score <= input$sat_filter[2])) &
          
          # --- !!! NEW: Net Price Filter !!! ---
          # (Include 0s, or filter by range)
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
    data_to_show <- filtered_data()
    
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
  
  # --- Output for the filtered data table ---
  output$filtered_table <- renderDT({
    data_to_show <- filtered_data() %>%
      select(
        "Name" = all_of(col_inst_name),
        "Division" = all_of(col_division),
        "Conf" = all_of(col_conference),
        "Distance (mi)" = distance_miles,
        "Win %" = win_pct,
        "Accept %" = accept_rate_pct,
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
      options = list(pageLength = 100)
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
  
  # --- Output the saved table ---
  output$saved_table <- renderDT({
    data_to_show <- saved_data() %>%
      select(
        "Name" = all_of(col_inst_name),
        "Division" = all_of(col_division),
        "Conf" = all_of(col_conference),
        "Distance (mi)" = distance_miles,
        "Win %" = win_pct,
        "Accept %" = accept_rate_pct,
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
      options = list(pageLength = 25)
    ) %>%
      formatCurrency(
        c("Tuition (In)", "Tuition (Out)", "Avg. Net Price", "Net Price (110k+)"), 
        digits = 0
      )
  })
  
} # <-- This bracket closes the server function

# ===================================================================
# --- 8. Run the App ---
# ===================================================================
shinyApp(ui = ui, server = server)