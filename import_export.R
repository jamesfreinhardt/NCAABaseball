# Data Import/Migration Script for Player Registration System
# Use this script to bulk import player data from CSV files

library(DBI)
library(RSQLite)
library(dplyr)

# Source the database files
source("db_setup.R")
source("db_functions.R")

# Initialize database if needed
if (!check_database()) {
  cat("Initializing database...\n")
  init_database()
}

# ===================================================================
# Function: Import players from CSV
# ===================================================================

import_players_from_csv <- function(csv_file) {
  
  if (!file.exists(csv_file)) {
    cat("Error: File not found:", csv_file, "\n")
    return(invisible(NULL))
  }
  
  cat("Reading CSV file:", csv_file, "\n")
  data <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  cat("Found", nrow(data), "records to import\n\n")
  
  success_count <- 0
  error_count <- 0
  
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    
    tryCatch({
      # Create or get user
      existing_user <- get_user_by_email(row$email)
      
      if (nrow(existing_user) > 0) {
        user_id <- existing_user$user_id[1]
        cat(sprintf("[%d/%d] Updating existing user: %s %s\n", 
                    i, nrow(data), row$first_name, row$last_name))
      } else {
        result <- create_user(
          email = row$email,
          first_name = row$first_name,
          last_name = row$last_name,
          phone = if("phone" %in% names(row)) row$phone else NULL,
          zip_code = if("zip_code" %in% names(row)) row$zip_code else NULL,
          state = if("state" %in% names(row)) row$state else NULL
        )
        
        if (!result$success) {
          cat(sprintf("[%d/%d] Error creating user: %s\n", i, nrow(data), result$message))
          error_count <- error_count + 1
          next
        }
        
        user_id <- result$user_id
        cat(sprintf("[%d/%d] Created new user: %s %s (ID: %d)\n", 
                    i, nrow(data), row$first_name, row$last_name, user_id))
      }
      
      # Save player profile if data exists
      if ("high_school" %in% names(row) && !is.na(row$high_school) && row$high_school != "") {
        save_player_profile(
          user_id = user_id,
          high_school = if("high_school" %in% names(row)) row$high_school else NULL,
          graduation_year = if("graduation_year" %in% names(row)) as.integer(row$graduation_year) else NULL,
          height_inches = if("height_inches" %in% names(row)) as.integer(row$height_inches) else NULL,
          weight_lbs = if("weight_lbs" %in% names(row)) as.integer(row$weight_lbs) else NULL,
          primary_position = if("primary_position" %in% names(row)) row$primary_position else NULL,
          secondary_position = if("secondary_position" %in% names(row)) row$secondary_position else NULL,
          bats = if("bats" %in% names(row)) row$bats else NULL,
          throws = if("throws" %in% names(row)) row$throws else NULL
        )
      }
      
      # Save academic info if data exists
      if ("gpa" %in% names(row) && !is.na(row$gpa) && row$gpa > 0) {
        save_academic_info(
          user_id = user_id,
          gpa = if("gpa" %in% names(row)) as.numeric(row$gpa) else NULL,
          weighted_gpa = if("weighted_gpa" %in% names(row)) as.numeric(row$weighted_gpa) else NULL,
          sat_score = if("sat_score" %in% names(row)) as.integer(row$sat_score) else NULL,
          act_score = if("act_score" %in% names(row)) as.integer(row$act_score) else NULL,
          class_rank = if("class_rank" %in% names(row)) as.integer(row$class_rank) else NULL,
          class_size = if("class_size" %in% names(row)) as.integer(row$class_size) else NULL
        )
      }
      
      # Save athletic metrics if data exists
      if ("batting_avg" %in% names(row) || "era" %in% names(row)) {
        save_athletic_metrics(
          user_id = user_id,
          batting_avg = if("batting_avg" %in% names(row)) as.numeric(row$batting_avg) else NULL,
          on_base_pct = if("on_base_pct" %in% names(row)) as.numeric(row$on_base_pct) else NULL,
          slugging_pct = if("slugging_pct" %in% names(row)) as.numeric(row$slugging_pct) else NULL,
          home_runs = if("home_runs" %in% names(row)) as.integer(row$home_runs) else NULL,
          rbi = if("rbi" %in% names(row)) as.integer(row$rbi) else NULL,
          stolen_bases = if("stolen_bases" %in% names(row)) as.integer(row$stolen_bases) else NULL,
          era = if("era" %in% names(row)) as.numeric(row$era) else NULL,
          strikeouts = if("strikeouts" %in% names(row)) as.integer(row$strikeouts) else NULL,
          walks = if("walks" %in% names(row)) as.integer(row$walks) else NULL,
          innings_pitched = if("innings_pitched" %in% names(row)) as.numeric(row$innings_pitched) else NULL,
          wins = if("wins" %in% names(row)) as.integer(row$wins) else NULL,
          saves = if("saves" %in% names(row)) as.integer(row$saves) else NULL,
          exit_velocity = if("exit_velocity" %in% names(row)) as.numeric(row$exit_velocity) else NULL,
          sixty_yard_dash = if("sixty_yard_dash" %in% names(row)) as.numeric(row$sixty_yard_dash) else NULL,
          fastball_velocity = if("fastball_velocity" %in% names(row)) as.numeric(row$fastball_velocity) else NULL
        )
      }
      
      success_count <- success_count + 1
      
    }, error = function(e) {
      cat(sprintf("[%d/%d] Error: %s\n", i, nrow(data), e$message))
      error_count <- error_count + 1
    })
  }
  
  cat("\n=== Import Summary ===\n")
  cat("Total records:", nrow(data), "\n")
  cat("Successful:", success_count, "\n")
  cat("Errors:", error_count, "\n")
  
  invisible(list(total = nrow(data), success = success_count, errors = error_count))
}

# ===================================================================
# Function: Export players to CSV
# ===================================================================

export_players_to_csv <- function(output_file = "player_export.csv") {
  
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  
  # Query all player data
  query <- "
    SELECT 
      u.user_id,
      u.email,
      u.first_name,
      u.last_name,
      u.phone,
      u.zip_code,
      u.state,
      u.created_at,
      p.high_school,
      p.graduation_year,
      p.height_inches,
      p.weight_lbs,
      p.primary_position,
      p.secondary_position,
      p.bats,
      p.throws,
      a.gpa,
      a.weighted_gpa,
      a.sat_score,
      a.act_score,
      a.class_rank,
      a.class_size,
      m.batting_avg,
      m.on_base_pct,
      m.slugging_pct,
      m.home_runs,
      m.rbi,
      m.stolen_bases,
      m.era,
      m.strikeouts,
      m.walks,
      m.innings_pitched,
      m.wins,
      m.saves,
      m.exit_velocity,
      m.sixty_yard_dash,
      m.fastball_velocity,
      pr.preferred_divisions,
      pr.max_distance_miles,
      pr.max_tuition,
      pr.preferred_locale
    FROM users u
    LEFT JOIN player_profile p ON u.user_id = p.user_id
    LEFT JOIN academic_info a ON u.user_id = a.user_id
    LEFT JOIN athletic_metrics m ON u.user_id = m.user_id
    LEFT JOIN user_preferences pr ON u.user_id = pr.user_id
    ORDER BY u.last_name, u.first_name
  "
  
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  
  write.csv(data, output_file, row.names = FALSE, na = "")
  
  cat("Exported", nrow(data), "player records to:", output_file, "\n")
  
  invisible(data)
}

# ===================================================================
# Function: Create sample CSV template
# ===================================================================

create_import_template <- function(template_file = "player_import_template.csv") {
  
  # Create a sample template with column headers and one example row
  template <- data.frame(
    email = "player@example.com",
    first_name = "John",
    last_name = "Smith",
    phone = "555-123-4567",
    zip_code = "21703",
    state = "MD",
    high_school = "Example High School",
    graduation_year = 2025,
    height_inches = 72,
    weight_lbs = 185,
    primary_position = "Shortstop",
    secondary_position = "Second Base",
    bats = "Right",
    throws = "Right",
    gpa = 3.5,
    weighted_gpa = 3.8,
    sat_score = 1300,
    act_score = 28,
    class_rank = 25,
    class_size = 300,
    batting_avg = 0.350,
    on_base_pct = 0.425,
    slugging_pct = 0.550,
    home_runs = 8,
    rbi = 42,
    stolen_bases = 15,
    era = NA,
    strikeouts = NA,
    walks = NA,
    innings_pitched = NA,
    wins = NA,
    saves = NA,
    exit_velocity = 92.5,
    sixty_yard_dash = 6.8,
    fastball_velocity = NA,
    stringsAsFactors = FALSE
  )
  
  write.csv(template, template_file, row.names = FALSE, na = "")
  
  cat("Created import template:", template_file, "\n")
  cat("Edit this file with your player data and use import_players_from_csv() to import\n")
  cat("\nRequired columns: email, first_name, last_name\n")
  cat("Optional columns: All others (leave blank or NA if not applicable)\n")
  
  invisible(template)
}

# ===================================================================
# Example Usage
# ===================================================================

if (!interactive()) {
  cat("=== Player Data Import/Export Tool ===\n\n")
  
  cat("Available functions:\n")
  cat("1. create_import_template('my_template.csv') - Create a CSV template\n")
  cat("2. import_players_from_csv('my_data.csv') - Import players from CSV\n")
  cat("3. export_players_to_csv('my_export.csv') - Export all players to CSV\n")
  cat("\nExample:\n")
  cat("  create_import_template('players.csv')\n")
  cat("  # Edit players.csv with your data\n")
  cat("  import_players_from_csv('players.csv')\n")
}
