# Database Functions for User Registration and Profile Management
# CRUD operations for the NCAA Baseball recruiting database

library(DBI)
library(RSQLite)

# Global database path
DB_PATH <- "ncaa_baseball.db"

# ===================================================================
# USER CRUD OPERATIONS
# ===================================================================

# Create new user
create_user <- function(email, first_name, last_name, phone = NULL, 
                       zip_code = NULL, state = NULL) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  
  tryCatch({
    # Check if email already exists
    existing <- dbGetQuery(con, 
      "SELECT user_id FROM users WHERE email = ?", 
      params = list(email))
    
    if (nrow(existing) > 0) {
      dbDisconnect(con)
      return(list(success = FALSE, message = "Email already exists", user_id = NULL))
    }
    
    # Insert new user
    dbExecute(con, 
      "INSERT INTO users (email, first_name, last_name, phone, zip_code, state) 
       VALUES (?, ?, ?, ?, ?, ?)",
      params = list(email, first_name, last_name, phone, zip_code, state))
    
    user_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
    dbDisconnect(con)
    
    return(list(success = TRUE, message = "User created successfully", user_id = user_id))
    
  }, error = function(e) {
    dbDisconnect(con)
    return(list(success = FALSE, message = paste("Error:", e$message), user_id = NULL))
  })
}

# Get user by ID
get_user <- function(user_id) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  user <- dbGetQuery(con, "SELECT * FROM users WHERE user_id = ?", params = list(user_id))
  dbDisconnect(con)
  return(user)
}

# Get user by email
get_user_by_email <- function(email) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  user <- dbGetQuery(con, "SELECT * FROM users WHERE email = ?", params = list(email))
  dbDisconnect(con)
  return(user)
}

# Update user information
update_user <- function(user_id, email = NULL, first_name = NULL, last_name = NULL, 
                       phone = NULL, zip_code = NULL, state = NULL) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  
  tryCatch({
    updates <- list()
    params <- list()
    
    if (!is.null(email)) { updates <- c(updates, "email = ?"); params <- c(params, email) }
    if (!is.null(first_name)) { updates <- c(updates, "first_name = ?"); params <- c(params, first_name) }
    if (!is.null(last_name)) { updates <- c(updates, "last_name = ?"); params <- c(params, last_name) }
    if (!is.null(phone)) { updates <- c(updates, "phone = ?"); params <- c(params, phone) }
    if (!is.null(zip_code)) { updates <- c(updates, "zip_code = ?"); params <- c(params, zip_code) }
    if (!is.null(state)) { updates <- c(updates, "state = ?"); params <- c(params, state) }
    
    if (length(updates) > 0) {
      updates <- c(updates, "updated_at = CURRENT_TIMESTAMP")
      params <- c(params, user_id)
      
      query <- paste0("UPDATE users SET ", paste(updates, collapse = ", "), " WHERE user_id = ?")
      dbExecute(con, query, params = params)
    }
    
    dbDisconnect(con)
    return(list(success = TRUE, message = "User updated successfully"))
    
  }, error = function(e) {
    dbDisconnect(con)
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

# ===================================================================
# PLAYER PROFILE CRUD OPERATIONS
# ===================================================================

# Create or update player profile
save_player_profile <- function(user_id, high_school = NULL, graduation_year = NULL,
                               height_inches = NULL, weight_lbs = NULL,
                               primary_position = NULL, secondary_position = NULL,
                               bats = NULL, throws = NULL) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  
  tryCatch({
    # Check if profile exists
    existing <- dbGetQuery(con, 
      "SELECT profile_id FROM player_profile WHERE user_id = ?", 
      params = list(user_id))
    
    if (nrow(existing) > 0) {
      # Update existing profile
      updates <- list()
      params <- list()
      
      if (!is.null(high_school)) { updates <- c(updates, "high_school = ?"); params <- c(params, high_school) }
      if (!is.null(graduation_year)) { updates <- c(updates, "graduation_year = ?"); params <- c(params, graduation_year) }
      if (!is.null(height_inches)) { updates <- c(updates, "height_inches = ?"); params <- c(params, height_inches) }
      if (!is.null(weight_lbs)) { updates <- c(updates, "weight_lbs = ?"); params <- c(params, weight_lbs) }
      if (!is.null(primary_position)) { updates <- c(updates, "primary_position = ?"); params <- c(params, primary_position) }
      if (!is.null(secondary_position)) { updates <- c(updates, "secondary_position = ?"); params <- c(params, secondary_position) }
      if (!is.null(bats)) { updates <- c(updates, "bats = ?"); params <- c(params, bats) }
      if (!is.null(throws)) { updates <- c(updates, "throws = ?"); params <- c(params, throws) }
      
      if (length(updates) > 0) {
        params <- c(params, user_id)
        query <- paste0("UPDATE player_profile SET ", paste(updates, collapse = ", "), " WHERE user_id = ?")
        dbExecute(con, query, params = params)
      }
      
    } else {
      # Insert new profile
      dbExecute(con, 
        "INSERT INTO player_profile (user_id, high_school, graduation_year, height_inches, 
         weight_lbs, primary_position, secondary_position, bats, throws) 
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(user_id, high_school, graduation_year, height_inches, weight_lbs,
                     primary_position, secondary_position, bats, throws))
    }
    
    dbDisconnect(con)
    return(list(success = TRUE, message = "Player profile saved successfully"))
    
  }, error = function(e) {
    dbDisconnect(con)
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

# Get player profile
get_player_profile <- function(user_id) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  profile <- dbGetQuery(con, "SELECT * FROM player_profile WHERE user_id = ?", params = list(user_id))
  dbDisconnect(con)
  return(profile)
}

# ===================================================================
# ATHLETIC METRICS CRUD OPERATIONS
# ===================================================================

# Save athletic metrics
save_athletic_metrics <- function(user_id, batting_avg = NULL, on_base_pct = NULL,
                                 slugging_pct = NULL, home_runs = NULL, rbi = NULL,
                                 stolen_bases = NULL, era = NULL, strikeouts = NULL,
                                 walks = NULL, innings_pitched = NULL, wins = NULL,
                                 saves = NULL, exit_velocity = NULL, sixty_yard_dash = NULL,
                                 fastball_velocity = NULL) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  
  tryCatch({
    # Check if metrics exist
    existing <- dbGetQuery(con, 
      "SELECT metric_id FROM athletic_metrics WHERE user_id = ?", 
      params = list(user_id))
    
    if (nrow(existing) > 0) {
      # Update existing metrics
      updates <- list()
      params <- list()
      
      if (!is.null(batting_avg)) { updates <- c(updates, "batting_avg = ?"); params <- c(params, batting_avg) }
      if (!is.null(on_base_pct)) { updates <- c(updates, "on_base_pct = ?"); params <- c(params, on_base_pct) }
      if (!is.null(slugging_pct)) { updates <- c(updates, "slugging_pct = ?"); params <- c(params, slugging_pct) }
      if (!is.null(home_runs)) { updates <- c(updates, "home_runs = ?"); params <- c(params, home_runs) }
      if (!is.null(rbi)) { updates <- c(updates, "rbi = ?"); params <- c(params, rbi) }
      if (!is.null(stolen_bases)) { updates <- c(updates, "stolen_bases = ?"); params <- c(params, stolen_bases) }
      if (!is.null(era)) { updates <- c(updates, "era = ?"); params <- c(params, era) }
      if (!is.null(strikeouts)) { updates <- c(updates, "strikeouts = ?"); params <- c(params, strikeouts) }
      if (!is.null(walks)) { updates <- c(updates, "walks = ?"); params <- c(params, walks) }
      if (!is.null(innings_pitched)) { updates <- c(updates, "innings_pitched = ?"); params <- c(params, innings_pitched) }
      if (!is.null(wins)) { updates <- c(updates, "wins = ?"); params <- c(params, wins) }
      if (!is.null(saves)) { updates <- c(updates, "saves = ?"); params <- c(params, saves) }
      if (!is.null(exit_velocity)) { updates <- c(updates, "exit_velocity = ?"); params <- c(params, exit_velocity) }
      if (!is.null(sixty_yard_dash)) { updates <- c(updates, "sixty_yard_dash = ?"); params <- c(params, sixty_yard_dash) }
      if (!is.null(fastball_velocity)) { updates <- c(updates, "fastball_velocity = ?"); params <- c(params, fastball_velocity) }
      
      if (length(updates) > 0) {
        params <- c(params, user_id)
        query <- paste0("UPDATE athletic_metrics SET ", paste(updates, collapse = ", "), " WHERE user_id = ?")
        dbExecute(con, query, params = params)
      }
      
    } else {
      # Insert new metrics
      dbExecute(con, 
        "INSERT INTO athletic_metrics (user_id, batting_avg, on_base_pct, slugging_pct, 
         home_runs, rbi, stolen_bases, era, strikeouts, walks, innings_pitched, wins, 
         saves, exit_velocity, sixty_yard_dash, fastball_velocity) 
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(user_id, batting_avg, on_base_pct, slugging_pct, home_runs, rbi,
                     stolen_bases, era, strikeouts, walks, innings_pitched, wins, saves,
                     exit_velocity, sixty_yard_dash, fastball_velocity))
    }
    
    dbDisconnect(con)
    return(list(success = TRUE, message = "Athletic metrics saved successfully"))
    
  }, error = function(e) {
    dbDisconnect(con)
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

# Get athletic metrics
get_athletic_metrics <- function(user_id) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  metrics <- dbGetQuery(con, "SELECT * FROM athletic_metrics WHERE user_id = ?", params = list(user_id))
  dbDisconnect(con)
  return(metrics)
}

# ===================================================================
# ACADEMIC INFO CRUD OPERATIONS
# ===================================================================

# Save academic info
save_academic_info <- function(user_id, gpa = NULL, weighted_gpa = NULL, 
                              sat_score = NULL, act_score = NULL,
                              class_rank = NULL, class_size = NULL) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  
  tryCatch({
    # Check if academic info exists
    existing <- dbGetQuery(con, 
      "SELECT academic_id FROM academic_info WHERE user_id = ?", 
      params = list(user_id))
    
    if (nrow(existing) > 0) {
      # Update existing info
      updates <- list()
      params <- list()
      
      if (!is.null(gpa)) { updates <- c(updates, "gpa = ?"); params <- c(params, gpa) }
      if (!is.null(weighted_gpa)) { updates <- c(updates, "weighted_gpa = ?"); params <- c(params, weighted_gpa) }
      if (!is.null(sat_score)) { updates <- c(updates, "sat_score = ?"); params <- c(params, sat_score) }
      if (!is.null(act_score)) { updates <- c(updates, "act_score = ?"); params <- c(params, act_score) }
      if (!is.null(class_rank)) { updates <- c(updates, "class_rank = ?"); params <- c(params, class_rank) }
      if (!is.null(class_size)) { updates <- c(updates, "class_size = ?"); params <- c(params, class_size) }
      
      if (length(updates) > 0) {
        params <- c(params, user_id)
        query <- paste0("UPDATE academic_info SET ", paste(updates, collapse = ", "), " WHERE user_id = ?")
        dbExecute(con, query, params = params)
      }
      
    } else {
      # Insert new academic info
      dbExecute(con, 
        "INSERT INTO academic_info (user_id, gpa, weighted_gpa, sat_score, act_score, 
         class_rank, class_size) 
         VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(user_id, gpa, weighted_gpa, sat_score, act_score, class_rank, class_size))
    }
    
    dbDisconnect(con)
    return(list(success = TRUE, message = "Academic info saved successfully"))
    
  }, error = function(e) {
    dbDisconnect(con)
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

# Get academic info
get_academic_info <- function(user_id) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  info <- dbGetQuery(con, "SELECT * FROM academic_info WHERE user_id = ?", params = list(user_id))
  dbDisconnect(con)
  return(info)
}

# ===================================================================
# USER PREFERENCES CRUD OPERATIONS
# ===================================================================

# Save user preferences
save_user_preferences <- function(user_id, preferred_divisions = NULL, 
                                 max_distance_miles = NULL, min_enrollment = NULL,
                                 max_enrollment = NULL, min_acceptance_rate = NULL,
                                 max_acceptance_rate = NULL, max_tuition = NULL,
                                 preferred_climate = NULL, preferred_locale = NULL) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  
  tryCatch({
    # Check if preferences exist
    existing <- dbGetQuery(con, 
      "SELECT preference_id FROM user_preferences WHERE user_id = ?", 
      params = list(user_id))
    
    if (nrow(existing) > 0) {
      # Update existing preferences
      updates <- list()
      params <- list()
      
      if (!is.null(preferred_divisions)) { updates <- c(updates, "preferred_divisions = ?"); params <- c(params, preferred_divisions) }
      if (!is.null(max_distance_miles)) { updates <- c(updates, "max_distance_miles = ?"); params <- c(params, max_distance_miles) }
      if (!is.null(min_enrollment)) { updates <- c(updates, "min_enrollment = ?"); params <- c(params, min_enrollment) }
      if (!is.null(max_enrollment)) { updates <- c(updates, "max_enrollment = ?"); params <- c(params, max_enrollment) }
      if (!is.null(min_acceptance_rate)) { updates <- c(updates, "min_acceptance_rate = ?"); params <- c(params, min_acceptance_rate) }
      if (!is.null(max_acceptance_rate)) { updates <- c(updates, "max_acceptance_rate = ?"); params <- c(params, max_acceptance_rate) }
      if (!is.null(max_tuition)) { updates <- c(updates, "max_tuition = ?"); params <- c(params, max_tuition) }
      if (!is.null(preferred_climate)) { updates <- c(updates, "preferred_climate = ?"); params <- c(params, preferred_climate) }
      if (!is.null(preferred_locale)) { updates <- c(updates, "preferred_locale = ?"); params <- c(params, preferred_locale) }
      
      if (length(updates) > 0) {
        params <- c(params, user_id)
        query <- paste0("UPDATE user_preferences SET ", paste(updates, collapse = ", "), " WHERE user_id = ?")
        dbExecute(con, query, params = params)
      }
      
    } else {
      # Insert new preferences
      dbExecute(con, 
        "INSERT INTO user_preferences (user_id, preferred_divisions, max_distance_miles, 
         min_enrollment, max_enrollment, min_acceptance_rate, max_acceptance_rate, 
         max_tuition, preferred_climate, preferred_locale) 
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(user_id, preferred_divisions, max_distance_miles, min_enrollment,
                     max_enrollment, min_acceptance_rate, max_acceptance_rate, max_tuition,
                     preferred_climate, preferred_locale))
    }
    
    dbDisconnect(con)
    return(list(success = TRUE, message = "Preferences saved successfully"))
    
  }, error = function(e) {
    dbDisconnect(con)
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

# Get user preferences
get_user_preferences <- function(user_id) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  prefs <- dbGetQuery(con, "SELECT * FROM user_preferences WHERE user_id = ?", params = list(user_id))
  dbDisconnect(con)
  return(prefs)
}

# ===================================================================
# HELPER FUNCTIONS
# ===================================================================

# Get complete user profile (all tables joined)
get_complete_profile <- function(user_id) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  
  user <- get_user(user_id)
  profile <- get_player_profile(user_id)
  metrics <- get_athletic_metrics(user_id)
  academic <- get_academic_info(user_id)
  prefs <- get_user_preferences(user_id)
  
  dbDisconnect(con)
  
  return(list(
    user = user,
    profile = profile,
    metrics = metrics,
    academic = academic,
    preferences = prefs
  ))
}

# Delete user and all related data (cascade delete)
delete_user <- function(user_id) {
  con <- dbConnect(RSQLite::SQLite(), DB_PATH)
  
  tryCatch({
    dbExecute(con, "DELETE FROM users WHERE user_id = ?", params = list(user_id))
    dbDisconnect(con)
    return(list(success = TRUE, message = "User deleted successfully"))
    
  }, error = function(e) {
    dbDisconnect(con)
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}
