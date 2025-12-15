# Database Setup Script for NCAA Baseball Recruiting App
# This script creates the SQLite database and tables for user registration

library(DBI)
library(RSQLite)

# Function to initialize the database
init_database <- function(db_path = "ncaa_baseball.db") {
  
  # Create connection to SQLite database
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Create users table (basic account information)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      user_id INTEGER PRIMARY KEY AUTOINCREMENT,
      email TEXT UNIQUE NOT NULL,
      first_name TEXT NOT NULL,
      last_name TEXT NOT NULL,
      phone TEXT,
      zip_code TEXT,
      state TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Create player_profile table (athletic information)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS player_profile (
      profile_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      high_school TEXT,
      graduation_year INTEGER,
      height_inches INTEGER,
      weight_lbs INTEGER,
      primary_position TEXT,
      secondary_position TEXT,
      bats TEXT,
      throws TEXT,
      FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
    )
  ")
  
  # Create athletic_metrics table (performance statistics)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS athletic_metrics (
      metric_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      batting_avg REAL,
      on_base_pct REAL,
      slugging_pct REAL,
      home_runs INTEGER,
      rbi INTEGER,
      stolen_bases INTEGER,
      era REAL,
      strikeouts INTEGER,
      walks INTEGER,
      innings_pitched REAL,
      wins INTEGER,
      saves INTEGER,
      exit_velocity REAL,
      sixty_yard_dash REAL,
      fastball_velocity REAL,
      FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
    )
  ")
  
  # Create academic_info table (academic information)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS academic_info (
      academic_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      gpa REAL,
      weighted_gpa REAL,
      sat_score INTEGER,
      act_score INTEGER,
      class_rank INTEGER,
      class_size INTEGER,
      FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
    )
  ")
  
  # Create user_preferences table (college search preferences)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_preferences (
      preference_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      preferred_divisions TEXT,
      max_distance_miles INTEGER,
      min_enrollment INTEGER,
      max_enrollment INTEGER,
      min_acceptance_rate REAL,
      max_acceptance_rate REAL,
      max_tuition INTEGER,
      preferred_climate TEXT,
      preferred_locale TEXT,
      FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
    )
  ")
  
  # Create indexes for better query performance
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_player_user_id ON player_profile(user_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_athletic_user_id ON athletic_metrics(user_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_academic_user_id ON academic_info(user_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_preferences_user_id ON user_preferences(user_id)")
  
  # Close connection
  dbDisconnect(con)
  
  message("Database initialized successfully at: ", db_path)
  return(TRUE)
}

# Function to check if database exists and is properly initialized
check_database <- function(db_path = "ncaa_baseball.db") {
  if (!file.exists(db_path)) {
    return(FALSE)
  }
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  tables <- dbListTables(con)
  dbDisconnect(con)
  
  required_tables <- c("users", "player_profile", "athletic_metrics", 
                       "academic_info", "user_preferences")
  
  return(all(required_tables %in% tables))
}

# Run initialization if script is called directly
if (!interactive()) {
  init_database()
}
