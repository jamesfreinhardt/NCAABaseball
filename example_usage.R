# Example Usage of the Registration Database Functions
# This script demonstrates how to programmatically use the registration system

# Source the database files
source("db_setup.R")
source("db_functions.R")

# Initialize database if it doesn't exist
if (!check_database()) {
  cat("Initializing database...\n")
  init_database()
}

# ===================================================================
# Example 1: Create a new player profile
# ===================================================================

cat("\n=== Example 1: Creating a New Player Profile ===\n")

# Step 1: Create a user account
user_result <- create_user(
  email = "mike.trout@example.com",
  first_name = "Mike",
  last_name = "Trout",
  phone = "555-123-4567",
  zip_code = "08033",
  state = "NJ"
)

if (user_result$success) {
  cat("User created with ID:", user_result$user_id, "\n")
  user_id <- user_result$user_id
  
  # Step 2: Add player profile information
  save_player_profile(
    user_id = user_id,
    high_school = "Millville Senior High School",
    graduation_year = 2009,
    height_inches = 74,  # 6'2"
    weight_lbs = 235,
    primary_position = "Outfield",
    secondary_position = "First Base",
    bats = "Right",
    throws = "Right"
  )
  
  # Step 3: Add athletic metrics
  save_athletic_metrics(
    user_id = user_id,
    batting_avg = 0.531,
    home_runs = 18,
    rbi = 45,
    stolen_bases = 20,
    exit_velocity = 95.5,
    sixty_yard_dash = 6.5
  )
  
  # Step 4: Add academic information
  save_academic_info(
    user_id = user_id,
    gpa = 3.5,
    sat_score = 1200
  )
  
  # Step 5: Add college search preferences
  save_user_preferences(
    user_id = user_id,
    preferred_divisions = "1",
    max_distance_miles = 1000,
    max_tuition = 60000,
    preferred_locale = "City, Suburb"
  )
  
  cat("Complete profile created!\n")
}

# ===================================================================
# Example 2: Retrieve and display a player profile
# ===================================================================

cat("\n=== Example 2: Retrieving a Player Profile ===\n")

# Get user by email
user <- get_user_by_email("mike.trout@example.com")

if (nrow(user) > 0) {
  user_id <- user$user_id[1]
  
  # Get complete profile
  profile <- get_complete_profile(user_id)
  
  cat("\n--- Personal Information ---\n")
  cat("Name:", profile$user$first_name, profile$user$last_name, "\n")
  cat("Email:", profile$user$email, "\n")
  cat("Phone:", profile$user$phone, "\n")
  cat("Location:", profile$user$zip_code, ",", profile$user$state, "\n")
  
  if (nrow(profile$profile) > 0) {
    cat("\n--- Player Profile ---\n")
    cat("High School:", profile$profile$high_school, "\n")
    cat("Graduation Year:", profile$profile$graduation_year, "\n")
    cat("Height:", floor(profile$profile$height_inches / 12), "'", 
        profile$profile$height_inches %% 12, '"', "\n")
    cat("Weight:", profile$profile$weight_lbs, "lbs\n")
    cat("Position:", profile$profile$primary_position, "\n")
    cat("Bats/Throws:", profile$profile$bats, "/", profile$profile$throws, "\n")
  }
  
  if (nrow(profile$academic) > 0) {
    cat("\n--- Academic Information ---\n")
    cat("GPA:", profile$academic$gpa, "\n")
    if (!is.na(profile$academic$sat_score)) {
      cat("SAT:", profile$academic$sat_score, "\n")
    }
    if (!is.na(profile$academic$act_score)) {
      cat("ACT:", profile$academic$act_score, "\n")
    }
  }
  
  if (nrow(profile$metrics) > 0) {
    cat("\n--- Athletic Metrics ---\n")
    if (!is.na(profile$metrics$batting_avg)) {
      cat("Batting Average:", round(profile$metrics$batting_avg, 3), "\n")
    }
    if (!is.na(profile$metrics$home_runs)) {
      cat("Home Runs:", profile$metrics$home_runs, "\n")
    }
    if (!is.na(profile$metrics$exit_velocity)) {
      cat("Exit Velocity:", profile$metrics$exit_velocity, "mph\n")
    }
    if (!is.na(profile$metrics$sixty_yard_dash)) {
      cat("60-Yard Dash:", profile$metrics$sixty_yard_dash, "seconds\n")
    }
  }
  
  if (nrow(profile$preferences) > 0) {
    cat("\n--- College Preferences ---\n")
    cat("Preferred Divisions:", profile$preferences$preferred_divisions, "\n")
    cat("Max Distance:", profile$preferences$max_distance_miles, "miles\n")
    cat("Max Tuition: $", format(profile$preferences$max_tuition, big.mark = ","), "\n", sep = "")
  }
}

# ===================================================================
# Example 3: Update existing profile
# ===================================================================

cat("\n=== Example 3: Updating a Player Profile ===\n")

# Update user contact information
update_user(
  user_id = user_id,
  phone = "555-999-8888"
)

# Update athletic metrics (add pitching stats)
save_athletic_metrics(
  user_id = user_id,
  era = 2.35,
  strikeouts = 45,
  wins = 8,
  fastball_velocity = 92.0
)

cat("Profile updated!\n")

# ===================================================================
# Example 4: Create a pitcher profile
# ===================================================================

cat("\n=== Example 4: Creating a Pitcher Profile ===\n")

# Create another user
pitcher_result <- create_user(
  email = "clayton.kershaw@example.com",
  first_name = "Clayton",
  last_name = "Kershaw",
  zip_code = "75201",
  state = "TX"
)

if (pitcher_result$success) {
  pitcher_id <- pitcher_result$user_id
  
  save_player_profile(
    user_id = pitcher_id,
    high_school = "Highland Park High School",
    graduation_year = 2006,
    height_inches = 76,  # 6'4"
    weight_lbs = 225,
    primary_position = "Pitcher",
    bats = "Left",
    throws = "Left"
  )
  
  # Pitching-focused metrics
  save_athletic_metrics(
    user_id = pitcher_id,
    era = 1.45,
    strikeouts = 125,
    walks = 18,
    innings_pitched = 95.1,
    wins = 13,
    saves = 0,
    fastball_velocity = 96.5
  )
  
  save_academic_info(
    user_id = pitcher_id,
    gpa = 3.8,
    sat_score = 1380
  )
  
  cat("Pitcher profile created!\n")
}

# ===================================================================
# Example 5: Query all users
# ===================================================================

cat("\n=== Example 5: Listing All Registered Players ===\n")

con <- dbConnect(RSQLite::SQLite(), DB_PATH)
all_users <- dbGetQuery(con, "
  SELECT u.user_id, u.first_name, u.last_name, u.email, 
         p.graduation_year, p.primary_position, p.height_inches,
         a.gpa, a.sat_score
  FROM users u
  LEFT JOIN player_profile p ON u.user_id = p.user_id
  LEFT JOIN academic_info a ON u.user_id = a.user_id
  ORDER BY u.last_name
")
dbDisconnect(con)

if (nrow(all_users) > 0) {
  cat("\nRegistered Players:\n")
  cat("==================\n")
  for (i in 1:nrow(all_users)) {
    player <- all_users[i, ]
    cat(sprintf("\n%s. %s %s (%s)\n", 
                i, player$first_name, player$last_name, player$email))
    if (!is.na(player$primary_position)) {
      cat("   Position:", player$primary_position, "\n")
    }
    if (!is.na(player$graduation_year)) {
      cat("   Grad Year:", player$graduation_year, "\n")
    }
    if (!is.na(player$gpa)) {
      cat("   GPA:", player$gpa, "\n")
    }
  }
  cat("\nTotal registered players:", nrow(all_users), "\n")
}

# ===================================================================
# Example 6: Search for players by criteria
# ===================================================================

cat("\n=== Example 6: Searching for Players by Criteria ===\n")

# Find all pitchers graduating in or after 2006
con <- dbConnect(RSQLite::SQLite(), DB_PATH)
pitchers <- dbGetQuery(con, "
  SELECT u.first_name, u.last_name, p.graduation_year, 
         m.era, m.fastball_velocity
  FROM users u
  JOIN player_profile p ON u.user_id = p.user_id
  JOIN athletic_metrics m ON u.user_id = m.user_id
  WHERE p.primary_position = 'Pitcher' 
    AND p.graduation_year >= 2006
    AND m.era IS NOT NULL
  ORDER BY m.era
")
dbDisconnect(con)

if (nrow(pitchers) > 0) {
  cat("\nPitchers (sorted by ERA):\n")
  cat("========================\n")
  for (i in 1:nrow(pitchers)) {
    pitcher <- pitchers[i, ]
    cat(sprintf("%s. %s %s - ERA: %.2f, FB Vel: %.1f mph (Class of %d)\n",
                i, pitcher$first_name, pitcher$last_name, 
                pitcher$era, pitcher$fastball_velocity, pitcher$graduation_year))
  }
}

cat("\n=== Examples Completed ===\n")
cat("Database location:", DB_PATH, "\n")
cat("\nYou can explore the database using SQLite tools or the Shiny app.\n")
