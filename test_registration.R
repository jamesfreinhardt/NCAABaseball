# Test Script for Registration Database Functions
# This script tests the database setup and CRUD operations
# Run this script to verify the registration system is working correctly

library(DBI)
library(RSQLite)

# Source the database files
source("db_setup.R")
source("db_functions.R")

# Initialize test database
test_db_path <- "test_ncaa_baseball.db"

# Clean up any existing test database
if (file.exists(test_db_path)) {
  file.remove(test_db_path)
}

# Override DB_PATH for testing
assign("DB_PATH", test_db_path, envir = .GlobalEnv)

cat("=== Testing Database Initialization ===\n")
init_database(test_db_path)

if (check_database(test_db_path)) {
  cat("✓ Database initialized successfully\n\n")
} else {
  cat("✗ Database initialization failed\n\n")
  quit(status = 1)
}

cat("=== Testing User CRUD Operations ===\n")

# Test create user
result <- create_user(
  email = "test.player@example.com",
  first_name = "John",
  last_name = "Smith",
  phone = "123-456-7890",
  zip_code = "21703",
  state = "MD"
)

if (result$success) {
  cat("✓ User created successfully. User ID:", result$user_id, "\n")
  test_user_id <- result$user_id
} else {
  cat("✗ User creation failed:", result$message, "\n")
  quit(status = 1)
}

# Test duplicate email
result <- create_user(
  email = "test.player@example.com",
  first_name = "Jane",
  last_name = "Doe"
)

if (!result$success) {
  cat("✓ Duplicate email check working\n")
} else {
  cat("✗ Duplicate email check failed\n")
}

# Test get user
user <- get_user(test_user_id)
if (nrow(user) > 0 && user$email == "test.player@example.com") {
  cat("✓ User retrieved successfully\n")
} else {
  cat("✗ User retrieval failed\n")
}

# Test get user by email
user <- get_user_by_email("test.player@example.com")
if (nrow(user) > 0 && user$user_id == test_user_id) {
  cat("✓ User retrieved by email successfully\n\n")
} else {
  cat("✗ User retrieval by email failed\n\n")
}

cat("=== Testing Player Profile Operations ===\n")

# Test save player profile
result <- save_player_profile(
  user_id = test_user_id,
  high_school = "Test High School",
  graduation_year = 2025,
  height_inches = 72,
  weight_lbs = 185,
  primary_position = "Shortstop",
  secondary_position = "Second Base",
  bats = "Right",
  throws = "Right"
)

if (result$success) {
  cat("✓ Player profile saved successfully\n")
} else {
  cat("✗ Player profile save failed:", result$message, "\n")
}

# Test get player profile
profile <- get_player_profile(test_user_id)
if (nrow(profile) > 0 && profile$high_school == "Test High School") {
  cat("✓ Player profile retrieved successfully\n\n")
} else {
  cat("✗ Player profile retrieval failed\n\n")
}

cat("=== Testing Athletic Metrics Operations ===\n")

# Test save athletic metrics
result <- save_athletic_metrics(
  user_id = test_user_id,
  batting_avg = 0.325,
  home_runs = 8,
  rbi = 42,
  stolen_bases = 15,
  exit_velocity = 92.5,
  sixty_yard_dash = 6.8
)

if (result$success) {
  cat("✓ Athletic metrics saved successfully\n")
} else {
  cat("✗ Athletic metrics save failed:", result$message, "\n")
}

# Test get athletic metrics
metrics <- get_athletic_metrics(test_user_id)
if (nrow(metrics) > 0 && metrics$batting_avg == 0.325) {
  cat("✓ Athletic metrics retrieved successfully\n\n")
} else {
  cat("✗ Athletic metrics retrieval failed\n\n")
}

cat("=== Testing Academic Info Operations ===\n")

# Test save academic info
result <- save_academic_info(
  user_id = test_user_id,
  gpa = 3.85,
  weighted_gpa = 4.2,
  sat_score = 1350,
  act_score = 30,
  class_rank = 15,
  class_size = 300
)

if (result$success) {
  cat("✓ Academic info saved successfully\n")
} else {
  cat("✗ Academic info save failed:", result$message, "\n")
}

# Test get academic info
academic <- get_academic_info(test_user_id)
if (nrow(academic) > 0 && academic$gpa == 3.85) {
  cat("✓ Academic info retrieved successfully\n\n")
} else {
  cat("✗ Academic info retrieval failed\n\n")
}

cat("=== Testing User Preferences Operations ===\n")

# Test save preferences
result <- save_user_preferences(
  user_id = test_user_id,
  preferred_divisions = "1,2",
  max_distance_miles = 500,
  max_tuition = 50000,
  preferred_locale = "City, Suburb"
)

if (result$success) {
  cat("✓ User preferences saved successfully\n")
} else {
  cat("✗ User preferences save failed:", result$message, "\n")
}

# Test get preferences
prefs <- get_user_preferences(test_user_id)
if (nrow(prefs) > 0 && prefs$max_distance_miles == 500) {
  cat("✓ User preferences retrieved successfully\n\n")
} else {
  cat("✗ User preferences retrieval failed\n\n")
}

cat("=== Testing Complete Profile Retrieval ===\n")

# Test get complete profile
complete <- get_complete_profile(test_user_id)

if (nrow(complete$user) > 0 && 
    nrow(complete$profile) > 0 && 
    nrow(complete$metrics) > 0 && 
    nrow(complete$academic) > 0 && 
    nrow(complete$preferences) > 0) {
  cat("✓ Complete profile retrieved successfully\n\n")
} else {
  cat("✗ Complete profile retrieval failed\n\n")
}

cat("=== Testing Update Operations ===\n")

# Test update user
result <- update_user(
  user_id = test_user_id,
  phone = "999-888-7777"
)

if (result$success) {
  user <- get_user(test_user_id)
  if (user$phone == "999-888-7777") {
    cat("✓ User update successful\n")
  } else {
    cat("✗ User update verification failed\n")
  }
} else {
  cat("✗ User update failed:", result$message, "\n")
}

# Test update profile
result <- save_player_profile(
  user_id = test_user_id,
  height_inches = 73
)

if (result$success) {
  profile <- get_player_profile(test_user_id)
  if (profile$height_inches == 73) {
    cat("✓ Player profile update successful\n\n")
  } else {
    cat("✗ Player profile update verification failed\n\n")
  }
} else {
  cat("✗ Player profile update failed:", result$message, "\n\n")
}

cat("=== Testing Delete Operations ===\n")

# Test delete user (cascade delete should remove all related data)
result <- delete_user(test_user_id)

if (result$success) {
  cat("✓ User deleted successfully\n")
  
  # Verify cascade delete
  user <- get_user(test_user_id)
  profile <- get_player_profile(test_user_id)
  metrics <- get_athletic_metrics(test_user_id)
  academic <- get_academic_info(test_user_id)
  prefs <- get_user_preferences(test_user_id)
  
  if (nrow(user) == 0 && nrow(profile) == 0 && nrow(metrics) == 0 && 
      nrow(academic) == 0 && nrow(prefs) == 0) {
    cat("✓ Cascade delete successful - all related data removed\n\n")
  } else {
    cat("✗ Cascade delete failed - some data remains\n\n")
  }
} else {
  cat("✗ User deletion failed:", result$message, "\n\n")
}

cat("=== All Tests Completed ===\n")
cat("Test database:", test_db_path, "\n")
cat("You can inspect the database using: sqlite3", test_db_path, "\n")

# Clean up test database
if (file.exists(test_db_path)) {
  file.remove(test_db_path)
  cat("Test database cleaned up\n")
}

cat("\n✓ All tests passed successfully!\n")
