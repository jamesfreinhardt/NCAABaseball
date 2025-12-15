# Quick Start Guide - Player Registration System

This guide will help you get started with the new player registration system for the NCAA Baseball recruiting application.

## 1. Installation

### Install Required R Packages

Open R or RStudio and run:

```r
# Install packages if not already installed
install.packages(c("shiny", "DBI", "RSQLite", "dplyr", "leaflet", 
                   "tidyr", "DT", "shinyjs", "geosphere", "zipcodeR", 
                   "plotly", "bslib", "shinyWidgets", "scales"))
```

## 2. Running the Application

### Start the Shiny App

```r
# Navigate to the project directory and run
shiny::runApp()
```

The app will automatically:
- Initialize the SQLite database if it doesn't exist
- Create all necessary tables
- Load all existing data

## 3. Using the Registration System

### For Players

1. **Open the app** in your web browser (usually opens automatically)

2. **Navigate to "Player Registration"** tab in the main navigation

3. **Fill out your profile:**
   - Personal Information (name, email, phone, location)
   - Player Profile (high school, graduation year, position, height, weight)
   - Academic Information (GPA, SAT/ACT scores)
   - Athletic Metrics (batting stats, pitching stats, speed)
   - College Preferences (divisions, distance, budget)

4. **Click "Save Profile"** to store your information

5. **To update later:** Click "Load Existing Profile", enter your email, and your data will populate the form

### For Administrators

#### Testing the System

Run the automated test suite to verify everything works:

```r
source("test_registration.R")
```

#### Viewing Player Data

Use the example script to see how to query player data:

```r
source("example_usage.R")
```

#### Bulk Import Players

1. Create a CSV template:
```r
source("import_export.R")
create_import_template("players.csv")
```

2. Edit `players.csv` with your player data

3. Import the data:
```r
import_players_from_csv("players.csv")
```

#### Export/Backup Player Data

```r
source("import_export.R")
export_players_to_csv("player_backup.csv")
```

## 4. Common Tasks

### View All Registered Players

```r
source("db_functions.R")
library(DBI)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "ncaa_baseball.db")
players <- dbGetQuery(con, "
  SELECT u.first_name, u.last_name, u.email, 
         p.graduation_year, p.primary_position
  FROM users u
  LEFT JOIN player_profile p ON u.user_id = p.user_id
  ORDER BY u.last_name
")
dbDisconnect(con)
print(players)
```

### Search for Specific Players

```r
# Find all Division 1 prospects
source("db_functions.R")
con <- dbConnect(RSQLite::SQLite(), "ncaa_baseball.db")
d1_prospects <- dbGetQuery(con, "
  SELECT u.first_name, u.last_name, u.email, pr.preferred_divisions
  FROM users u
  JOIN user_preferences pr ON u.user_id = pr.user_id
  WHERE pr.preferred_divisions LIKE '%1%'
")
dbDisconnect(con)
print(d1_prospects)
```

### Get Complete Profile for a Player

```r
source("db_functions.R")
# Get player by email
user <- get_user_by_email("player@example.com")
if (nrow(user) > 0) {
  profile <- get_complete_profile(user$user_id[1])
  print(profile)
}
```

## 5. Integration with College Search

The registration system automatically integrates with the college search:

- When a player saves their zip code, it updates the home location on the map
- Player preferences can be used to pre-filter college searches
- Academic and athletic metrics help identify appropriate programs

## 6. Database Location

The player database is stored in:
```
ncaa_baseball.db
```

This file is automatically created in the same directory as `app.R` and is excluded from version control to protect player privacy.

## 7. Troubleshooting

### "Database not found" error
**Solution:** The database will be created automatically when you first run the app. If you see this error, just restart the app.

### "Package not installed" error
**Solution:** Run the installation command from step 1 to install missing packages.

### Profile won't load
**Solution:** 
- Verify you're using the correct email address
- Check that a profile was previously saved
- Try saving a new profile first

### Want to reset the database
**Solution:** Delete `ncaa_baseball.db` file and restart the app. A fresh database will be created.

## 8. Security Notes

- The database file is excluded from Git to protect player privacy
- Email addresses are unique - one profile per email
- All data stays local unless you explicitly export it
- No passwords are stored (authentication is not implemented)

## 9. Next Steps

After setting up the registration system, you can:

1. **Customize the form** - Edit `app.R` to add or remove fields
2. **Add authentication** - Implement user login functionality
3. **Create coach portal** - Allow coaches to search registered players
4. **Add document upload** - Let players upload videos or transcripts
5. **Email notifications** - Send confirmation emails when profiles are created

## 10. Getting Help

For detailed information, see:
- `REGISTRATION.md` - Complete documentation
- `db_setup.R` - Database schema and table definitions
- `db_functions.R` - API documentation for all functions
- `example_usage.R` - Code examples

## Summary of Files

| File | Purpose |
|------|---------|
| `app.R` | Main application with registration UI |
| `db_setup.R` | Database initialization |
| `db_functions.R` | Database operations (create, read, update, delete) |
| `test_registration.R` | Automated test suite |
| `example_usage.R` | Usage examples and demonstrations |
| `import_export.R` | CSV import/export utilities |
| `REGISTRATION.md` | Complete documentation |
| `QUICKSTART.md` | This quick start guide |

---

**Ready to get started?** Just run `shiny::runApp()` and navigate to the Player Registration tab!
