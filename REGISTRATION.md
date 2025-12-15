# Player Registration System

This document describes the new player registration system added to the NCAA Baseball recruiting application.

## Overview

The registration system allows baseball players to create profiles with their personal, academic, and athletic information. This data is stored in a SQLite database and can be used to help match players with appropriate college baseball programs.

## Features

### 1. Database Schema

The system uses SQLite with the following tables:

- **users**: Basic account information (email, name, contact info)
- **player_profile**: Athletic profile (height, weight, positions, bats/throws)
- **athletic_metrics**: Performance statistics (batting avg, ERA, velocity, etc.)
- **academic_info**: Academic credentials (GPA, SAT/ACT scores, class rank)
- **user_preferences**: College search preferences (divisions, distance, tuition)

### 2. Registration Form

The registration page includes sections for:

#### Personal Information
- First Name, Last Name (required)
- Email Address (required, used as unique identifier)
- Phone Number
- Zip Code and State

#### Player Profile
- High School name
- Graduation Year
- Height (inches) and Weight (lbs)
- Primary and Secondary Positions
- Batting and Throwing preferences (Right/Left/Switch)

#### Academic Information
- GPA (4.0 scale) and Weighted GPA
- SAT Score (400-1600)
- ACT Score (1-36)
- Class Rank and Class Size

#### Athletic Metrics

**Hitting Statistics:**
- Batting Average
- On-Base Percentage
- Slugging Percentage
- Home Runs, RBI, Stolen Bases
- Exit Velocity (mph)

**Pitching Statistics:**
- ERA (Earned Run Average)
- Strikeouts, Walks
- Innings Pitched
- Wins, Saves
- Fastball Velocity (mph)

**Speed & Athleticism:**
- 60-Yard Dash time (seconds)

#### College Search Preferences
- Preferred Divisions (1, 2, 3)
- Maximum Distance from Home (miles)
- Maximum Tuition budget
- Preferred School Settings (City, Suburb, Rural)

### 3. Data Persistence

- All data is saved to a SQLite database (`ncaa_baseball.db`)
- The database file is excluded from version control via `.gitignore`
- Users can save and update their profiles
- Users can load existing profiles using their email address

## Installation & Setup

### Prerequisites

The following R packages are required:
```r
install.packages(c("shiny", "DBI", "RSQLite", "dplyr", "leaflet", 
                   "tidyr", "DT", "shinyjs", "geosphere", "zipcodeR", 
                   "plotly", "bslib", "shinyWidgets", "scales"))
```

### Database Initialization

The database is automatically initialized when the Shiny app starts. However, you can manually initialize it by running:

```r
source("db_setup.R")
init_database()
```

This creates the database file and all necessary tables.

## Usage

### Running the Application

1. Start the Shiny application:
```r
shiny::runApp()
```

2. Navigate to the "Player Registration" tab in the application

3. Fill out the registration form with your information

4. Click "Save Profile" to save your data

### Loading an Existing Profile

1. Click the "Load Existing Profile" button
2. Enter your email address in the dialog
3. Click "Load Profile"
4. Your saved information will populate the form

### Integration with College Search

When you save your registration:
- Your home zip code is automatically applied to the college search map
- Your preferences can be used to filter college search results
- College coaches can view your profile information (future enhancement)

## Database Functions

### User Operations
- `create_user()`: Create a new user account
- `get_user()`: Retrieve user by ID
- `get_user_by_email()`: Retrieve user by email
- `update_user()`: Update user information

### Profile Operations
- `save_player_profile()`: Save/update player profile
- `get_player_profile()`: Retrieve player profile

### Metrics Operations
- `save_athletic_metrics()`: Save/update athletic statistics
- `get_athletic_metrics()`: Retrieve athletic statistics

### Academic Operations
- `save_academic_info()`: Save/update academic information
- `get_academic_info()`: Retrieve academic information

### Preferences Operations
- `save_user_preferences()`: Save/update search preferences
- `get_user_preferences()`: Retrieve search preferences

### Utility Functions
- `get_complete_profile()`: Get all user data in one call
- `delete_user()`: Delete user and all related data
- `check_database()`: Verify database is properly initialized

## Data Validation

The system includes validation for:
- Required fields (First Name, Last Name, Email)
- Email format validation
- Numeric range validation for test scores, measurements, and statistics
- Unique email addresses (no duplicates)

## Security Considerations

1. **Database file is excluded from version control** to protect user privacy
2. **Input sanitization** is handled by R's parameterized queries
3. **No passwords are stored** (authentication is not implemented in this version)
4. **SQL injection protection** via DBI parameterized queries

## Future Enhancements

Potential improvements for the registration system:

1. **User Authentication**: Add password-based login system
2. **Email Verification**: Verify email addresses before activation
3. **Document Upload**: Allow players to upload videos, stats sheets, or transcripts
4. **Coach Portal**: Allow coaches to search and contact registered players
5. **Profile Visibility Controls**: Let players control who can see their profile
6. **Recruiting Timeline**: Track communication and offers from colleges
7. **Profile Completeness**: Show progress bar for profile completion
8. **Export Functionality**: Export profile as PDF for sharing with coaches

## File Structure

```
NCAABaseball/
├── app.R                    # Main Shiny application (modified)
├── db_setup.R              # Database initialization script (new)
├── db_functions.R          # Database CRUD operations (new)
├── test_registration.R     # Automated test suite (new)
├── example_usage.R         # Example code demonstrating API usage (new)
├── import_export.R         # CSV import/export utilities (new)
├── REGISTRATION.md         # This documentation file (new)
├── .gitignore              # Updated to exclude database files
└── ncaa_baseball.db        # SQLite database (created at runtime)
```

## Testing

### Running Automated Tests

To verify the registration system is working correctly, run:

```r
source("test_registration.R")
```

This will:
- Create a test database
- Test all CRUD operations
- Verify data integrity
- Test cascade deletes
- Clean up test data

### Running Examples

To see the registration system in action:

```r
source("example_usage.R")
```

This will:
- Create sample player profiles
- Demonstrate retrieval operations
- Show update operations
- Demonstrate search queries

## Data Import/Export

### Creating a CSV Template

To create a template for bulk importing players:

```r
source("import_export.R")
create_import_template("players_template.csv")
```

### Importing Players from CSV

To import player data from a CSV file:

```r
source("import_export.R")
import_players_from_csv("my_players.csv")
```

The CSV file should include columns for:
- **Required**: email, first_name, last_name
- **Optional**: All other fields (phone, zip_code, high_school, gpa, batting_avg, etc.)

### Exporting Players to CSV

To export all player data to a CSV file:

```r
source("import_export.R")
export_players_to_csv("player_backup.csv")
```

This creates a complete backup of all player data that can be imported later.


## Troubleshooting

### Database errors
If you encounter database errors, try:
1. Delete the `ncaa_baseball.db` file
2. Restart the Shiny app (database will be recreated)

### Missing packages
If the app fails to start:
1. Check that all required packages are installed
2. Run: `install.packages(c("DBI", "RSQLite"))`

### Profile not loading
If your profile doesn't load:
1. Verify you're using the correct email address
2. Check that the database file exists
3. Try creating a new profile

## Support

For issues or questions about the registration system, please refer to the code comments in:
- `db_setup.R` - Database schema and initialization
- `db_functions.R` - CRUD operations and data management
- `app.R` - UI and server logic for registration

## Data Schema Details

### Users Table
```sql
CREATE TABLE users (
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
```

### Player Profile Table
```sql
CREATE TABLE player_profile (
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
```

### Athletic Metrics Table
```sql
CREATE TABLE athletic_metrics (
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
```

### Academic Info Table
```sql
CREATE TABLE academic_info (
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
```

### User Preferences Table
```sql
CREATE TABLE user_preferences (
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
```
