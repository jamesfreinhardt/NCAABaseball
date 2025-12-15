# Additional Metrics for Baseball Recruits

This document describes the additional metrics that have been added to help recruits evaluate NCAA baseball programs.

## New Metrics Overview

### 1. Team Success Trajectory
**Purpose**: Shows whether a team's performance is improving, declining, or staying stable over time.

**Calculation**: 
- Uses win percentage data from the last 4 years
- Performs linear regression to calculate the trend slope
- Categorizes as:
  - **Improving** (▲): Slope > 0.02 (Green)
  - **Declining** (▼): Slope < -0.02 (Red)  
  - **Stable** (■): Slope between -0.02 and 0.02 (Gray)

**Why it matters**: Recruits can see if a program is on the rise (good time to join) or in decline. An improving team may offer more opportunities as the program builds momentum.

**Visualization**: Indicator gauge showing current vs. previous win percentage with delta indicator

---

### 2. In-State Recruiting Percentage
**Purpose**: Shows what percentage of the current roster is from the school's home state.

**Calculation**:
- Counts players on current roster from school's home state
- Divides by total roster size
- Displays as percentage

**Why it matters**: 
- High in-state % may indicate strong local recruiting ties
- Low in-state % suggests national recruiting reach
- Helps recruits understand if being from the home state gives an advantage
- Useful for understanding travel costs and family visit feasibility

**Visualization**: Gauge chart showing percentage from 0-100%

---

### 3. Freshman Playing Time
**Purpose**: Shows average games played by each class year, highlighting opportunities for freshmen.

**Calculation**:
- Analyzes roster data from last 4 years
- Calculates average games played for Fr., So., Jr., Sr. classes
- Displays comparative bar chart

**Why it matters**:
- Critical for recruits who want to know if freshmen get playing time
- Low freshman numbers may indicate limited opportunities for newcomers
- High freshman numbers suggest the program gives younger players chances
- Helps set realistic expectations for first year

**Visualization**: Bar chart comparing average games played across all four class years

---

### 4. Roster Depth by Position
**Purpose**: Shows current number of players at each position.

**Calculation**:
- Categorizes players into position groups: P, C, 1B, 2B, 3B, SS, OF, DH
- Counts current roster players at each position
- Displays distribution

**Why it matters**:
- Shows competition level at recruit's position
- 15+ pitchers = high competition for mound time
- 2-3 catchers = potential opportunity
- Helps recruits target programs with openings at their position
- Indicates where program may need to recruit

**Visualization**: Bar chart showing player count by position, sorted by total count

---

### 5. Coach Win Percentage at Current School
**Purpose**: Separates the coach's overall career record from their record specifically at the current school.

**Calculation**:
- Career Record: Total wins/losses across all coaching positions
- At School: Wins/losses only at the current institution
- Both displayed with win percentages

**Why it matters**:
- A coach may have a great overall record but be new to the current school
- Shows if coach has found success at THIS program or is still building
- Recent success at current school can indicate program fit and trajectory
- Helps distinguish between established vs. building programs

**Visualization**: Text display showing both career and current school records with percentages

---

## Existing Metrics (Context)

These new metrics complement the existing metrics:

- **Current Roster Size & Heights**: Basic roster composition
- **Class Breakdown**: Distribution of Fr/So/Jr/Sr
- **Top 3 Recruiting States**: Geographic recruiting patterns
- **Win % Last 4 Years**: Historical performance trend line
- **Recruiting Geography (4 Years)**: State-by-state recruiting over time
- **Freshman Retention Rate**: Percentage of freshmen who return the following year

## How Recruits Should Use These Metrics

### For Position Players:
1. Check **Roster Depth** at your position - look for opportunities
2. Review **Freshman Playing Time** - will you get on the field?
3. Examine **Team Trajectory** - joining an improving team is exciting
4. Consider **In-State %** - does geographic diversity or local ties matter to you?

### For Pitchers:
1. Check **Roster Depth** for pitchers - 20+ pitchers = tough competition
2. Review **Freshman Playing Time** - do freshman pitchers get innings?
3. Look at **Coach Record at School** - is the pitching coach/head coach winning here?
4. Consider **Team Trajectory** - improving team may need pitching help

### For All Recruits:
- **High Retention + Good Playing Time** = players are happy and getting opportunities
- **Improving Trajectory + Coach Success at School** = program on the rise
- **Position Openings + Good Trajectory** = potential for immediate impact
- **Strong In-State % (if you're from that state)** = may indicate recruiting preference

## Technical Implementation

All metrics are calculated dynamically in the Shiny app using:
- `combined_ncaa_records.csv`: Historical win/loss data
- `combined_ncaa_rosters.csv`: Player-level roster data with games played
- `ncaa_team_history_updated2026.csv`: Coach information
- `input.csv`: School demographic and current season data

Metrics are displayed in the "Roster Metrics" tab when schools are added to the "Saved List".
