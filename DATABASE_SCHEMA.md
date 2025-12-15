# Database Schema Diagram

## Entity Relationship Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                              DATABASE SCHEMA                             │
│                           ncaa_baseball.db                               │
└─────────────────────────────────────────────────────────────────────────┘


┌──────────────────────────┐
│         USERS            │  Primary table for user accounts
├──────────────────────────┤
│ • user_id (PK)          │
│ • email (UNIQUE)        │
│ • first_name            │
│ • last_name             │
│ • phone                 │
│ • zip_code              │
│ • state                 │
│ • created_at            │
│ • updated_at            │
└──────────┬───────────────┘
           │
           │ (One user has...)
           │
    ┌──────┴────────┬─────────────┬──────────────┬─────────────┐
    │               │             │              │             │
    │               │             │              │             │
    ▼               ▼             ▼              ▼             ▼
┌────────────┐ ┌─────────┐ ┌──────────┐ ┌──────────┐ ┌────────────┐
│  PLAYER    │ │ ATHLETIC│ │ ACADEMIC │ │   USER   │ │  (Future)  │
│  PROFILE   │ │ METRICS │ │   INFO   │ │  PREFS   │ │  VIDEOS/   │
├────────────┤ ├─────────┤ ├──────────┤ ├──────────┤ │   DOCS     │
│•profile_id │ │•metric_ │ │•academic │ │•pref_id  │ └────────────┘
│ (PK)       │ │ id (PK) │ │ _id (PK) │ │ (PK)     │
│•user_id    │ │•user_id │ │•user_id  │ │•user_id  │
│ (FK)       │ │ (FK)    │ │ (FK)     │ │ (FK)     │
├────────────┤ ├─────────┤ ├──────────┤ ├──────────┤
│ Athletic   │ │Hitting  │ │Education │ │Search    │
│ Profile    │ │Stats    │ │Metrics   │ │Criteria  │
│            │ │         │ │          │ │          │
│•high_      │ │•batting │ │•gpa      │ │•preferred│
│ school     │ │ _avg    │ │•weighted │ │ _divisions│
│•graduation │ │•on_base │ │ _gpa     │ │•max_dist │
│ _year      │ │ _pct    │ │•sat_score│ │ _miles   │
│•height_    │ │•slugging│ │•act_score│ │•min_enr  │
│ inches     │ │ _pct    │ │•class_   │ │•max_enr  │
│•weight_lbs │ │•home_   │ │ rank     │ │•min_acc  │
│•primary_   │ │ runs    │ │•class_   │ │ _rate    │
│ position   │ │•rbi     │ │ size     │ │•max_acc  │
│•secondary_ │ │•stolen_ │ │          │ │ _rate    │
│ position   │ │ bases   │ │          │ │•max_     │
│•bats       │ │•exit_   │ │          │ │ tuition  │
│•throws     │ │ velocity│ │          │ │•preferred│
│            │ │         │ │          │ │ _climate │
│            │ │Pitching │ │          │ │•preferred│
│            │ │Stats    │ │          │ │ _locale  │
│            │ │         │ │          │ │          │
│            │ │•era     │ │          │ │          │
│            │ │•strike  │ │          │ │          │
│            │ │ outs    │ │          │ │          │
│            │ │•walks   │ │          │ │          │
│            │ │•innings │ │          │ │          │
│            │ │ _pitched│ │          │ │          │
│            │ │•wins    │ │          │ │          │
│            │ │•saves   │ │          │ │          │
│            │ │•fastball│ │          │ │          │
│            │ │ _velo   │ │          │ │          │
│            │ │         │ │          │ │          │
│            │ │Speed    │ │          │ │          │
│            │ │         │ │          │ │          │
│            │ │•60_yard │ │          │ │          │
│            │ │ _dash   │ │          │ │          │
└────────────┘ └─────────┘ └──────────┘ └──────────┘


RELATIONSHIPS:
─────────────

users → player_profile     (1:1)  One user has one player profile
users → athletic_metrics   (1:1)  One user has one set of metrics
users → academic_info      (1:1)  One user has one academic record
users → user_preferences   (1:1)  One user has one preference set

All relationships use CASCADE DELETE:
- Deleting a user automatically deletes all related records


INDEXES:
────────

- users.email (UNIQUE) - Fast lookup by email
- player_profile.user_id - Fast join to users
- athletic_metrics.user_id - Fast join to users
- academic_info.user_id - Fast join to users
- user_preferences.user_id - Fast join to users


DATA FLOW:
──────────

1. USER REGISTRATION
   ↓
2. Create USERS record (required)
   ↓
3. Create related records (optional):
   - PLAYER_PROFILE (physical attributes, positions)
   - ACADEMIC_INFO (grades, test scores)
   - ATHLETIC_METRICS (performance stats)
   - USER_PREFERENCES (search criteria)
   ↓
4. Data available for:
   - Profile display in app
   - College search filtering
   - Coach recruiting searches
   - Export/reporting


SAMPLE QUERY PATTERNS:
──────────────────────

Get complete profile:
  SELECT * FROM users u
  LEFT JOIN player_profile p ON u.user_id = p.user_id
  LEFT JOIN academic_info a ON u.user_id = a.user_id
  LEFT JOIN athletic_metrics m ON u.user_id = m.user_id
  LEFT JOIN user_preferences pr ON u.user_id = pr.user_id
  WHERE u.email = 'player@example.com'

Find pitchers with good stats:
  SELECT u.first_name, u.last_name, m.era, m.strikeouts
  FROM users u
  JOIN player_profile p ON u.user_id = p.user_id
  JOIN athletic_metrics m ON u.user_id = m.user_id
  WHERE p.primary_position = 'Pitcher'
    AND m.era < 3.0
    AND m.strikeouts > 50
  ORDER BY m.era

Find players by academic criteria:
  SELECT u.first_name, u.last_name, a.gpa, a.sat_score
  FROM users u
  JOIN academic_info a ON u.user_id = a.user_id
  WHERE a.gpa >= 3.5
    AND a.sat_score >= 1300
  ORDER BY a.gpa DESC


FUTURE ENHANCEMENTS:
────────────────────

Potential additional tables:

┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│   VIDEOS     │     │  TRANSCRIPTS │     │   COACHES    │
├──────────────┤     ├──────────────┤     ├──────────────┤
│•video_id     │     │•transcript_id│     │•coach_id     │
│•user_id (FK) │     │•user_id (FK) │     │•school_id    │
│•url          │     │•file_path    │     │•name         │
│•type         │     │•upload_date  │     │•email        │
│•upload_date  │     │•verified     │     │•phone        │
└──────────────┘     └──────────────┘     └──────────────┘

┌──────────────┐     ┌──────────────┐
│ COMMUNICATIONS│     │   OFFERS     │
├──────────────┤     ├──────────────┤
│•comm_id      │     │•offer_id     │
│•user_id (FK) │     │•user_id (FK) │
│•coach_id(FK) │     │•school_id    │
│•date         │     │•offer_type   │
│•message      │     │•offer_date   │
│•status       │     │•status       │
└──────────────┘     └──────────────┘
