# Got it ✅ — you want to download climate data in R and then aggregate it by U.S. ZIP code to get average temperature and precipitation.
# This requires two main steps:
#   
#   Get climate data for each ZIP code’s coordinates.
# Aggregate by ZIP code.
# 
# Since ZIP codes are not weather stations, we’ll:
#   
#   Use a ZIP code → latitude/longitude lookup table.
# Pull climate data for each coordinate (e.g., from NASA POWER via nasapower — no station IDs needed).
# Compute averages.
# 

#Complete R Example
# Install required packages
packages <- c("tidyverse", "zipcodeR", "nasapower")
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
lapply(packages, install_if_missing)



library(tidyverse)
library(zipcodeR)
library(nasapower)

Sch_Location <- read.csv("input.csv") %>% select(unitid, latitude, longitude)

# --- NEW: helper to safely pull monthly climatology from NASA POWER ---
get_school_monthly_climatology <- function(unitid, lat, lon, pars = c("T2M", "CLOUD_AMT", "PRECTOTCORR")) {
  # Returns a tibble with columns: unitid, month (1..12), variable (t2m/cloud_amt/prectotcorr), value
  out_template <- tibble(
    unitid = rep(unitid, 36),
    month = rep(1:12, times = 3),
    variable = rep(c("t2m","cloud_amt","prectotcorr"), each = 12),
    value = NA_real_
  )

  # Ensure parameters are ALL-CAPS (nasapower expects parameter codes in upper case)
  pars <- toupper(pars)

  res <- tryCatch(
    {
      get_power(
        community = "AG",
        lonlat = c(lon, lat),
        pars = pars,
        temporal_api = "climatology"
      )
    }, error = function(e) {
      message(sprintf("get_power() failed for unitid=%s lat=%s lon=%s -> %s", unitid, lat, lon, e$message))
      NULL
    }
  )

  if (is.null(res) || nrow(res) == 0) return(out_template)

  # Make a lower-cased copy of column names for case-insensitive matching
  res_lc <- res %>% rename_with(~ tolower(.x))

  # Find the month column (common names: month or month_number)
  month_col <- NULL
  if ("month" %in% names(res)) month_col <- "month"
  else if ("mo" %in% names(res)) month_col <- "mo"
  else if ("month_number" %in% names(res)) month_col <- "month_number"

  if (is.null(month_col)) {
    # If months are not rows, check if columns for JAN..DEC exist for each parameter
    # In that case, try to pivot columns into month rows
    jan_names <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
    res_low <- names(res) %>% tolower()

    # pick parameter column prefix among pars
    pars_low <- tolower(pars)
    # collect matching parameter names that include months
    param_month_cols <- intersect(res_low, unlist(lapply(pars_low, function(p) paste0(p, "_", jan_names))))

    if (length(param_month_cols) > 0) {
      # build res_long by taking columns matching param_month_cols
      df_long <- tibble()
      for (p in pars_low) {
        p_cols <- grep(paste0("^", p, "_"), names(res), ignore.case = TRUE, value = TRUE)
        if (length(p_cols) == 12) {
          # map to months
          vals <- suppressWarnings(as.numeric(res[1, p_cols]))
          # replace NaN/Inf with NA for guards
          vals[is.nan(vals) | is.infinite(vals)] <- NA_real_
          tmp <- tibble(month = 1:12, variable = p, value = vals)
          df_long <- bind_rows(df_long, tmp)
        }
      }

      if (nrow(df_long) == 0) return(out_template)

      df_long <- df_long %>% mutate(unitid = unitid) %>% select(unitid, month, variable, value)
      # normalize variable names to our convention
      df_long <- df_long %>% mutate(variable = case_when(
        variable %in% c("t2m") ~ "t2m",
        variable %in% c("cloud_amt","cloud_amt_mean") ~ "cloud_amt",
        variable %in% c("prectotcorr","prectot") ~ "prectotcorr",
        TRUE ~ variable
      ))

      # reorder and return merged with out_template
      df_long <- df_long %>% arrange(variable, month)
      return(df_long)
    }

    # --- NEW: check for responses that have one row per parameter and JAN..DEC month columns
    # Example layout (from preflight): LON, LAT, PARAMETER, JAN, FEB, ..., DEC
    param_col_candidates <- c("parameter", "parm", "par", "PARAMETER", "PARAM")
    param_col <- intersect(tolower(names(res)), tolower(param_col_candidates))
    month_cols <- intersect(tolower(names(res)), jan_names)
    if (length(param_col) == 1 && length(month_cols) == 12) {
      # pivot each row (parameter) into 12 month rows
      param_col_name <- names(res)[which(tolower(names(res)) == param_col)]
      # Ensure month columns are ordered Jan..Dec
      month_names_full <- vapply(jan_names, function(j) {
        matches <- which(tolower(names(res)) == j)
        if (length(matches) > 0) names(res)[matches[1]] else NA_character_
      }, character(1), USE.NAMES = FALSE)
      month_names_full <- month_names_full[!is.na(month_names_full)]

      df_long <- tibble()
      for (r in seq_len(nrow(res))) {
        param_name <- as.character(res[[param_col_name]][r])
        var_norm <- case_when(
          grepl("T2M", param_name, ignore.case = TRUE) ~ "t2m",
          grepl("CLOUD", param_name, ignore.case = TRUE) ~ "cloud_amt",
          grepl("PRECTOT|PRECTOTCORR|PRECIP", param_name, ignore.case = TRUE) ~ "prectotcorr",
          TRUE ~ tolower(gsub("\\s+", "_", param_name))
        )

        vals <- as.numeric(res[r, month_names_full])
        vals[is.nan(vals) | is.infinite(vals)] <- NA_real_

        tmp <- tibble(month = 1:12, variable = var_norm, value = vals)
        df_long <- bind_rows(df_long, tmp)
      }

      df_long <- df_long %>% mutate(unitid = unitid) %>% select(unitid, month, variable, value)
      df_long <- df_long %>% arrange(variable, month)

      return(df_long)
    }

    # If still nothing, return template
    return(out_template)
  }

  # If we have month rows, attempt to select our parameters
  # Use the lower-case copy for safe selection
  res_lc <- res_lc %>% mutate(month = as.integer(.data[[month_col]]))

  # try several possible param names in returned table
  param_map <- list(
    t2m = c("t2m", "t_2m", "t2m_mean"),
    cloud_amt = c("cloud_amt", "cloud_amt_mean", "cc"),
    prectotcorr = c("prectotcorr", "prectot", "precip")
  )

  df_long <- tibble()
  for (p in names(param_map)) {
    # search case-insensitive using the lower-cased copy
    found <- intersect(names(res_lc), param_map[[p]])
    if (length(found) > 0) {
      colname <- found[1]
      # pull from the lower-cased copy, coerce to numeric and scrub NaN/Inf
      tmp <- res_lc %>% select(month, value = !!sym(colname)) %>% mutate(value = suppressWarnings(as.numeric(value)), variable = p)
      tmp$value[is.nan(tmp$value) | is.infinite(tmp$value)] <- NA_real_
      df_long <- bind_rows(df_long, tmp)
    } else {
      # add NA column if not found
      df_long <- bind_rows(df_long, tibble(month = 1:12, value = NA_real_, variable = p))
    }
  }

  df_long <- df_long %>% mutate(unitid = unitid) %>% select(unitid, month, variable, value)

  # Ensure ordering: t2m, cloud_amt, prectotcorr (each 1..12)
  df_long <- df_long %>% arrange(variable, month)

  return(df_long)
}


# Function to get NASA POWER daily climate data for a ZIP code
get_zip_climate <- function(unitid, lat, lon) {
  tryCatch({
    data <- get_power(
      community = "AG",
      lonlat = c(lon, lat),
      pars = c("T2M", "CLOUD_AMT","PRECTOTCORR"),
      temporal_api = "climatology")
      
    
    data %>% cbind(unitid)
      
  }, error = function(e) {
    message(sprintf("Failed for ZIP %s: %s", zip, e$message))
    # Return empty tibble with correct columns
    tibble(zipcode = zip,  T2M = numeric(), CLOUD_AMT = numeric(), PRECTOTCORR = numeric())
  })
}



# Save results
## === NEW: Main pipeline ===

# Load school locations and remove rows with missing lat/lon
Sch_Location <- read.csv("input.csv") %>%
  select(unitid, latitude, longitude) %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Use purrr to map across schools and fetch climatologies
library(purrr)

## -------- TEST MODE --------
# Set test_first_n > 0 to only process the first N unitids for quick tests.
# Set test_first_n <- 0 to run ALL unitids.
test_first_n <- 0

to_fetch <- if (test_first_n > 0) Sch_Location %>% slice_head(n = test_first_n) else Sch_Location

message("Fetching climatologies for ", nrow(to_fetch), " schools. This may take a while depending on API limits.")

# --- PRE-FLIGHT: test the API for a single lat/lon and print a short summary so we can adapt parsing ---
if (nrow(to_fetch) > 0) {
  sample <- to_fetch %>% slice_head(n = 1)
  message("Running preflight get_power() for sample unitid=", sample$unitid)
  test_res <- tryCatch(
    {
      get_power(
        community = "AG",
        lonlat = c(sample$longitude, sample$latitude),
        pars = toupper(c("T2M","CLOUD_AMT","PRECTOTCORR")),
        temporal_api = "climatology"
      )
    }, error = function(e) {
      message("Preflight get_power failed: ", e$message)
      NULL
    }
  )

  if (!is.null(test_res)) {
    message("Preflight returned columns: ", paste(head(names(test_res), 30), collapse = ", "))
    if (nrow(test_res) > 0) {
      message("Preview of first 3 rows (showing first 6 columns):")
      print(utils::head(test_res[, seq_len(min(6, ncol(test_res)))], 3))
    }
  }
}

# I: fetch per school -> long format rows (unitid, month, variable, value)
clim_long_all <- to_fetch %>%
  pmap_dfr(function(unitid, latitude, longitude) {
    # small pause to be kind to the API / avoid rate limits
    Sys.sleep(0.15)
    get_school_monthly_climatology(unitid, latitude, longitude)
  })

# II: compute quartile rank by variable & month across all unitid's
clim_long_all <- clim_long_all %>%
  group_by(variable, month) %>%
  mutate(month_quartile = if_else(is.na(value), NA_integer_, ntile(value, 4))) %>%
  ungroup()

# III: compute annual mean for each unitid/variable and then overall annual quartile
annual_summary <- clim_long_all %>%
  group_by(unitid, variable) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(annual_quartile = if_else(is.na(mean_value), NA_integer_, ntile(mean_value, 4))) %>%
  ungroup()

# IV: pivot to the requested wide format: 3 rows per unitid (variables) and month columns
month_labels <- month.abb

clim_wide_values <- clim_long_all %>%
  mutate(month_label = month_labels[month]) %>%
  select(unitid, variable, month_label, value) %>%
  pivot_wider(names_from = month_label, values_from = value) %>%
  arrange(unitid, variable)

clim_wide_quart <- clim_long_all %>%
  mutate(month_label = month_labels[month]) %>%
  select(unitid, variable, month_label, month_quartile) %>%
  pivot_wider(names_from = month_label, values_from = month_quartile, names_prefix = "Q_") %>%
  arrange(unitid, variable)

# join annual_summary
clim_final <- clim_wide_values %>%
  left_join(clim_wide_quart, by = c("unitid", "variable")) %>%
  left_join(annual_summary %>% select(unitid, variable, mean_value, annual_quartile), by = c("unitid","variable"))

# Convert T2M (°C) to °F inside wide output (month cols and mean_value) so CSVs use °F
month_labels <- month.abb
month_cols <- intersect(names(clim_final), month_labels)
if (any(tolower(clim_final$variable) == "t2m")) {
  t2m_idx <- which(tolower(clim_final$variable) == "t2m")
  if (length(month_cols) > 0) {
    clim_final[t2m_idx, month_cols] <- lapply(clim_final[t2m_idx, month_cols], function(col) {
      as.numeric(col) * 9/5 + 32
    })
  }
  # convert mean_value for t2m
  if ("mean_value" %in% names(clim_final)) {
    clim_final$mean_value[t2m_idx] <- as.numeric(clim_final$mean_value[t2m_idx]) * 9/5 + 32
  }
}

# Save final CSV
suffix <- if (exists("test_first_n") && test_first_n > 0) paste0("_first", test_first_n) else ""
out_file <- paste0("school_climatology_monthly_wide", suffix, ".csv")
# Ensure no list-columns before saving (write.csv can't encode lists)
clim_final <- clim_final %>% mutate(across(where(is.list), ~ sapply(., function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.atomic(x)) return(paste(as.character(x), collapse = ";"))
  # fallback: deparse complex objects
  paste(deparse(x), collapse = ";")
})))
write.csv(clim_final, out_file, row.names = FALSE, fileEncoding = "UTF-8")
message("Note: T2M (temperature) converted from °C to °F in CSV output.")
message("Saved climatology wide table to: ", out_file)

head(clim_final)

# === NEW: long-format output with per-month quartiles and annual summaries attached ===
clim_long_final <- clim_long_all %>%
  left_join(annual_summary %>% select(unitid, variable, mean_value, annual_quartile), by = c("unitid", "variable")) %>%
  # ensure months are ordered
  arrange(unitid, variable, month)

# Convert t2m values (°C -> °F) in long-form output
clim_long_final <- clim_long_final %>%
  mutate(value = ifelse(tolower(variable) == "t2m", as.numeric(value) * 9/5 + 32, as.numeric(value)))

# Save long-form CSV
out_long <- paste0("school_climatology_monthly_long", suffix, ".csv")
clim_long_final <- clim_long_final %>% mutate(across(where(is.list), ~ sapply(., function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.atomic(x)) return(paste(as.character(x), collapse = ";"))
  paste(deparse(x), collapse = ";")
})))
write.csv(clim_long_final, out_long, row.names = FALSE, fileEncoding = "UTF-8")
message("Saved climatology long table to: ", out_long)
message("Note: T2M (temperature) values in long output are in °F (converted from °C).")

# Show a preview
head(clim_long_final)


# How It Works
# 
# zipcodeR: Gets ZIP code coordinates.
# nasapower: Downloads daily temperature (T2M) and precipitation (PRECTOT) for given coordinates.
# Loop + pmap: Iterates over ZIP codes to fetch data.
# Aggregation: Computes mean temperature and precipitation per ZIP.
# 
# 
# Performance Notes
# 
# The U.S. has ~42,000 ZIP codes — fetching all will take hours and may hit API limits.
# For nationwide data, it’s better to:
#   
#   Download gridded climate data (e.g., TerraClimate via climateR).
# Spatially join ZIP code polygons to the grid for faster processing.
