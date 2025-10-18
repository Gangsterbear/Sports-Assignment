# Midterm: Centers 3PT Evolution Analysis (R)
# Filename: midterm_centers_3pt_analysis.R
# Purpose: Pull season-level NBA player stats via nba_api (Python) using reticulate,
#          filter for centers, compute descriptive statistics, produce visualizations,
#          and save a dataset for submission.
# Version: 1.0.1
# Date: 2025-10-18

# ---------- User parameters ----------
use_local_cache <- FALSE       # If TRUE, script will try to load data/data_cached_centers.csv and skip API fetching
install_python_pkgs <- FALSE   # If TRUE, uses reticulate::py_install to install nba_api and pandas (may require a writable Python environment)
start_year <- 1999             # first season starting year (e.g., 1999 -> "1999-00")
end_year <- as.integer(format(Sys.Date(), "%Y"))  # last season starting year (will create "YYYY-(YY+1)")
save_dir <- "data"
raw_dir <- file.path(save_dir, "raw")

# Quick demo override to fetch only a few seasons (faster for presentation)
quick_demo <- TRUE
demo_start_year <- 2018
demo_end_year   <- 2020

# A safe switch to force using a single cached canonical CSV if present
use_cached_canonical <- FALSE

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- Packages ----------
required_cran <- c("reticulate", "dplyr", "readr", "ggplot2", "lubridate", "tidyr", "skimr", "scales", "corrplot")
new_pkgs <- required_cran[!(required_cran %in% installed.packages()[, "Package"])]
if(length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")

library(reticulate)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(skimr)
library(scales)
library(corrplot)

# ---------- Optionally install Python packages ----------
if (install_python_pkgs) {
  # This will install into reticulate default python environment; set use_python() beforehand if needed
  py_install(c("nba_api", "pandas", "requests"), pip = TRUE)
}

# ---------- Reticulate config ----------
# Optionally select a specific python binary with use_python("/usr/bin/python3") before py_config()
py_config()  # prints info to console; helpful in the live walkthrough

# ---------- Helper functions ----------
season_str <- function(y) {
  paste0(y, "-", sprintf("%02d", (y + 1) %% 100))
}

safe_save_rds <- function(object, path) {
  tryCatch({
    saveRDS(object, path)
  }, error = function(e) {
    message("Could not save RDS: ", e$message)
  })
}

safe_read_rds <- function(path) {
  tryCatch({
    readRDS(path)
  }, error = function(e) {
    NULL
  })
}

# --------- CHUNK 2: Season helpers ----------
make_season_str <- function(start_year) paste0(start_year, "-", sprintf("%02d", (start_year + 1) %% 100))
season_seq <- function(start_year = 1999, end_year = as.integer(format(Sys.Date(), "%Y"))) {
  yrs <- seq(start_year, end_year)
  sapply(yrs, make_season_str)
}

# Determine seasons_to_run, respecting quick_demo if set
if (exists("quick_demo") && isTRUE(quick_demo)) {
  seasons_to_run <- season_seq(demo_start_year, demo_end_year)
} else {
  seasons_to_run <- season_seq(start_year, end_year)
}
message("Seasons to fetch (preview): ", paste(head(seasons_to_run, 6), collapse = ", "), if (length(seasons_to_run) > 6) " ...")

# ---------- Data acquisition ----------
# If you have a cached CSV for quick demo, set use_local_cache = TRUE and place it at data/data_cached_centers.csv
cached_csv <- file.path(save_dir, "data_cached_centers.csv")
output_csv <- file.path(save_dir, "centers_stats.csv")
output_rds <- file.path(save_dir, "centers_stats.rds")

if (use_local_cache && file.exists(cached_csv)) {
  message("Loading cached CSV:", cached_csv)
  centers_stats <- read_csv(cached_csv, show_col_types = FALSE)
} else {
  # Import the python endpoint module (no automatic conversion so we can use py_to_r explicitly)
  nba_module <- import("nba_api.stats.endpoints.leaguedashplayerstats", convert = FALSE)
  pandas <- import("pandas", convert = FALSE)
  
  # --------- CHUNK 3: robust per-season fetch with retry & caching ----------
  fetch_season_safe <- function(season, raw_dir = raw_dir, nba_module = NULL, max_attempts = 3, pause = 1) {
    raw_file <- file.path(raw_dir, paste0("leaguedashplayerstats_", gsub("-", "", season), ".rds"))
    if (file.exists(raw_file)) {
      message("Loading cached season: ", season)
      return(readRDS(raw_file))
    }
    if (is.null(nba_module)) {
      nba_module <- reticulate::import("nba_api.stats.endpoints.leaguedashplayerstats", convert = FALSE)
    }
    attempt <- 1
    while (attempt <= max_attempts) {
      message(sprintf("Fetching %s (attempt %d/%d)...", season, attempt, max_attempts))
      res <- tryCatch({
        resp <- nba_module$LeagueDashPlayerStats(
          season = season,
          season_type_all_star = "Regular Season",
          per_mode_detailed = "PerGame"
        )
        df_list <- resp$get_data_frames()
        df <- py_to_r(df_list[[1]])
        df$Season <- season
        saveRDS(df, raw_file)
        return(df)
      }, error = function(e) {
        message("Fetch error for ", season, ": ", e$message)
        NULL
      })
      if (!is.null(res)) return(res)
      Sys.sleep(pause * attempt) # backoff
      attempt <- attempt + 1
    }
    warning("All attempts failed for season ", season)
    NULL
  }
  
  # --------- CHUNK 4: batch fetch wrapper (use instead of explicit for-loop) ----------
  batch_fetch_seasons <- function(seasons, raw_dir = raw_dir) {
    dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
    nba_module_local <- reticulate::import("nba_api.stats.endpoints.leaguedashplayerstats", convert = FALSE)
    results <- lapply(seasons, function(s) {
      tryCatch({
        fetch_season_safe(s, raw_dir = raw_dir, nba_module = nba_module_local)
      }, error = function(e) {
        message("Season ", s, " failed: ", e$message); NULL
      })
    })
    results <- results[!sapply(results, is.null)]
    if (length(results) == 0) stop("No seasons fetched successfully.")
    dplyr::bind_rows(results)
  }
  
  # Use seasons_to_run (respects quick_demo). Convert to start-year integers for loop
  seasons <- as.integer(substr(seasons_to_run, 1, 4))
  seasons <- seasons[seasons <= end_year]
  seasons <- seasons[seasons >= 1946] # earliest NBA season if needed
  
  all_seasons_list <- list()
  
  for (y in seasons) {
    s <- season_str(y)
    raw_path <- file.path(raw_dir, paste0("leaguedashplayerstats_", gsub("-", "", s), ".rds"))
    
    # If we have cached per-season data, load it; else fetch via API
    df_season <- safe_read_rds(raw_path)
    if (is.null(df_season)) {
      message("Fetching season ", s, " ...")
      # Use the robust fetch helper
      df_season <- fetch_season_safe(s, raw_dir = raw_dir, nba_module = nba_module)
    } else {
      message("Loaded cached season file: ", raw_path)
    }
    
    if (!is.null(df_season)) {
      # Standardize some column names (make.names) to make downstream robust
      names(df_season) <- make.names(names(df_season))
      all_seasons_list[[s]] <- df_season
    }
  } # end seasons loop
  
  # Combine seasons
  if (length(all_seasons_list) == 0) stop("No season data downloaded. Check network / API access.")
  combined <- bind_rows(all_seasons_list, .id = "SeasonID")
  # Ensure Season column exists and normalize
  if (!"Season" %in% names(combined)) combined$Season <- combined$SeasonID
  
  # Save combined raw table
  centers_stats_raw <- combined
  saveRDS(centers_stats_raw, file.path(save_dir, "centers_stats_raw_all.rds"))
  message("Saved raw combined player-season data to data/centers_stats_raw_all.rds")
  
  # ---------- Identify columns we care about ----------
  # Possible column names (depending on endpoint/per_mode) include:
  # PLAYER_ID, PLAYER_NAME, TEAM_ABBREVIATION, PLAYER_POSITION or GROUP_PLAYER_POSITION or PLAYER_POSITION
  # FG3A, FG3M, FG3_PCT, GP (games played), MIN, PTS, REB, AST
  # Let's inspect and adapt
  df_cols <- names(centers_stats_raw)
  message("Available columns (sample): ", paste(head(df_cols, 30), collapse = ", "))
  
  # We will try to find position column
  pos_candidates <- c("PLAYER_POSITION", "POSITION", "GROUP_PLAYER_POSITION", "PLAYERPOSITION")
  pos_col <- intersect(pos_candidates, df_cols)
  pos_col <- if (length(pos_col) > 0) pos_col[1] else NA
  
  # FG3A & FG3_PCT detection
  fga3_col <- if ("FG3A" %in% df_cols) "FG3A" else if ("FG3A_per_game" %in% df_cols) "FG3A_per_game" else NA
  fg3pct_col <- if ("FG3_PCT" %in% df_cols) "FG3_PCT" else if ("FG3_PCT_per_game" %in% df_cols) "FG3_PCT_per_game" else NA
  gp_col <- if ("GP" %in% df_cols) "GP" else NA
  
  message("Using columns - position:", pos_col, "; FG3A:", fga3_col, "; FG3_PCT:", fg3pct_col, "; GP:", gp_col)
  
  # Filter centers: if position column available use pattern matching; otherwise heuristics
  if (!is.na(pos_col)) {
    centers_all <- centers_stats_raw %>%
      mutate(Position = as.character(.data[[pos_col]])) %>%
      filter(!is.na(Position) & grepl("C", Position)) # keeps C, C-F, F-C etc.
  } else {
    message("No position column detected. Trying fallback: use 'START_POSITION' or 'STARTPOSITION' or consider players with high rebounds.")
    fallbacks <- intersect(c("START_POSITION", "STARTPOSITION", "START.Position"), df_cols)
    if (length(fallbacks) > 0) {
      centers_all <- centers_stats_raw %>%
        mutate(Position = as.character(.data[[fallbacks[1]]])) %>%
        filter(!is.na(Position) & grepl("C", Position))
    } else {
      # As last resort: do not filter by position, and flag that user must provide position info separately
      warning("Position column not found. The dataset is not filtered to centers. Consider providing a position mapping file.")
      centers_all <- centers_stats_raw
      centers_all$Position <- NA
    }
  }
  
  # Keep/rename key columns into a canonical set
  canonical <- centers_all %>%
    mutate(
      PlayerID = if ("PLAYER_ID" %in% names(.)) .data$PLAYER_ID else if ("PLAYERID" %in% names(.)) .data$PLAYERID else NA_integer_,
      Player = if ("PLAYER_NAME" %in% names(.)) .data$PLAYER_NAME else if ("PLAYERNAME" %in% names(.)) .data$PLAYERNAME else NA_character_,
      Team = if ("TEAM_ABBREVIATION" %in% names(.)) .data$TEAM_ABBREVIATION else if ("TEAM" %in% names(.)) .data$TEAM else NA_character_,
      FG3A = if (!is.na(fga3_col)) as.numeric(.data[[fga3_col]]) else NA_real_,
      FG3_PCT = if (!is.na(fg3pct_col)) as.numeric(.data[[fg3pct_col]]) else NA_real_,
      GP = if (!is.na(gp_col)) as.numeric(.data[[gp_col]]) else NA_real_,
      MIN = if ("MIN" %in% names(.)) suppressWarnings(as.numeric(.data$MIN)) else NA_real_,
      PTS = if ("PTS" %in% names(.)) suppressWarnings(as.numeric(.data$PTS)) else NA_real_,
      REB = if ("REB" %in% names(.)) suppressWarnings(as.numeric(.data$REB)) else NA_real_,
      AST = if ("AST" %in% names(.)) suppressWarnings(as.numeric(.data$AST)) else NA_real_,
      Season = as.character(Season)
    ) %>%
    select(Season, PlayerID, Player, Team, Position, GP, MIN, PTS, REB, AST, FG3A, FG3_PCT)
  
  # Save canonical dataset
  centers_stats <- canonical
  write_csv(centers_stats, output_csv)
  saveRDS(centers_stats, output_rds)
  message("Saved centers dataset to: ", output_csv, " and ", output_rds)
}

# --------- CHUNK 5: convenience loader & demo runner ----------
load_centers_csv_if_exists <- function(path = file.path(save_dir, "centers_stats.csv")) {
  if (file.exists(path) && isTRUE(use_cached_canonical)) {
    message("Loading canonical centers dataset from: ", path)
    return(readr::read_csv(path, show_col_types = FALSE))
  } else {
    return(NULL)
  }
}

run_quick_pipeline <- function() {
  df <- load_centers_csv_if_exists()
  if (!is.null(df)) return(df)
  if (use_local_cache && file.exists(cached_csv)) {
    message("Loading cached CSV: ", cached_csv)
    df <- readr::read_csv(cached_csv, show_col_types = FALSE)
    return(df)
  }
  if (exists("batch_fetch_seasons")) {
    df <- batch_fetch_seasons(seasons_to_run, raw_dir = raw_dir)
  } else {
    message("Batch fetch helper not present; cannot run quick pipeline.")
    return(NULL)
  }
  readr::write_csv(df, file.path(save_dir, "centers_raw_download.csv"))
  df
}

# ---------- Data cleaning ----------
# Replace impossible percentages (e.g., 0/0) and ensure numeric types
centers_stats <- centers_stats %>%
  mutate(
    FG3A = as.numeric(FG3A),
    FG3_PCT = as.numeric(FG3_PCT),
    GP = as.numeric(GP),
    SeasonStart = as.integer(substr(Season, 1, 4))
  )

# Some rows might contain NA player names; drop them
centers_stats <- centers_stats %>% filter(!is.na(Player) & Player != "")

# ---------- Section 3: Summary statistics ----------
# Overall summary for FG3A and FG3_PCT (centers only)
cat("\nOverall descriptive statistics for centers (all seasons combined):\n")
print(summary(centers_stats %>% select(FG3A, FG3_PCT, GP, PTS, REB, AST)))

mean_fg3a <- mean(centers_stats$FG3A, na.rm = TRUE)
sd_fg3a <- sd(centers_stats$FG3A, na.rm = TRUE)
min_fg3a <- min(centers_stats$FG3A, na.rm = TRUE)
max_fg3a <- max(centers_stats$FG3A, na.rm = TRUE)

mean_fg3pct <- mean(centers_stats$FG3_PCT, na.rm = TRUE)
sd_fg3pct <- sd(centers_stats$FG3_PCT, na.rm = TRUE)
min_fg3pct <- min(centers_stats$FG3_PCT, na.rm = TRUE)
max_fg3pct <- max(centers_stats$FG3_PCT, na.rm = TRUE)

cat(sprintf("\nFG3A (per-game) mean = %.3f (SD = %.3f), min = %.3f, max = %.3f\n",
            mean_fg3a, sd_fg3a, min_fg3a, max_fg3a))
cat(sprintf("FG3_PCT mean = %.3f (SD = %.3f), min = %.3f, max = %.3f\n",
            mean_fg3pct, sd_fg3pct, min_fg3pct, max_fg3pct))

# Use skimr for a richer summary
cat("\nA skim summary (selecting the most relevant variables):\n")
print(skim(centers_stats %>% select(SeasonStart, Player, Position, FG3A, FG3_PCT, GP, PTS, REB)))

# By-season aggregated summaries
season_summary <- centers_stats %>%
  group_by(SeasonStart) %>%
  summarize(
    n_center_seasons = n(),
    avg_FG3A = mean(FG3A, na.rm = TRUE),
    sd_FG3A = sd(FG3A, na.rm = TRUE),
    avg_FG3_PCT = mean(FG3_PCT, na.rm = TRUE),
    sd_FG3_PCT = sd(FG3_PCT, na.rm = TRUE),
    prop_shooting_1plus = mean(FG3A >= 1, na.rm = TRUE)
  ) %>%
  arrange(SeasonStart)
print(head(season_summary, 10))

# ---------- Section 4: Visualizations and Tables ----------
# 1) Histogram of FG3A per game (centers)
p1 <- ggplot(centers_stats, aes(x = FG3A)) +
  geom_histogram(binwidth = 0.25, fill = "skyblue", color = "black") +
  labs(title = "Distribution of 3PA per Game for Centers (all seasons)",
       x = "3PA per Game",
       y = "Count of Center-Seasons") +
  theme_minimal()
ggsave(file.path(save_dir, "hist_fg3a_centers.png"), p1, width = 8, height = 5, dpi = 150)

# 2) Line plot: average FG3A per season
p2 <- ggplot(season_summary, aes(x = SeasonStart, y = avg_FG3A)) +
  geom_line(color = "blue", size = 1) +
  geom_point() +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  labs(title = "Average 3PA per Game by Centers Over Time",
       x = "Season (start year)",
       y = "Avg 3PA per Game") +
  theme_minimal()
ggsave(file.path(save_dir, "line_avg_fg3a_by_season.png"), p2, width = 9, height = 5, dpi = 150)

# 3) Line plot: average FG3_PCT per season
p3 <- ggplot(season_summary, aes(x = SeasonStart, y = avg_FG3_PCT)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point() +
  labs(title = "Average 3P% for Centers Over Time",
       x = "Season (start year)",
       y = "Avg 3P%") +
  theme_minimal()
ggsave(file.path(save_dir, "line_avg_fg3pct_by_season.png"), p3, width = 9, height = 5, dpi = 150)

# 4) Proportion of centers with >=1 3PA per game over time
p4 <- ggplot(season_summary, aes(x = SeasonStart, y = prop_shooting_1plus)) +
  geom_line(color = "purple", size = 1) +
  geom_point() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Proportion of Centers Attempting >=1 3PA per Game by Season",
       x = "Season (start year)",
       y = "Proportion (>=1 3PA/game)") +
  theme_minimal()
ggsave(file.path(save_dir, "prop_centers_1plus_3pa.png"), p4, width = 9, height = 5, dpi = 150)

# 5) Scatter: FG3A vs FG3_PCT
p5 <- ggplot(centers_stats %>% filter(!is.na(FG3_PCT)), aes(x = FG3A, y = FG3_PCT)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  labs(title = "FG3A vs FG3% for Centers (all seasons)",
       x = "3PA per Game",
       y = "3P%") +
  theme_minimal()
ggsave(file.path(save_dir, "scatter_fg3a_vs_fg3pct.png"), p5, width = 8, height = 6, dpi = 150)

# 6) Correlation heatmap (numeric features)
numeric_df <- centers_stats %>% select(GP, MIN, PTS, REB, AST, FG3A, FG3_PCT) %>% mutate_all(as.numeric)
cor_mat <- cor(na.omit(numeric_df), use = "pairwise.complete.obs")
png(file.path(save_dir, "correlation_heatmap.png"), width = 800, height = 600)
corrplot(cor_mat, method = "color", addCoef.col = "black", number.cex = 0.7, tl.cex = 0.9, title = "Correlation Matrix (centers)")
dev.off()

# 7) Table: Top center seasons by FG3A
top_by_fg3a <- centers_stats %>%
  arrange(desc(FG3A)) %>%
  select(Player, Season, Team, FG3A, FG3_PCT, GP, PTS) %>%
  slice_head(n = 10)
write_csv(top_by_fg3a, file.path(save_dir, "top_center_seasons_by_fg3a.csv"))
message("Saved top_by_fg3a table to data/top_center_seasons_by_fg3a.csv")

# Print top table to console for presentation
print(top_by_fg3a)

# ---------- Save final canonical centers dataset (again) ----------
write_csv(centers_stats, output_csv)
saveRDS(centers_stats, output_rds)
message("Final centers dataset written to: ", output_csv)

# ---------- Quick plots display for interactive session ----------
# If running interactively, print plots to the RStudio Plots pane
if (interactive()) {
  print(p1); print(p2); print(p3); print(p4); print(p5)
}

# --------- CHUNK 6: interactive quick-run instruction ----------
# In RStudio interactive session, run:
# demo_df <- run_quick_pipeline()
# if (is.data.frame(demo_df)) {
#   centers_stats_raw <- demo_df
#   # then run the canonicalization code above (or source the file to re-run)
# }
# Or to just load canonical CSV quickly:
# centers_stats <- load_centers_csv_if_exists()

print(list.files(save_dir, full.names = TRUE))