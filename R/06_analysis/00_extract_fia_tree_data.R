# =============================================================================
# Extract FIA Tree Data from Database
# =============================================================================
# Pulls tree-level data from FIA SQLite databases and saves as CSV
# =============================================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(readr)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  EXTRACT FIA TREE DATA FROM DATABASE\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# SETTINGS
# =============================================================================

# FIA database files
DB_FILES <- c(
  "data/raw/fia_sqlite/CT/unzipped/SQLite_FIADB_VT.db",
  "data/raw/fia_sqlite/CT/unzipped/SQLite_FIADB_NH.db",
  "data/raw/fia_sqlite/CT/unzipped/SQLite_FIADB_NY.db",
  "data/raw/fia_sqlite/CT/unzipped/SQLite_FIADB_ME.db",
  "data/raw/fia_sqlite/CT/unzipped/SQLite_FIADB_CT.db",
  "data/raw/fia_sqlite/CT/unzipped/SQLite_FIADB_RI.db",
  "data/raw/fia_sqlite/CT/unzipped/SQLite_FIADB_MA.db"
)

# Status codes for live trees
LIVE_STATUS <- c(1)

# Minimum DBH (inches)
MIN_DBH <- 5.0

cat("Database files to process:\n")
for (db in DB_FILES) {
  if (file.exists(db)) {
    cat("  ✓", db, "\n")
  } else {
    cat("  ✗", db, "(not found)\n")
  }
}
cat("\n")

# =============================================================================
# EXTRACT TREE DATA
# =============================================================================

all_trees <- list()

for (db_file in DB_FILES) {
  if (!file.exists(db_file)) {
    cat("Skipping:", db_file, "(not found)\n")
    next
  }
  
  state <- gsub(".*/(\\w+)_FIADB\\.db", "\\1", db_file)
  cat("Processing", state, "...\n")
  
  # Connect to database
  con <- dbConnect(SQLite(), db_file)
  
  # Check available tables
  tables <- dbListTables(con)
  
  if (!all(c("TREE", "COND", "PLOT") %in% tables)) {
    cat("  ⚠ Missing required tables, skipping\n\n")
    dbDisconnect(con)
    next
  }
  
  # Extract tree data with plot and condition info
  query <- "
    SELECT 
      t.CN AS TREE_CN,
      t.PLT_CN,
      t.CONDID,
      p.STATECD,
      p.UNITCD,
      p.COUNTYCD,
      p.PLOT,
      p.INVYR,
      p.MEASYEAR,
      t.SUBP,
      t.TREE,
      t.STATUSCD,
      t.SPCD,
      t.DIA,
      t.HT,
      t.ACTUALHT,
      t.CR,
      t.CCLCD,
      t.TREECLCD,
      t.DAMAGE1,
      t.DAMAGE2,
      t.DAMAGE3
    FROM TREE t
    JOIN PLOT p ON t.PLT_CN = p.CN
    WHERE t.STATUSCD IN (1)
      AND t.DIA >= 5.0
  "
  
  trees <- dbGetQuery(con, query)
  
  cat("  Trees extracted:", nrow(trees), "\n")
  
  # Add state name
  trees$STATE <- state
  
  all_trees[[state]] <- trees
  
  dbDisconnect(con)
  cat("\n")
}

# =============================================================================
# COMBINE AND ADD SPECIES NAMES
# =============================================================================

cat("Combining all states...\n")

fia_trees <- bind_rows(all_trees)

cat("  Total trees:", nrow(fia_trees), "\n\n")

# Load species reference table if available
ref_species_file <- "data/raw/fia_sqlite/CT/unzipped/REF_SPECIES.csv"

if (file.exists(ref_species_file)) {
  cat("Adding species common names...\n")
  
  ref_species <- read_csv(ref_species_file, show_col_types = FALSE) %>%
    select(SPCD, COMMON_NAME, GENUS, SPECIES, SPECIES_SYMBOL = SYMBOL)
  
  fia_trees <- fia_trees %>%
    left_join(ref_species, by = "SPCD")
  
  cat("  ✓ Species names added\n\n")
} else {
  cat("⚠ REF_SPECIES.csv not found, species codes only\n\n")
  fia_trees$COMMON_NAME <- paste0("Species_", fia_trees$SPCD)
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

output_dir <- "data/processed"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

output_file <- file.path(output_dir, "fia_tree.csv")

write_csv(fia_trees, output_file)

cat("✓ Saved FIA tree data:", output_file, "\n")
cat("  Rows:", nrow(fia_trees), "\n")
cat("  Columns:", ncol(fia_trees), "\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

summary_stats <- fia_trees %>%
  group_by(STATE) %>%
  summarise(
    n_trees = n(),
    n_plots = n_distinct(PLT_CN),
    mean_dbh = mean(DIA, na.rm = TRUE),
    mean_ht = mean(HT, na.rm = TRUE),
    n_species = n_distinct(SPCD),
    .groups = "drop"
  )

print(summary_stats)

cat("\n")
cat("Overall:\n")
cat("  States:", n_distinct(fia_trees$STATE), "\n")
cat("  Trees:", nrow(fia_trees), "\n")
cat("  Plots:", n_distinct(fia_trees$PLT_CN), "\n")
cat("  Species:", n_distinct(fia_trees$SPCD), "\n\n")

cat("Next: Run large tree analysis\n")
cat("  Rscript R/06_analysis/09_large_tree_analysis.R\n\n")
