# =============================================================================
# Large Tree Analysis: NEFIN vs FIA (WITH LATIN NAMES!)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(RSQLite)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
})

set.seed(42)

cat("\n═══════════════════════════════════════════════════════════════════\n")
cat("  LARGE TREE ANALYSIS: NEFIN vs FIA\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# SETTINGS
# =============================================================================

MIN_N_PER_SPECIES <- 30
N_BOOT <- 200

NEFIN_LIVE_CODES <- c(1)
FIA_LIVE_CODES <- c(1)

# =============================================================================
# LOAD NEFIN TREE DATA
# =============================================================================

cat("Loading NEFIN tree data...\n")

nefin_trees <- read_csv("data/raw/nefin/TREE_RAW_DATA.csv", 
                        show_col_types = FALSE) %>%
  transmute(
    dataset = "NEFIN",
    tree_id = as.character(row_number()),  # Make character
    status = treeStatus,
    species_raw = str_squish(treeSpecies),
    dbh_cm = as.numeric(DBH),
    ht_m = if ("fldTotalHeight" %in% names(.)) as.numeric(fldTotalHeight) 
    else if ("HT" %in% names(.)) as.numeric(HT)
    else NA_real_
  ) %>%
  filter(status %in% NEFIN_LIVE_CODES, !is.na(species_raw), !is.na(dbh_cm), dbh_cm > 0) %>%
  mutate(
    # Latin name: lowercase, no punctuation
    species = str_to_lower(species_raw),
    species = str_replace_all(species, "[^a-z ]", ""),
    species = str_squish(species)
  )

cat("  NEFIN trees:", format(nrow(nefin_trees), big.mark = ","), "\n")
cat("  Unique species:", n_distinct(nefin_trees$species), "\n\n")

# =============================================================================
# LOAD FIA TREE DATA WITH LATIN NAMES
# =============================================================================

cat("Loading FIA tree data from state databases...\n")

# Find all databases
fia_base_dir <- "data/raw/fia_sqlite"
state_dirs <- list.dirs(fia_base_dir, recursive = FALSE, full.names = TRUE)

cat("  State directories found:", length(state_dirs), "\n")

fia_dbs <- c()
for (state_dir in state_dirs) {
  unzipped_dir <- file.path(state_dir, "unzipped")
  if (dir.exists(unzipped_dir)) {
    dbs <- list.files(unzipped_dir, pattern = "SQLite_FIADB.*\\.db$", 
                      full.names = TRUE)
    if (length(dbs) > 0) {
      fia_dbs <- c(fia_dbs, dbs)
    }
  }
}

cat("  Total FIA databases found:", length(fia_dbs), "\n")
for (db in fia_dbs) {
  cat("    ", basename(db), "\n")
}
cat("\n")

if (length(fia_dbs) == 0) {
  stop("No FIA databases found!")
}

# Function to pull trees WITH LATIN NAMES
pull_fia_trees <- function(db_path) {
  state <- gsub(".*FIADB_([A-Z]{2})\\.db", "\\1", basename(db_path))
  cat("  ", state, "... ")
  
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  tables <- dbListTables(con)
  if (!("TREE" %in% tables)) {
    cat("SKIPPED\n")
    return(NULL)
  }
  
  # CRITICAL: Pull GENUS and SPECIES (Latin name) from REF_SPECIES
  if ("REF_SPECIES" %in% tables) {
    # Get REF_SPECIES with Latin names
    ref_query <- "SELECT SPCD, GENUS, SPECIES, COMMON_NAME FROM REF_SPECIES"
    ref_species <- tryCatch(dbGetQuery(con, ref_query), error = function(e) NULL)
    
    if (is.null(ref_species)) {
      cat("ERROR reading REF_SPECIES\n")
      return(NULL)
    }
    
    # Combine GENUS + SPECIES to make Latin name
    ref_species <- ref_species %>%
      mutate(
        latin_name = paste(str_to_lower(GENUS), str_to_lower(SPECIES)),
        latin_name = str_squish(latin_name)
      )
    
  } else {
    cat("NO REF_SPECIES\n")
    return(NULL)
  }
  
  # Pull tree data
  query <- "
    SELECT 
      t.CN as tree_cn,
      t.STATUSCD,
      t.DIA,
      t.HT,
      t.SPCD
    FROM TREE t
    WHERE t.STATUSCD = 1
      AND t.DIA IS NOT NULL
      AND t.DIA > 0
  "
  
  trees <- tryCatch(dbGetQuery(con, query), error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(trees) || nrow(trees) == 0) {
    cat("NO DATA\n")
    return(NULL)
  }
  
  # Join with REF_SPECIES to get Latin names
  trees <- trees %>%
    left_join(ref_species, by = "SPCD") %>%
    filter(!is.na(latin_name))
  
  cat(format(nrow(trees), big.mark = ","), "trees\n")
  
  trees %>%
    transmute(
      dataset = "FIA",
      tree_id = as.character(tree_cn),  # Ensure character
      status = STATUSCD,
      species_raw = latin_name,  # Use Latin name!
      dbh_cm = DIA * 2.54,
      ht_m = if (!is.null(HT)) HT * 0.3048 else NA_real_,
      species = latin_name  # Already standardized
    )
}

# Pull from all databases
cat("\n")
fia_trees_list <- lapply(fia_dbs, pull_fia_trees)
fia_trees <- bind_rows(fia_trees_list[!sapply(fia_trees_list, is.null)]) %>%
  filter(!is.na(species), !is.na(dbh_cm), dbh_cm > 0)

cat("\n  Total FIA trees:", format(nrow(fia_trees), big.mark = ","), "\n")
cat("  Unique species:", n_distinct(fia_trees$species), "\n\n")

# =============================================================================
# CHECK SPECIES OVERLAP
# =============================================================================

cat("Checking species overlap...\n")

nefin_species <- unique(nefin_trees$species)
fia_species <- unique(fia_trees$species)
overlap <- intersect(nefin_species, fia_species)

cat("  NEFIN unique:", length(nefin_species), "\n")
cat("  FIA unique:", length(fia_species), "\n")
cat("  Overlap:", length(overlap), "\n\n")

if (length(overlap) < 5) {
  cat("  ⚠ LIMITED OVERLAP!\n\n")
  
  cat("  Top 10 NEFIN species:\n")
  print(nefin_trees %>% count(species, sort = TRUE) %>% head(10))
  
  cat("\n  Top 10 FIA species:\n")
  print(fia_trees %>% count(species, sort = TRUE) %>% head(10))
  
  cat("\n  Overlapping species:\n")
  if (length(overlap) > 0) {
    overlap_counts <- bind_rows(
      nefin_trees %>% filter(species %in% overlap) %>% count(species, name = "NEFIN"),
      fia_trees %>% filter(species %in% overlap) %>% count(species, name = "FIA")
    ) %>%
      group_by(species) %>%
      summarize(NEFIN = sum(NEFIN, na.rm = TRUE),
                FIA = sum(FIA, na.rm = TRUE),
                .groups = "drop")
    print(overlap_counts)
  }
  
  if (length(overlap) == 0) {
    stop("No species overlap - cannot proceed")
  }
}

# Show overlapping species counts
cat("  Overlapping species (sample):\n")
nefin_counts <- nefin_trees %>% 
  filter(species %in% overlap) %>% 
  count(species, name = "NEFIN")

fia_counts <- fia_trees %>% 
  filter(species %in% overlap) %>% 
  count(species, name = "FIA")

overlap_sample <- nefin_counts %>%
  full_join(fia_counts, by = "species") %>%
  mutate(
    NEFIN = replace_na(NEFIN, 0),
    FIA = replace_na(FIA, 0)
  ) %>%
  arrange(desc(pmin(NEFIN, FIA)))

print(head(overlap_sample, 15))
cat("\n")

# Combine
trees_all <- bind_rows(nefin_trees, fia_trees)

# =============================================================================
# SPECIES ELIGIBILITY
# =============================================================================

cat("Finding eligible species (min", MIN_N_PER_SPECIES, "per dataset)...\n")

species_counts <- trees_all %>%
  group_by(dataset, species) %>%
  summarize(n = n(), .groups = "drop")

eligible_species <- species_counts %>%
  pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>%
  filter(NEFIN >= MIN_N_PER_SPECIES, FIA >= MIN_N_PER_SPECIES)

cat("  Eligible species:", nrow(eligible_species), "\n\n")

if (nrow(eligible_species) == 0) {
  cat("  ⚠ No species meet threshold!\n\n")
  
  close <- species_counts %>%
    pivot_wider(names_from = dataset, values_from = n, values_fill = 0) %>%
    mutate(min_n = pmin(NEFIN, FIA)) %>%
    filter(min_n >= 10) %>%
    arrange(desc(min_n))
  
  cat("  Species closest to threshold:\n")
  print(head(close, 20))
  
  cat("\n  Suggestion: Reduce MIN_N_PER_SPECIES\n")
  stop("No eligible species")
}

cat("  Eligible species (top 20):\n")
print(eligible_species %>% 
        arrange(desc(pmin(NEFIN, FIA))) %>% 
        head(20))
cat("\n")

trees_elig <- trees_all %>%
  filter(species %in% eligible_species$species)

cat("  Analysis trees:\n")
cat("    NEFIN:", format(sum(trees_elig$dataset == "NEFIN"), big.mark = ","), "\n")
cat("    FIA:", format(sum(trees_elig$dataset == "FIA"), big.mark = ","), "\n\n")

# =============================================================================
# ANALYSIS
# =============================================================================

cat("Computing species summaries...\n")

species_stats <- trees_elig %>%
  group_by(dataset, species) %>%
  summarize(
    n = n(),
    dbh_median = median(dbh_cm, na.rm = TRUE),
    dbh_p95 = quantile(dbh_cm, 0.95, na.rm = TRUE),
    dbh_p99 = quantile(dbh_cm, 0.99, na.rm = TRUE),
    dbh_max = max(dbh_cm, na.rm = TRUE),
    .groups = "drop"
  )

species_wide <- species_stats %>%
  pivot_wider(
    names_from = dataset,
    values_from = -c(dataset, species),
    names_sep = "__"
  ) %>%
  mutate(
    dbh_p99_delta = dbh_p99__NEFIN - dbh_p99__FIA,
    dbh_max_delta = dbh_max__NEFIN - dbh_max__FIA,
    dbh_p99_pct = 100 * (dbh_p99__NEFIN - dbh_p99__FIA) / dbh_p99__FIA
  )

cat("  Species analyzed:", nrow(species_wide), "\n\n")

# Bootstrap for P99
cat("Bootstrap confidence intervals (", N_BOOT, "reps)...\n")

boot_delta <- function(x_nefin, x_fia, p = 0.99, nboot = N_BOOT) {
  x_nefin <- x_nefin[is.finite(x_nefin)]
  x_fia <- x_fia[is.finite(x_fia)]
  
  if (length(x_nefin) < 5 || length(x_fia) < 5) {
    return(tibble(est = NA_real_, lo95 = NA_real_, hi95 = NA_real_))
  }
  
  deltas <- replicate(nboot, {
    qn <- quantile(sample(x_nefin, replace = TRUE), p, na.rm = TRUE)
    qf <- quantile(sample(x_fia, replace = TRUE), p, na.rm = TRUE)
    as.numeric(qn - qf)
  })
  
  tibble(
    est = as.numeric(quantile(x_nefin, p) - quantile(x_fia, p)),
    lo95 = quantile(deltas, 0.025),
    hi95 = quantile(deltas, 0.975)
  )
}

boot_results <- trees_elig %>%
  group_by(species) %>%
  group_modify(~{
    dbh_n <- filter(.x, dataset == "NEFIN")$dbh_cm
    dbh_f <- filter(.x, dataset == "FIA")$dbh_cm
    boot_delta(dbh_n, dbh_f, p = 0.99) %>%
      rename_with(~paste0("dbh_p99_", .))
  })

species_final <- species_wide %>%
  left_join(boot_results, by = "species") %>%
  arrange(desc(dbh_p99_delta))

# Save
output_dir <- "data/processed/large_tree_analysis"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(species_final, file.path(output_dir, "species_summary.csv"))
write_csv(eligible_species, file.path(output_dir, "eligible_species.csv"))

cat("\n✓ Results saved to:", output_dir, "\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("═══════════════════════════════════════════════════════════════════\n")
cat("  SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════════\n\n")

significant <- species_final %>%
  filter(dbh_p99_lo95 > 0)

if (nrow(significant) > 0) {
  cat("Species with statistically significant NEFIN advantage (P99 DBH):\n\n")
  print(significant %>% 
          select(species, dbh_p99_delta, dbh_p99_pct, 
                 dbh_p99_lo95, dbh_p99_hi95, n__NEFIN, n__FIA) %>%
          head(15))
  
  cat("\n")
  cat(sprintf("✓ %d/%d species show NEFIN captures larger trees (95%% CI)\n",
              nrow(significant), nrow(species_final)))
  cat(sprintf("  Average P99 advantage: %.1f cm (%.1f%%)\n",
              mean(significant$dbh_p99_delta),
              mean(significant$dbh_p99_pct)))
  
  major <- significant %>% filter(dbh_p99_delta > 10)
  if (nrow(major) > 0) {
    cat(sprintf("  %d species with >10 cm advantage\n\n", nrow(major)))
    cat("Species priority for allometric work (>10 cm advantage):\n")
    print(major %>% 
            select(species, dbh_p99_delta, dbh_max__NEFIN, dbh_max__FIA) %>%
            arrange(desc(dbh_p99_delta)))
    cat("\n✓ NEFIN highly valuable for allometric equation development\n")
  }
} else {
  cat("→ No species show statistically significant NEFIN advantage\n")
  cat("  FIA captures similar-sized trees\n")
}

cat("\nAnalysis complete!\n\n")