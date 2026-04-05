# ============================================================================
# Utility Functions
# ============================================================================

# Filter plot data based on user selections -----------------------------------
filter_plot_data <- function(data, dataset_filter, state_filter, biomass_range) {
  
  # Start with full dataset
  filtered <- data
  
  # Apply dataset filter
  if (dataset_filter != "all") {
    filtered <- filtered %>%
      filter(dataset == toupper(dataset_filter))
  }
  
  # Apply state filter
  if (!"All" %in% state_filter) {
    filtered <- filtered %>%
      filter(state %in% state_filter)
  }
  
  # Apply biomass range filter
  filtered <- filtered %>%
    filter(
      biomass >= biomass_range[1],
      biomass <= biomass_range[2]
    )
  
  return(filtered)
}

# Calculate summary statistics by dataset -------------------------------------
calculate_summary_stats <- function(data, dataset_filter) {
  
  # Determine which datasets to include
  if (dataset_filter == "all") {
    datasets_to_calc <- c("FIA", "NEFIN", "Pooled")
  } else if (dataset_filter == "pooled") {
    datasets_to_calc <- "Pooled"
  } else {
    datasets_to_calc <- toupper(dataset_filter)
  }
  
  # Calculate stats for each dataset
  results <- list()
  
  for (ds in datasets_to_calc) {
    if (ds == "Pooled") {
      subset_data <- data
      dataset_label <- "Pooled"
    } else {
      subset_data <- data %>% filter(dataset == ds)
      dataset_label <- ds
    }
    
    # Sample sizes
    n_plots <- nrow(subset_data)
    
    # Biomass stats
    biomass_mean <- mean(subset_data$biomass, na.rm = TRUE)
    biomass_sd <- sd(subset_data$biomass, na.rm = TRUE)
    biomass_median <- median(subset_data$biomass, na.rm = TRUE)
    biomass_p95 <- quantile(subset_data$biomass, 0.95, na.rm = TRUE)
    biomass_p99 <- quantile(subset_data$biomass, 0.99, na.rm = TRUE)
    
    # NDVI stats
    ndvi_s2_mean <- mean(subset_data$ndvi_s2, na.rm = TRUE)
    ndvi_s2_sd <- sd(subset_data$ndvi_s2, na.rm = TRUE)
    
    ndvi_modis_mean <- mean(subset_data$ndvi_modis, na.rm = TRUE)
    ndvi_modis_sd <- sd(subset_data$ndvi_modis, na.rm = TRUE)
    
    # Climate stats
    temp_mean <- mean(subset_data$temp_mean, na.rm = TRUE)
    temp_sd <- sd(subset_data$temp_mean, na.rm = TRUE)
    
    precip_mean <- mean(subset_data$precip_annual, na.rm = TRUE)
    precip_sd <- sd(subset_data$precip_annual, na.rm = TRUE)
    
    results[[dataset_label]] <- list(
      dataset = dataset_label,
      n_plots = n_plots,
      biomass_mean = biomass_mean,
      biomass_sd = biomass_sd,
      biomass_median = biomass_median,
      biomass_p95 = biomass_p95,
      biomass_p99 = biomass_p99,
      ndvi_s2_mean = ndvi_s2_mean,
      ndvi_s2_sd = ndvi_s2_sd,
      ndvi_modis_mean = ndvi_modis_mean,
      ndvi_modis_sd = ndvi_modis_sd,
      temp_mean = temp_mean,
      temp_sd = temp_sd,
      precip_mean = precip_mean,
      precip_sd = precip_sd
    )
  }
  
  return(bind_rows(results))
}

# Perform statistical tests between FIA and NEFIN -----------------------------
perform_comparison_tests <- function(data) {

  fia_data <- data %>% filter(dataset == "FIA")
  nefin_data <- data %>% filter(dataset == "NEFIN")

  # Skip if either dataset is empty
  if (nrow(fia_data) == 0 || nrow(nefin_data) == 0) {
    return(NULL)
  }

  # Safe wrappers: return NULL when either vector has < 2 non-NA values
  safe_ks <- function(x, y) {
    x <- x[!is.na(x)]; y <- y[!is.na(y)]
    if (length(x) < 2 || length(y) < 2) return(NULL)
    ks.test(x, y)
  }
  safe_wilcox <- function(x, y) {
    x <- x[!is.na(x)]; y <- y[!is.na(y)]
    if (length(x) < 2 || length(y) < 2) return(NULL)
    wilcox.test(x, y)
  }

  # Kolmogorov-Smirnov tests
  ks_biomass <- safe_ks(fia_data$biomass, nefin_data$biomass)
  ks_ndvi_s2 <- safe_ks(fia_data$ndvi_s2, nefin_data$ndvi_s2)
  ks_temp <- safe_ks(fia_data$temp_mean, nefin_data$temp_mean)
  ks_precip <- safe_ks(fia_data$precip_annual, nefin_data$precip_annual)

  # Mann-Whitney U tests (for medians)
  mw_biomass <- safe_wilcox(fia_data$biomass, nefin_data$biomass)

  return(list(
    ks_biomass = ks_biomass,
    ks_ndvi_s2 = ks_ndvi_s2,
    ks_temp = ks_temp,
    ks_precip = ks_precip,
    mw_biomass = mw_biomass
  ))
}

# Format p-values for display -------------------------------------------------
format_pvalue <- function(p) {
  if (is.na(p)) return("N/A")
  if (p < 0.001) return("p<0.001")
  return(paste0("p=", round(p, 3)))
}

# Format numbers with appropriate precision -----------------------------------
format_stat <- function(x, digits = 1) {
  if (is.na(x)) return("N/A")
  return(format(round(x, digits), nsmall = digits))
}

# Create composite label with mean ± SD ---------------------------------------
format_mean_sd <- function(mean_val, sd_val, digits = 1) {
  paste0(
    format_stat(mean_val, digits),
    " ± ",
    format_stat(sd_val, digits)
  )
}
