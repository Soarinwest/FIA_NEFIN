# ============================================================================
# mod_modeling.R — Tab 5: Modeling Results
# Three sub-tabs: Performance, Training Comparison, Predictions
# ============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────

modeling_ui <- function(id) {
  ns <- NS(id)

  navset_card_tab(

    # Sub-tab 1: Performance ───────────────────────────────────────────────────
    nav_panel(
      title = tagList(bsicons::bs_icon("graph-up"), " Performance"),
      value = "perf",
      tagList(
        # Subtle value boxes — neutral slate with left accent
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(
            title    = "Best Test R\u00b2",
            value    = uiOutput(ns("best_r2_val")),
            showcase = bsicons::bs_icon("bullseye"),
            theme    = "secondary"
          ),
          value_box(
            title    = "Best Test RMSE",
            value    = uiOutput(ns("best_rmse_val")),
            showcase = bsicons::bs_icon("rulers"),
            theme    = "secondary"
          ),
          value_box(
            title    = "Pooled Gain (10m RF)",
            value    = uiOutput(ns("pooled_gain_val")),
            showcase = bsicons::bs_icon("arrow-up-circle"),
            theme    = "success"
          )
        ),

        # Filter row — choices populated dynamically in server
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput(ns("perf_model"), "Model type:",
            choices = "All", selected = "All"),
          selectInput(ns("perf_scale"), "Scale:",
            choices = "All", selected = "All"),
          selectInput(ns("perf_scenario"), "Scenario:",
            choices = "All", selected = "All")
        ),

        # Charts
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("CV Fold R\u00b2 by Scenario"),
            card_body(plotly::plotlyOutput(ns("fold_r2_box"), height = "420px"))
          ),
          card(
            card_header("Residuals vs. Observed Biomass"),
            card_body(
              tags$p(class = "text-muted small",
                "Positive = overprediction. Dashed line = zero bias.",
                " Loess smooth shows systematic bias structure."
              ),
              plotly::plotlyOutput(ns("residual_plot"), height = "380px")
            )
          )
        )
      )
    ),

    # Sub-tab 2: Training Comparison ───────────────────────────────────────────
    nav_panel(
      title = tagList(bsicons::bs_icon("bar-chart-steps"), " Training Comparison"),
      value = "comparison",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("RMSE by Training Scenario"),
          card_body(
            tags$p(class = "text-muted small",
              "Test-set RMSE (Mg/ha) for FIA-only, NEFIN-only, and Pooled training at each scale.",
              " Lower is better."
            ),
            plotly::plotlyOutput(ns("rmse_comparison"), height = "320px")
          )
        ),
        card(
          card_header("Fuzzing Impact  - Significance Tests"),
          card_body(
            tags$p(class = "text-muted small",
              "Paired comparison of prediction errors between FIA-only and other training scenarios."
            ),
            DT::DTOutput(ns("sig_table"))
          )
        )
      ),
      card(
        card_header("Spatial CV Fold Performance"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            div(
              h6("R\u00b2 by Fold"),
              tags$img(src = "figures/cv_r2_boxplots.png",
                       style = "max-width:100%; border-radius:4px;")
            ),
            div(
              h6("RMSE by Fold"),
              tags$img(src = "figures/cv_rmse_boxplots.png",
                       style = "max-width:100%; border-radius:4px;")
            )
          )
        )
      )
    ),

    # Sub-tab 3: Interpretability (inner tabs for full-width content) ─────────
    nav_panel(
      title = tagList(bsicons::bs_icon("diagram-3"), " Interpretability"),
      value = "interpretability",

      navset_card_tab(
        id = ns("interp_tabs"),

        # Inner tab: Variable Importance
        nav_panel(
          title = "Variable Importance",
          layout_columns(
            col_widths = c(4, 4, 4),
            selectInput(ns("imp_model"), "Model:",
              choices = "All", selected = "All"),
            selectInput(ns("imp_scale"), "Scale:",
              choices = "All", selected = "All"),
            tags$p(class = "text-muted small mt-4",
              "Dots show per-scenario importance.",
              " ETH Canopy Height anchors at 100%."
            )
          ),
          plotly::plotlyOutput(ns("var_imp"), height = "550px")
        ),

        # Inner tab: Prediction Maps
        nav_panel(
          title = "Prediction Maps",
          selectInput(ns("pred_map_select"), "Map:",
            choices = c(
              "10m Fine Scale  - Pooled"                  = "pred_fine_pooled.png",
              "250m Coarse Scale  - Pooled"               = "pred_coarse_pooled.png",
              "Absolute Difference (Pooled  - FIA-only)" = "pred_abs_diff.png"
            ),
            selected = "pred_fine_pooled.png",
            width = "400px"
          ),
          uiOutput(ns("pred_map_display"))
        ),

        # Inner tab: Error by Biomass Class
        nav_panel(
          title = "Error by Biomass",
          tags$p(class = "text-muted small",
            "RMSE by test-set biomass quartile. FIA underpredicts high-biomass stands (Q4);",
            " NEFIN overpredicts low-biomass (Q1). Pooled balances both."
          ),
          tags$img(src = "figures/error_by_biomass_class.png",
                   style = "max-width:100%; border-radius:4px;")
        ),

        # Inner tab: Error by Terrain
        nav_panel(
          title = "Error by Terrain",
          tags$p(class = "text-muted small",
            "RMSE by terrain class (slope). Fuzzing hurts more on steep/heterogeneous terrain",
            " where 1-mile displacement crosses environmental gradients."
          ),
          tags$img(src = "figures/error_by_terrain.png",
                   style = "max-width:100%; border-radius:4px;")
        ),

        # Inner tab: Residual Structure
        nav_panel(
          title = "Residual Structure",
          tags$p(class = "text-muted small",
            "Residual (predicted  - observed) vs observed AGB.",
            " Positive = overprediction. Loess smooth with 95% CI.",
            " Biomass quartile boundaries shown as vertical bands."
          ),
          tags$img(src = "figures/residuals_vs_observed.png",
                   style = "max-width:100%; border-radius:4px;")
        )
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

modeling_server <- function(id, cv_results, fold_results, test_predictions,
                             var_importance, fuzzing_sig, fuzzing_rmse) {
  moduleServer(id, function(input, output, session) {

    # ── Populate filter dropdowns from actual data ─────────────────────────────
    observe({
      message("[modeling] Initializing Performance filters...")
      # Model names: prefer model_name if available, else model_type
      model_col <- if ("model_name" %in% names(fold_results)) "model_name" else "model_type"
      model_vals <- sort(unique(fold_results[[model_col]]))
      message("[modeling] Model values: ", paste(model_vals, collapse = ", "))
      updateSelectInput(session, "perf_model",
        choices = c("All", setNames(model_vals, model_vals)))

      scale_vals <- sort(unique(fold_results$scale))
      message("[modeling] Scale values: ", paste(scale_vals, collapse = ", "))
      updateSelectInput(session, "perf_scale",
        choices = c("All", setNames(scale_vals, scale_vals)))

      scenario_vals <- unique(fold_results$scenario)
      message("[modeling] Scenario values: ", paste(scenario_vals, collapse = ", "))
      updateSelectInput(session, "perf_scenario",
        choices = c("All", setNames(scenario_vals, scenario_vals)))
    })

    # ── Helper: apply perf filters ─────────────────────────────────────────────
    apply_perf_filter <- function(data, model_col = "model_type",
                                   scale_col = "scale", scenario_col = "scenario") {
      message("[modeling filter] Input: model=", input$perf_model,
              ", scale=", input$perf_scale, ", scenario=", input$perf_scenario)
      message("[modeling filter] Before filters: ", nrow(data), " rows")

      if (!is.null(input$perf_model) && input$perf_model != "All") {
        if ("model_name" %in% names(data)) {
          data <- dplyr::filter(data,
            .data[["model_name"]] == input$perf_model |
            .data[[model_col]] == input$perf_model)
        } else {
          data <- dplyr::filter(data, .data[[model_col]] == input$perf_model)
        }
        message("[modeling filter] After model filter: ", nrow(data), " rows")
      }
      if (!is.null(input$perf_scale) && input$perf_scale != "All") {
        data <- dplyr::filter(data,
          .data[[scale_col]] == input$perf_scale |
          grepl(sub(".*\\((.+)\\).*", "\\1", input$perf_scale),
                .data[[scale_col]], fixed = TRUE))
        message("[modeling filter] After scale filter: ", nrow(data), " rows")
      }
      if (!is.null(input$perf_scenario) && input$perf_scenario != "All") {
        data <- dplyr::filter(data, .data[[scenario_col]] == input$perf_scenario)
        message("[modeling filter] After scenario filter: ", nrow(data), " rows")
      }
      data
    }

    # ── Value boxes ────────────────────────────────────────────────────────────
    output$best_r2_val <- renderUI({
      best <- cv_results |>
        dplyr::arrange(dplyr::desc(test_r2)) |>
        dplyr::slice(1)
      tags$span(
        round(best$test_r2, 3),
        tags$br(),
        tags$small(class = "text-muted",
          best$model_type, " -", best$scale, " -", best$scenario)
      )
    })

    output$best_rmse_val <- renderUI({
      best <- cv_results |>
        dplyr::arrange(test_rmse) |>
        dplyr::slice(1)
      tags$span(
        paste0(round(best$test_rmse, 1), " Mg/ha"),
        tags$br(),
        tags$small(class = "text-muted",
          best$model_type, " -", best$scale, " -", best$scenario)
      )
    })

    output$pooled_gain_val <- renderUI({
      fia_row    <- dplyr::filter(fuzzing_rmse, scale == "10m", scenario == "FIA Only")
      pooled_row <- dplyr::filter(fuzzing_rmse, scale == "10m", scenario == "Pooled")
      if (nrow(fia_row) == 0 || nrow(pooled_row) == 0) return("N/A")
      pct_gain <- round(100 * (fia_row$rmse[1] - pooled_row$rmse[1]) / fia_row$rmse[1], 1)
      tags$span(
        paste0(pct_gain, "% lower RMSE"),
        tags$br(),
        tags$small(class = "text-muted", "Pooled vs. FIA-only (10m)")
      )
    })

    # ── Fold R² boxplot — colored by scenario (matching manuscript) ────────────
    output$fold_r2_box <- plotly::renderPlotly({
      data <- apply_perf_filter(fold_results,
                                 model_col    = "model_type",
                                 scale_col    = "scale",
                                 scenario_col = "scenario")
      req(nrow(data) > 0)

      data <- data |>
        dplyr::mutate(
          scenario = factor(scenario, levels = c("FIA Only", "NEFIN Only", "Pooled")),
          hover_text = paste0(
            "Model: ", model_type, "<br>",
            "Scale: ", scale, "<br>",
            "Scenario: ", scenario, "<br>",
            "Fold: ", fold, "<br>",
            "R\u00b2: ", round(r2, 3)
          )
        )

      # Determine facet column
      has_scale <- length(unique(data$scale)) > 1

      p <- ggplot2::ggplot(data,
        ggplot2::aes(x = scenario, y = r2,
                     fill = scenario, text = hover_text)) +
        ggplot2::geom_boxplot(alpha = 0.8, outlier.shape = NA) +
        ggplot2::geom_jitter(width = 0.15, size = 1.5, alpha = 0.6,
                             color = SLATE_TEXT) +
        ggplot2::scale_fill_manual(values = SCENARIO_COLORS) +
        ggplot2::labs(
          x    = NULL,
          y    = "Cross-Validation R\u00b2",
          fill = "Scenario"
        ) +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "none")

      if (has_scale) {
        p <- p + ggplot2::facet_wrap(~scale, nrow = 1)
      }

      plotly::ggplotly(p, tooltip = "text") |>
        plotly_dark_layout()
    })

    # ── Residual plot — replaces obs vs pred (matches manuscript Fig S2) ──────
    output$residual_plot <- plotly::renderPlotly({
      data <- apply_perf_filter(
        test_predictions,
        model_col    = "model",
        scale_col    = "scale",
        scenario_col = "scenario"
      )
      req(nrow(data) > 0)

      data <- data |>
        dplyr::mutate(
          residual_val = predicted - observed,
          scenario = factor(scenario, levels = c("FIA Only", "Pooled", "NEFIN Only")),
          hover_text = paste0(
            "Observed: ", round(observed, 1), " Mg/ha<br>",
            "Predicted: ", round(predicted, 1), " Mg/ha<br>",
            "Residual: ", round(residual_val, 1), " Mg/ha<br>",
            "Scenario: ", scenario
          )
        )

      p <- ggplot2::ggplot(data,
        ggplot2::aes(x = observed, y = residual_val, text = hover_text)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            color = SLATE_MUTED, linewidth = 0.6) +
        ggplot2::geom_point(ggplot2::aes(color = scenario),
                            alpha = 0.5, size = 1.5) +
        ggplot2::geom_smooth(ggplot2::aes(color = scenario),
                             method = "loess", se = FALSE, linewidth = 1.2) +
        ggplot2::scale_color_manual(values = SCENARIO_COLORS) +
        ggplot2::facet_wrap(~scenario, nrow = 1) +
        ggplot2::labs(
          x     = "Observed AGB (Mg/ha)",
          y     = "Residual (Predicted  - Observed)",
          color = "Scenario"
        ) +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "none")

      plotly::ggplotly(p, tooltip = "text") |>
        plotly_dark_layout()
    })

    # ── RMSE comparison chart — scenario colors ──────────────────────────────
    output$rmse_comparison <- plotly::renderPlotly({
      data <- fuzzing_rmse |>
        dplyr::mutate(
          scenario = factor(scenario, levels = c("FIA Only", "NEFIN Only", "Pooled")),
          hover_text = paste0(
            "Scale: ", scale, "<br>",
            "Scenario: ", scenario, "<br>",
            "RMSE: ", round(rmse, 1), " Mg/ha"
          )
        )

      p <- ggplot2::ggplot(data,
        ggplot2::aes(x = scale, y = rmse,
                     fill = scenario, text = hover_text)) +
        ggplot2::geom_col(position = "dodge", alpha = 0.85) +
        ggplot2::scale_fill_manual(values = SCENARIO_COLORS) +
        ggplot2::labs(
          x    = "Scale",
          y    = "Test RMSE (Mg/ha)",
          fill = "Training Scenario"
        ) +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "bottom")

      plotly::ggplotly(p, tooltip = "text") |>
        plotly_dark_layout()
    })

    # ── Significance table — dark theme ──────────────────────────────────────
    output$sig_table <- DT::renderDT({
      fuzzing_sig |>
        dplyr::mutate(
          p_value          = round(p_value, 4),
          mean_error_fia   = round(mean_error_fia, 2),
          mean_error_other = round(mean_error_other, 2),
          significant      = dplyr::if_else(significant, "\u2713 Yes", "No")
        ) |>
        dplyr::rename(
          Scale             = scale,
          Comparison        = comparison,
          N                 = n_paired,
          "Mean Error (FIA)" = mean_error_fia,
          "Mean Error (Other)" = mean_error_other,
          "p-value"         = p_value,
          Significant       = significant
        ) |>
        DT::datatable(
          rownames = FALSE,
          options  = list(
            dom      = "t",
            paging   = FALSE,
            ordering = FALSE,
            initComplete = DT::JS(
              "function(settings, json) {",
              "  $(this.api().table().container()).css({",
              "    'background-color': '#1e293b',",
              "    'color': '#e2e8f0'",
              "  });",
              "}"
            )
          )
        ) |>
        DT::formatStyle(
          "Significant",
          color = DT::styleEqual(c("\u2713 Yes", "No"), c("#10b981", "#64748b"))
        )
    })

    # ── Populate importance filter dropdowns ────────────────────────────────────
    observe({
      message("[var_imp] columns: ", paste(names(var_importance), collapse = ", "))
      if ("scenario" %in% names(var_importance)) {
        message("[var_imp] scenario values: ",
                paste(unique(var_importance$scenario), collapse = ", "))
      } else {
        message("[var_imp] WARNING: no 'scenario' column found!")
      }

      model_col <- if ("model_name" %in% names(var_importance)) "model_name" else "model_type"
      model_vals <- sort(unique(var_importance[[model_col]]))
      message("[var_imp] model values: ", paste(model_vals, collapse = ", "))
      updateSelectInput(session, "imp_model",
        choices = c("All", setNames(model_vals, model_vals)))

      scale_vals <- sort(unique(var_importance$scale))
      message("[var_imp] scale values: ", paste(scale_vals, collapse = ", "))
      updateSelectInput(session, "imp_scale",
        choices = c("All", setNames(scale_vals, scale_vals)))
    })

    # ── Prediction map selector ─────────────────────────────────────────────────
    output$pred_map_display <- renderUI({
      req(input$pred_map_select)
      tags$img(
        src   = paste0("figures/", input$pred_map_select),
        style = "max-width:100%; border-radius:4px;"
      )
    })

    # ── Variable importance — per-scenario dot plot with model/scale filters ──
    output$var_imp <- plotly::renderPlotly({
      data <- var_importance

      # Apply model filter
      if (!is.null(input$imp_model) && input$imp_model != "All") {
        if ("model_name" %in% names(data)) {
          data <- dplyr::filter(data,
            .data[["model_name"]] == input$imp_model |
            model_type == input$imp_model)
        } else {
          data <- dplyr::filter(data, model_type == input$imp_model)
        }
      }

      # Apply scale filter
      if (!is.null(input$imp_scale) && input$imp_scale != "All") {
        data <- dplyr::filter(data,
          scale == input$imp_scale |
          grepl(sub(".*\\((.+)\\).*", "\\1", input$imp_scale),
                scale, fixed = TRUE))
      }

      req(nrow(data) > 0)

      # Top 15 by mean importance, keep per-scenario values
      top_vars <- data |>
        dplyr::group_by(variable) |>
        dplyr::summarise(mean_imp = mean(importance_norm, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::arrange(dplyr::desc(mean_imp)) |>
        dplyr::slice_head(n = 15) |>
        dplyr::pull(variable)

      # Log actual scenario values for debugging
      message("[var_imp] Scenario values in data: ",
              paste(unique(data$scenario), collapse = ", "))

      data <- data |>
        dplyr::filter(variable %in% top_vars) |>
        dplyr::mutate(
          # Normalize scenario names to match SCENARIO_COLORS
          # Handles formats like "Scale (10m) Fia Only", "FIA Only", "fia_only"
          scenario = dplyr::case_when(
            grepl("fia",   scenario, ignore.case = TRUE) ~ "FIA Only",
            grepl("nefin", scenario, ignore.case = TRUE) ~ "NEFIN Only",
            grepl("pool",  scenario, ignore.case = TRUE) ~ "Pooled",
            TRUE ~ scenario
          ),
          scenario = factor(scenario, levels = c("FIA Only", "Pooled", "NEFIN Only")),
          var_label = dplyr::case_when(
            variable %in% c("canopy_height_10m",
                             "canopy_height",
                             "ETH_canopy_height") ~ "Canopy Height",
            variable == "canopy_height_250m"      ~ "Canopy Height (250m)",
            grepl("^tmean", variable)             ~ "Mean Temp",
            grepl("^tmax",  variable)             ~ "Max Temp",
            grepl("^tmin",  variable)             ~ "Min Temp",
            grepl("^ppt",   variable)             ~ "Precipitation",
            grepl("ndvi_s2",    variable)         ~ "NDVI (S2)",
            grepl("ndvi_modis", variable)         ~ "NDVI (MODIS)",
            grepl("ndwi_s2",    variable)         ~ "NDWI (S2)",
            grepl("ndwi_modis", variable)         ~ "NDWI (MODIS)",
            grepl("nbr_s2",     variable)         ~ "NBR (S2)",
            grepl("nbr_modis",  variable)         ~ "NBR (MODIS)",
            grepl("evi_s2",     variable)         ~ "EVI (S2)",
            grepl("evi_modis",  variable)         ~ "EVI (MODIS)",
            grepl("red_s2",     variable)         ~ "Red (S2)",
            grepl("red_modis",  variable)         ~ "Red (MODIS)",
            grepl("green_s2",   variable)         ~ "Green (S2)",
            grepl("green_modis",variable)         ~ "Green (MODIS)",
            grepl("blue_s2",    variable)         ~ "Blue (S2)",
            grepl("blue_modis", variable)         ~ "Blue (MODIS)",
            grepl("nir_modis",  variable)         ~ "NIR (MODIS)",
            grepl("swir1_modis",variable)         ~ "SWIR1 (MODIS)",
            grepl("^elevation", variable)         ~ "Elevation",
            grepl("^slope",     variable)         ~ "Slope",
            grepl("^aspect",    variable)         ~ "Aspect",
            TRUE ~ variable
          ),
          hover_text = paste0(
            var_label, "<br>",
            "Scenario: ", scenario, "<br>",
            "Importance: ", round(importance_norm, 1), "%"
          )
        )

      # Order by mean importance
      var_order <- data |>
        dplyr::group_by(var_label) |>
        dplyr::summarise(m = mean(importance_norm, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(m) |>
        dplyr::pull(var_label)
      data$var_label <- factor(data$var_label, levels = var_order)

      p <- ggplot2::ggplot(data,
        ggplot2::aes(x = importance_norm, y = var_label,
                     color = scenario, text = hover_text)) +
        ggplot2::geom_point(size = 3.5, alpha = 0.85) +
        ggplot2::scale_color_manual(values = SCENARIO_COLORS) +
        ggplot2::labs(
          x     = "Relative Importance (% of max)",
          y     = NULL,
          color = "Training Scenario"
        ) +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "bottom")

      plotly::ggplotly(p, tooltip = "text") |>
        plotly_dark_layout()
    })

  })
}
