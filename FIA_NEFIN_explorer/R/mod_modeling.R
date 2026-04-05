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
        # Value boxes
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(
            title    = "Best Test R\u00b2",
            value    = uiOutput(ns("best_r2_val")),
            showcase = bsicons::bs_icon("bullseye"),
            theme    = "success"
          ),
          value_box(
            title    = "Best Test RMSE",
            value    = uiOutput(ns("best_rmse_val")),
            showcase = bsicons::bs_icon("rulers"),
            theme    = "primary"
          ),
          value_box(
            title    = "Pooled Gain (10m RF)",
            value    = uiOutput(ns("pooled_gain_val")),
            showcase = bsicons::bs_icon("arrow-up-circle"),
            theme    = "success"
          )
        ),

        # Filter row
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput(ns("perf_model"), "Model type:",
            choices = c("All", "Random Forest" = "Random Forest",
                        "XGBoost" = "XGBoost"),
            selected = "All"),
          selectInput(ns("perf_scale"), "Scale:",
            choices = c("All", "Fine Scale (10m)", "Coarse Scale (250m)"),
            selected = "All"),
          selectInput(ns("perf_scenario"), "Scenario:",
            choices = c("All", "FIA Only", "NEFIN Only", "Pooled"),
            selected = "All")
        ),

        # Charts
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("CV Fold R\u00b2 Distribution"),
            card_body(plotly::plotlyOutput(ns("fold_r2_box"), height = "380px"))
          ),
          card(
            card_header("Observed vs. Predicted Biomass"),
            card_body(plotly::plotlyOutput(ns("obs_pred"), height = "380px"))
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
          card_header("Fuzzing Impact \u2014 Significance Tests"),
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

    # Sub-tab 3: Predictions ───────────────────────────────────────────────────
    nav_panel(
      title = tagList(bsicons::bs_icon("diagram-3"), " Predictions & Importance"),
      value = "predictions",
      layout_columns(
        col_widths = c(5, 7),
        card(
          full_screen = TRUE,
          card_header("Variable Importance (Fine Scale RF)"),
          card_body(
            tags$p(class = "text-muted small",
              "Top 15 predictors — ETH Global Canopy Height 2020 anchors at 100%.",
              " Climate from Daymet V4."
            ),
            plotly::plotlyOutput(ns("var_imp"), height = "500px")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Spatial Biomass Predictions \u2014 Northeastern US"),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              div(
                tags$p(class = "text-center small fw-bold", "10m Fine Scale \u2014 Pooled"),
                tags$img(src   = "figures/pred_fine_pooled.png",
                         style = "max-width:100%; border-radius:4px;")
              ),
              div(
                tags$p(class = "text-center small fw-bold", "250m Coarse Scale \u2014 Pooled"),
                tags$img(src   = "figures/pred_coarse_pooled.png",
                         style = "max-width:100%; border-radius:4px;")
              )
            ),
            div(
              class = "mt-2",
              tags$p(class = "text-center small fw-bold", "Absolute Difference (Pooled \u2212 FIA-only, 10m)"),
              tags$img(src   = "figures/pred_abs_diff.png",
                       style = "max-width:100%; border-radius:4px;")
            ),
            tags$p(class = "text-muted small mt-2",
              "10m and 250m Pooled (FIA+NEFIN) RF predictions over Chittenden County, VT.",
              " Bottom: absolute difference between Pooled and FIA-only predictions."
            )
          )
        )
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

modeling_server <- function(id, cv_results, fold_results, test_predictions,
                             var_importance, fuzzing_sig, fuzzing_rmse) {
  moduleServer(id, function(input, output, session) {

    # ── Helper: apply perf filters ─────────────────────────────────────────────
    apply_perf_filter <- function(data, model_col = "model_type",
                                   scale_col = "scale", scenario_col = "scenario") {
      if (!is.null(input$perf_model) && input$perf_model != "All")
        data <- dplyr::filter(data, .data[[model_col]] == input$perf_model)
      if (!is.null(input$perf_scale) && input$perf_scale != "All")
        data <- dplyr::filter(data, grepl(input$perf_scale, .data[[scale_col]], ignore.case = TRUE))
      if (!is.null(input$perf_scenario) && input$perf_scenario != "All")
        data <- dplyr::filter(data, .data[[scenario_col]] == input$perf_scenario)
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
        tags$small(best$model_type, "\u2014", best$scale, "\u2014", best$scenario)
      )
    })

    output$best_rmse_val <- renderUI({
      best <- cv_results |>
        dplyr::arrange(test_rmse) |>
        dplyr::slice(1)
      tags$span(
        paste0(round(best$test_rmse, 1), " Mg/ha"),
        tags$br(),
        tags$small(best$model_type, "\u2014", best$scale, "\u2014", best$scenario)
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
        tags$small("Pooled vs. FIA-only (10m)")
      )
    })

    # ── Fold R² boxplot ─────────────────────────────────────────────────────────
    output$fold_r2_box <- plotly::renderPlotly({
      data <- apply_perf_filter(fold_results,
                                 model_col    = "model_type",
                                 scale_col    = "scale",
                                 scenario_col = "scenario")
      req(nrow(data) > 0)

      data <- data |>
        dplyr::mutate(
          group_label = paste0(scale, "\n", scenario),
          hover_text  = paste0(
            "Model: ", model_type, "<br>",
            "Scale: ", scale, "<br>",
            "Scenario: ", scenario, "<br>",
            "Fold: ", fold, "<br>",
            "R\u00b2: ", round(r2, 3)
          )
        )

      p <- ggplot2::ggplot(data,
        ggplot2::aes(x = group_label, y = r2,
                     fill = model_type, text = hover_text)) +
        ggplot2::geom_boxplot(alpha = 0.75, outlier.size = 1.5) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
          values = c("Random Forest" = "#E69F00", "XGBoost" = "#56B4E9"),
          na.value = "#999999"
        ) +
        ggplot2::labs(
          x    = NULL,
          y    = "Cross-Validation R\u00b2",
          fill = "Model"
        ) +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "bottom")

      plotly::ggplotly(p, tooltip = "text")
    })

    # ── Observed vs Predicted scatter ──────────────────────────────────────────
    output$obs_pred <- plotly::renderPlotly({
      data <- apply_perf_filter(
        test_predictions,
        model_col    = "model",
        scale_col    = "scale",
        scenario_col = "scenario"
      )
      req(nrow(data) > 0)

      # Subsample if very large (> 5000 points) for performance
      if (nrow(data) > 5000) {
        set.seed(42)
        data <- dplyr::slice_sample(data, n = 5000)
      }

      data <- data |>
        dplyr::mutate(
          hover_text = paste0(
            "Observed: ", round(observed, 1), " Mg/ha<br>",
            "Predicted: ", round(predicted, 1), " Mg/ha<br>",
            "Error: ", round(abs_error, 1), " Mg/ha<br>",
            "Terrain: ", terrain_class, "<br>",
            "Biomass class: ", biomass_class
          )
        )

      max_val <- max(c(data$observed, data$predicted), na.rm = TRUE)

      p <- ggplot2::ggplot(data,
        ggplot2::aes(x = observed, y = predicted,
                     color = terrain_class, text = hover_text)) +
        ggplot2::geom_point(alpha = 0.45, size = 1.2) +
        ggplot2::geom_abline(slope = 1, intercept = 0,
                             color = "red", linetype = "dashed", linewidth = 0.8) +
        ggplot2::scale_color_viridis_d(option = "turbo", name = "Terrain class") +
        ggplot2::coord_fixed(ratio = 1, xlim = c(0, max_val), ylim = c(0, max_val)) +
        ggplot2::labs(
          x = "Observed Biomass (Mg/ha)",
          y = "Predicted Biomass (Mg/ha)"
        ) +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "right")

      plotly::ggplotly(p, tooltip = "text")
    })

    # ── RMSE comparison chart ──────────────────────────────────────────────────
    output$rmse_comparison <- plotly::renderPlotly({
      # fuzzing_rmse is already long: columns scale, scenario, n, rmse, r2, mae, bias
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
        ggplot2::scale_fill_manual(
          values = c(
            "FIA Only"   = "#E69F00",
            "NEFIN Only" = "#56B4E9",
            "Pooled"     = "#009E73"
          )
        ) +
        ggplot2::labs(
          x    = "Scale",
          y    = "Test RMSE (Mg/ha)",
          fill = "Training Scenario"
        ) +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "bottom")

      plotly::ggplotly(p, tooltip = "text")
    })

    # ── Significance table ─────────────────────────────────────────────────────
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
            ordering = FALSE
          )
        ) |>
        DT::formatStyle(
          "Significant",
          color = DT::styleEqual(c("\u2713 Yes", "No"), c("green", "gray"))
        )
    })

    # ── Variable importance chart ──────────────────────────────────────────────
    output$var_imp <- plotly::renderPlotly({
      # Filter to fine-scale Random Forest pooled (best model class)
      data <- var_importance |>
        dplyr::filter(
          grepl("random.forest|rf|Random Forest", model_type, ignore.case = TRUE),
          grepl("fine|10m", scale, ignore.case = TRUE)
        )

      # Fall back to all RF if no fine scale found
      if (nrow(data) == 0) {
        data <- var_importance |>
          dplyr::filter(grepl("random.forest|rf|Random Forest",
                               model_type, ignore.case = TRUE))
      }

      # Average across scenarios, take top 15
      data <- data |>
        dplyr::group_by(variable) |>
        dplyr::summarise(
          importance_norm = mean(importance_norm, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(importance_norm)) |>
        dplyr::slice_head(n = 15) |>
        dplyr::mutate(
          var_label = dplyr::case_when(
            variable %in% c("canopy_height_10m",
                             "canopy_height",
                             "ETH_canopy_height") ~ "ETH Global Canopy Height 2020",
            variable == "canopy_height_250m"      ~ "ETH Canopy Height 2020 (250m)",
            grepl("^tmean", variable)             ~ paste0("Temp (Daymet V4) \u2014 ",
                                                            sub("^tmean_?", "", variable)),
            grepl("^ppt", variable)               ~ paste0("Precip (Daymet V4) \u2014 ",
                                                            sub("^ppt_?", "", variable)),
            grepl("ndvi_s2",    variable)         ~ paste0("NDVI Sentinel-2 \u2014 ",
                                                            sub("ndvi_s2_?", "", variable)),
            grepl("ndvi_modis", variable)         ~ paste0("NDVI MODIS \u2014 ",
                                                            sub("ndvi_modis_?", "", variable)),
            grepl("^elevation", variable)         ~ paste0("Elevation \u2014 ",
                                                            sub("^elevation_?", "", variable)),
            grepl("^slope",     variable)         ~ paste0("Slope \u2014 ",
                                                            sub("^slope_?", "", variable)),
            TRUE ~ variable
          ),
          hover_text = paste0(
            var_label, "<br>",
            "Importance: ", round(importance_norm, 1), "%"
          )
        )

      p <- ggplot2::ggplot(data,
        ggplot2::aes(
          x    = importance_norm,
          y    = reorder(var_label, importance_norm),
          fill = importance_norm,
          text = hover_text
        )) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::scale_fill_viridis_c(option = "plasma", direction = -1) +
        ggplot2::labs(
          x     = "Normalized Importance (%)",
          y     = NULL,
          title = "Variable Importance \u2014 Fine Scale RF"
        ) +
        theme_fia_nefin()

      plotly::ggplotly(p, tooltip = "text")
    })

  })
}
