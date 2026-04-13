# ============================================================================
# mod_modeling.R -- Tab 5: Modeling Results
# Three sub-tabs: Performance, Training Comparison, Predictions
# ============================================================================

# -- UI ------------------------------------------------------------------------

modeling_ui <- function(id) {
  ns <- NS(id)

  navset_card_tab(

    # Sub-tab 1: Performance ---------------------------------------------------
    nav_panel(
      title = tagList(bsicons::bs_icon("graph-up"), " Performance"),
      value = "perf",
      tagList(
        # Subtle value boxes -- neutral slate with left accent
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(
            title    = "Best Test R\u00b2",
            value    = uiOutput(ns("best_r2_val")),
            showcase = bsicons::bs_icon("bullseye"),
            theme    = value_box_theme(bg = "#1e293b", fg = "#e2e8f0")
          ),
          value_box(
            title    = "Best Test RMSE",
            value    = uiOutput(ns("best_rmse_val")),
            showcase = bsicons::bs_icon("rulers"),
            theme    = value_box_theme(bg = "#1e293b", fg = "#e2e8f0")
          ),
          value_box(
            title    = "Pooled Gain (10m RF)",
            value    = uiOutput(ns("pooled_gain_val")),
            showcase = bsicons::bs_icon("arrow-up-circle"),
            theme    = value_box_theme(bg = "#1a3a36", fg = "#e2e8f0")
          )
        ),

        # Filter row -- choices populated dynamically in server
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
            card_header(
              "CV Fold R\u00b2 by Scenario ",
              tags$span(
                title = paste0(
                  "R-squared from spatial block cross-validation (25km blocks, 10km buffer, 10 folds). ",
                  "Each point is one fold. Higher = better fit. ",
                  "NEFIN Only has high CV R2 because its small, homogeneous training set ",
                  "is easy to fit but generalizes poorly to the broader landscape."
                ),
                style = "cursor:help; color:#64748b;",
                bsicons::bs_icon("info-circle")
              )
            ),
            card_body(plotly::plotlyOutput(ns("fold_r2_box"), height = "420px"))
          ),
          card(
            card_header(
              "Residuals vs. Observed Biomass ",
              tags$span(
                title = paste0(
                  "Residual = predicted minus observed. Points above zero = overprediction, below = underprediction. ",
                  "The loess curve reveals systematic bias structure: FIA models increasingly underpredict ",
                  "as true biomass increases because FIA training data lacks high-biomass examples."
                ),
                style = "cursor:help; color:#64748b;",
                bsicons::bs_icon("info-circle")
              )
            ),
            card_body(
              plotly::plotlyOutput(ns("residual_plot"), height = "400px")
            )
          )
        )
      )
    ),

    # Sub-tab 2: Training Comparison -------------------------------------------
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
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("CV Fold R\u00b2 by Scenario"),
          card_body(plotly::plotlyOutput(ns("cv_r2_box"), height = "380px"))
        ),
        card(
          card_header("CV Fold RMSE by Scenario"),
          card_body(plotly::plotlyOutput(ns("cv_rmse_box"), height = "380px"))
        )
      )
    ),

    # Sub-tab 3: Interpretability (inner tabs for full-width content) ---------
    nav_panel(
      title = tagList(bsicons::bs_icon("diagram-3"), " Interpretability"),
      value = "interpretability",

      navset_card_tab(
        id = ns("interp_tabs"),

        # Inner tab: Variable Importance
        nav_panel(
          title = tagList("Variable Importance ",
            tags$span(
              title = paste0(
                "Shows normalized variable importance (IncNodePurity) as % of the maximum within each model. ",
                "Canopy height anchors at 100% because it is the single most important predictor in every model, ",
                "explaining more variance than any other covariate. All other variables are shown relative to it. ",
                "Multiple dots per variable represent different training scenarios (FIA Only, Pooled, NEFIN Only), ",
                "showing how predictor rankings shift depending on training data composition."
              ),
              style = "cursor:help; color:#64748b;",
              bsicons::bs_icon("info-circle")
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            selectInput(ns("imp_model"), "Model:",
              choices = "All", selected = "All"),
            selectInput(ns("imp_scale"), "Scale:",
              choices = "All", selected = "All")
          ),
          plotly::plotlyOutput(ns("var_imp"), height = "550px")
        ),

        # Inner tab: Error by Biomass Quartile
        nav_panel(
          title = tagList("Error by Biomass ",
            tags$span(
              title = paste0(
                "RMSE computed separately for each biomass quartile (Q1=lowest, Q4=highest) of the test set. ",
                "Reveals where each training scenario performs best and worst. ",
                "FIA-trained models underpredict Q4 because FIA undersamples high-biomass stands. ",
                "NEFIN-trained models overpredict Q1 because NEFIN oversamples high-biomass stands."
              ),
              style = "cursor:help; color:#64748b;",
              bsicons::bs_icon("info-circle")
            )
          ),
          plotly::plotlyOutput(ns("error_by_biomass"), height = "450px")
        ),

        # Inner tab: Error by Terrain
        nav_panel(
          title = tagList("Error by Terrain ",
            tags$span(
              title = paste0(
                "RMSE by terrain class derived from slope. ",
                "Steep terrain amplifies FIA coordinate fuzzing effects because a 1-mile displacement ",
                "on steep slopes crosses larger environmental gradients than on flat terrain. ",
                "Pooled models reduce this effect by adding precisely-located NEFIN plots."
              ),
              style = "cursor:help; color:#64748b;",
              bsicons::bs_icon("info-circle")
            )
          ),
          plotly::plotlyOutput(ns("error_by_terrain"), height = "450px")
        )
      )
    )
  )
}


# -- Server --------------------------------------------------------------------

modeling_server <- function(id, cv_results, fold_results, test_predictions,
                             var_importance, fuzzing_sig, fuzzing_rmse) {
  moduleServer(id, function(input, output, session) {

    # -- Populate filter dropdowns from actual data -----------------------------
    observe({
      model_col <- if ("model_name" %in% names(fold_results)) "model_name" else "model_type"
      model_vals <- sort(unique(fold_results[[model_col]]))
      updateSelectInput(session, "perf_model",
        choices = c("All", setNames(model_vals, model_vals)))

      scale_vals <- sort(unique(fold_results$scale))
      updateSelectInput(session, "perf_scale",
        choices = c("All", setNames(scale_vals, scale_vals)))

      scenario_vals <- unique(fold_results$scenario)
      updateSelectInput(session, "perf_scenario",
        choices = c("All", setNames(scenario_vals, scenario_vals)))
    })

    # -- Helper: apply perf filters ---------------------------------------------
    apply_perf_filter <- function(data, model_col = "model_type",
                                   scale_col = "scale", scenario_col = "scenario") {
      if (!is.null(input$perf_model) && input$perf_model != "All") {
        model_pattern <- dplyr::case_when(
          grepl("Random Forest", input$perf_model, ignore.case = TRUE) ~ "^rf",
          grepl("XGBoost",       input$perf_model, ignore.case = TRUE) ~ "^xgb",
          TRUE ~ input$perf_model
        )
        if ("model_name" %in% names(data)) {
          data <- dplyr::filter(data,
            .data[["model_name"]] == input$perf_model |
            grepl(model_pattern, .data[[model_col]], ignore.case = TRUE))
        } else {
          data <- dplyr::filter(data,
            grepl(model_pattern, .data[[model_col]], ignore.case = TRUE))
        }
      }
      if (!is.null(input$perf_scale) && input$perf_scale != "All") {
        data <- dplyr::filter(data,
          .data[[scale_col]] == input$perf_scale |
          grepl(sub(".*\\((.+)\\).*", "\\1", input$perf_scale),
                .data[[scale_col]], fixed = TRUE))
      }
      if (!is.null(input$perf_scenario) && input$perf_scenario != "All") {
        data <- dplyr::filter(data, .data[[scenario_col]] == input$perf_scenario)
      }
      data
    }

    # -- Value boxes ------------------------------------------------------------
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

    # -- Fold R^2 boxplot -- colored by scenario (matching manuscript) ------------
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

    # -- Residual plot -- replaces obs vs pred (matches manuscript Fig S2) ------
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
        ggplot2::aes(x = observed, y = residual_val)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            color = SLATE_MUTED, linewidth = 0.6) +
        ggplot2::geom_point(ggplot2::aes(color = scenario, text = hover_text),
                            alpha = 0.5, size = 1.5) +
        ggplot2::geom_smooth(ggplot2::aes(color = scenario),
                             method = "loess", se = FALSE, linewidth = 1.2,
                             inherit.aes = TRUE) +
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

    # -- RMSE comparison chart -- scenario colors ------------------------------
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

    # -- Significance table -- dark theme --------------------------------------
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

    # -- CV Fold R2 boxplot (Training Comparison tab) ----------------------------
    output$cv_r2_box <- plotly::renderPlotly({
      req(nrow(fold_results) > 0)

      data <- fold_results |>
        dplyr::mutate(
          scenario = factor(scenario, levels = c("FIA Only", "NEFIN Only", "Pooled")),
          hover_text = paste0(
            "Scale: ", scale, "\n",
            "Scenario: ", scenario, "\n",
            "Fold: ", fold, "\n",
            "R2: ", round(r2, 3)
          )
        )

      p <- ggplot2::ggplot(data,
        ggplot2::aes(x = scenario, y = r2,
                     fill = scenario, text = hover_text)) +
        ggplot2::geom_boxplot(alpha = 0.8, outlier.shape = NA) +
        ggplot2::geom_jitter(width = 0.15, size = 1.5, alpha = 0.6,
                             color = SLATE_TEXT) +
        ggplot2::scale_fill_manual(values = SCENARIO_COLORS) +
        ggplot2::facet_wrap(~scale, nrow = 1) +
        ggplot2::labs(x = NULL, y = "Fold R2") +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "none")

      plotly::ggplotly(p, tooltip = "text") |>
        plotly_dark_layout()
    })

    # -- CV Fold RMSE boxplot (Training Comparison tab) ------------------------
    output$cv_rmse_box <- plotly::renderPlotly({
      req(nrow(fold_results) > 0)

      data <- fold_results |>
        dplyr::mutate(
          scenario = factor(scenario, levels = c("FIA Only", "NEFIN Only", "Pooled")),
          hover_text = paste0(
            "Scale: ", scale, "\n",
            "Scenario: ", scenario, "\n",
            "Fold: ", fold, "\n",
            "RMSE: ", round(rmse, 1), " Mg/ha"
          )
        )

      p <- ggplot2::ggplot(data,
        ggplot2::aes(x = scenario, y = rmse,
                     fill = scenario, text = hover_text)) +
        ggplot2::geom_boxplot(alpha = 0.8, outlier.shape = NA) +
        ggplot2::geom_jitter(width = 0.15, size = 1.5, alpha = 0.6,
                             color = SLATE_TEXT) +
        ggplot2::scale_fill_manual(values = SCENARIO_COLORS) +
        ggplot2::facet_wrap(~scale, nrow = 1) +
        ggplot2::labs(x = NULL, y = "RMSE (Mg/ha)") +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "none")

      plotly::ggplotly(p, tooltip = "text") |>
        plotly_dark_layout()
    })

    # -- Populate importance filter dropdowns ------------------------------------
    observe({
      model_col <- if ("model_name" %in% names(var_importance)) "model_name" else "model_type"
      model_vals <- sort(unique(var_importance[[model_col]]))
      updateSelectInput(session, "imp_model",
        choices = c("All", setNames(model_vals, model_vals)))

      scale_vals <- sort(unique(var_importance$scale))
      updateSelectInput(session, "imp_scale",
        choices = c("All", setNames(scale_vals, scale_vals)))
    })

    # -- Error by Biomass Quartile (dynamic from test_predictions) --------------
    output$error_by_biomass <- plotly::renderPlotly({
      req(nrow(test_predictions) > 0)

      data <- test_predictions |>
        dplyr::filter(!is.na(biomass_class), !is.na(abs_error)) |>
        dplyr::mutate(
          scenario = dplyr::case_when(
            grepl("fia",   model, ignore.case = TRUE) ~ "FIA Only",
            grepl("nefin", model, ignore.case = TRUE) ~ "NEFIN Only",
            grepl("pool",  model, ignore.case = TRUE) ~ "Pooled",
            TRUE ~ scenario
          ),
          scenario = factor(scenario, levels = c("FIA Only", "NEFIN Only", "Pooled"))
        )

      # RMSE by biomass class, scenario, and scale
      error_summary <- data |>
        dplyr::group_by(biomass_class, scenario, scale) |>
        dplyr::summarise(
          rmse = sqrt(mean(abs_error^2, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          hover_text = paste0(
            "Quartile: ", biomass_class, "\n",
            "Scenario: ", scenario, "\n",
            "Scale: ", scale, "\n",
            "RMSE: ", round(rmse, 1), " Mg/ha"
          )
        )

      p <- ggplot2::ggplot(error_summary,
        ggplot2::aes(x = biomass_class, y = rmse,
                     fill = scenario, text = hover_text)) +
        ggplot2::geom_col(position = "dodge", alpha = 0.85) +
        ggplot2::scale_fill_manual(values = SCENARIO_COLORS) +
        ggplot2::facet_wrap(~scale, nrow = 1) +
        ggplot2::labs(
          x = "Biomass Quartile",
          y = "RMSE (Mg/ha)",
          fill = "Scenario"
        ) +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "bottom")

      plotly::ggplotly(p, tooltip = "text") |>
        plotly_dark_layout()
    })

    # -- Error by Terrain Class (dynamic from test_predictions) -----------------
    output$error_by_terrain <- plotly::renderPlotly({
      req(nrow(test_predictions) > 0)

      data <- test_predictions |>
        dplyr::filter(!is.na(terrain_class), !is.na(abs_error)) |>
        dplyr::mutate(
          scenario = dplyr::case_when(
            grepl("fia",   model, ignore.case = TRUE) ~ "FIA Only",
            grepl("nefin", model, ignore.case = TRUE) ~ "NEFIN Only",
            grepl("pool",  model, ignore.case = TRUE) ~ "Pooled",
            TRUE ~ scenario
          ),
          scenario = factor(scenario, levels = c("FIA Only", "NEFIN Only", "Pooled"))
        )

      error_summary <- data |>
        dplyr::group_by(terrain_class, scenario, scale) |>
        dplyr::summarise(
          rmse = sqrt(mean(abs_error^2, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          hover_text = paste0(
            "Terrain: ", terrain_class, "\n",
            "Scenario: ", scenario, "\n",
            "Scale: ", scale, "\n",
            "RMSE: ", round(rmse, 1), " Mg/ha"
          )
        )

      p <- ggplot2::ggplot(error_summary,
        ggplot2::aes(x = terrain_class, y = rmse,
                     fill = scenario, text = hover_text)) +
        ggplot2::geom_col(position = "dodge", alpha = 0.85) +
        ggplot2::scale_fill_manual(values = SCENARIO_COLORS) +
        ggplot2::facet_wrap(~scale, nrow = 1) +
        ggplot2::labs(
          x = "Terrain Class",
          y = "RMSE (Mg/ha)",
          fill = "Scenario"
        ) +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "bottom")

      plotly::ggplotly(p, tooltip = "text") |>
        plotly_dark_layout()
    })

    # -- Variable importance -- per-scenario dot plot with model/scale filters --
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
