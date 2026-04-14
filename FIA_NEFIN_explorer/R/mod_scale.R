# ============================================================================
# mod_scale.R -- Tab 4: Scale Analysis
# ============================================================================

scale_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Row 1: Scale dependency curve (large) + compact selected scale info
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen  = TRUE,
        card_header("Scale-Dependent Augmentation Performance"),
        card_body(
          p(class = "text-muted small",
            "Click any point to explore metrics at that hexagon scale."
          ),
          plotly::plotlyOutput(ns("scale_curve"), height = "450px")
        )
      ),
      tagList(
        card(
          card_header(uiOutput(ns("scale_panel_header"))),
          card_body(
            style = "padding:8px;",
            uiOutput(ns("scale_info_boxes"))
          )
        ),
        card(
          card_body(
            plotly::plotlyOutput(ns("scale_metric_bars"), height = "200px")
          )
        )
      )
    ),

    # Row 2: Bootstrap variance table + chart
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Bootstrap Variance by Dataset"),
        card_body(
          tags$p(class = "text-muted small",
            "Variance of biomass estimates across 1,000 bootstrap replicates."
          ),
          tableOutput(ns("bootstrap_table"))
        )
      ),
      card(
        card_header("All Scale Metrics Comparison"),
        card_body(
          plotly::plotlyOutput(ns("all_metrics_chart"), height = "250px")
        )
      )
    )
  )
}


scale_server <- function(id, scale_metrics, bootstrap_variance) {
  moduleServer(id, function(input, output, session) {

    selected_scale <- reactiveVal(NULL)

    # -- Scale dependency curve ------------------------------------------------
    output$scale_curve <- plotly::renderPlotly({
      data <- scale_metrics |>
        dplyr::arrange(area_ha_num) |>
        dplyr::mutate(
          precision_color = dplyr::case_when(
            grepl("Critical",  precision_score, ignore.case = TRUE) ~ "#D55E00",
            grepl("High",      precision_score, ignore.case = TRUE) ~ "#E69F00",
            TRUE ~ "#56B4E9"
          ),
          hover_text = paste0(
            "<b>Scale:</b> ", scale, "<br>",
            "<b>RMSE:</b> ", round(rmse, 2), " Mg/ha<br>",
            "<b>MAE:</b> ", round(mae, 2), " Mg/ha<br>",
            "<b>% Improved:</b> ", round(pct_improved, 1), "%<br>",
            "<b>Precision:</b> ", precision_score
          )
        )

      p <- ggplot2::ggplot(data,
          ggplot2::aes(x = area_ha_num, y = rmse, text = hover_text)) +
        ggplot2::geom_line(color = "#009E73", linewidth = 1.0, alpha = 0.7) +
        ggplot2::geom_point(
          ggplot2::aes(size = pct_improved, color = precision_score),
          alpha = 0.9
        ) +
        ggplot2::scale_x_log10(
          labels = scales::label_comma(),
          name   = "Hexagon Area (ha, log scale)"
        ) +
        ggplot2::scale_color_manual(
          values = c(
            "Critical"       = "#D55E00",
            "High Value"     = "#E69F00",
            "Moderate Value" = "#56B4E9"
          ),
          na.value = "#999999"
        ) +
        ggplot2::scale_size_continuous(range = c(3, 10), name = "% Improved") +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(0.05, 0.1))
        ) +
        ggplot2::labs(
          y     = "Biomass RMSE (Mg/ha)",
          color = "Precision Tier",
          title = NULL
        ) +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "right")

      plotly::ggplotly(p, tooltip = "text", source = "scale_curve") |>
        plotly::event_register("plotly_click") |>
        plotly::layout(
          xaxis = list(type = "log", autorange = TRUE),
          yaxis = list(autorange = TRUE),
          legend = list(orientation = "v")
        )
    })

    # React to plotly click - update selected_scale
    observeEvent(plotly::event_data("plotly_click", source = "scale_curve"), {
      click <- plotly::event_data("plotly_click", source = "scale_curve")
      req(!is.null(click))
      x_val <- click$x
      # plotly log axis may return log10 value; try both raw and 10^x
      idx_raw <- which.min(abs(scale_metrics$area_ha_num - x_val))
      idx_log <- which.min(abs(scale_metrics$area_ha_num - 10^x_val))
      err_raw <- abs(scale_metrics$area_ha_num[idx_raw] - x_val)
      err_log <- abs(scale_metrics$area_ha_num[idx_log] - 10^x_val)
      idx <- if (err_log < err_raw) idx_log else idx_raw
      if (length(idx) > 0) selected_scale(scale_metrics$scale[idx])
    })

    # Panel header
    output$scale_panel_header <- renderUI({
      sc <- selected_scale()
      if (is.null(sc)) "Click a point on the curve to explore a scale"
      else paste0("Scale: ", sc)
    })

    # Info boxes for selected scale
    output$scale_info_boxes <- renderUI({
      sc <- selected_scale()
      if (is.null(sc)) {
        return(tags$p(class = "text-muted small",
          "Click a point on the scale curve."))
      }
      row <- dplyr::filter(scale_metrics, scale == sc)
      if (nrow(row) == 0) return(NULL)

      tags$table(
        class = "table table-sm mb-0",
        style = "font-size:0.85em; color:#e2e8f0;",
        tags$tr(tags$td("RMSE"), tags$td(style = "text-align:right; font-weight:bold;",
          paste0(round(row$rmse, 1), " Mg/ha"))),
        tags$tr(tags$td("MAE"), tags$td(style = "text-align:right; font-weight:bold;",
          paste0(round(row$mae, 1), " Mg/ha"))),
        tags$tr(tags$td("% Improved"), tags$td(style = "text-align:right; font-weight:bold; color:#10b981;",
          paste0(round(row$pct_improved, 1), "%"))),
        tags$tr(tags$td("N Hexagons"), tags$td(style = "text-align:right; font-weight:bold;",
          scales::comma(row$n_hexes)))
      )
    })

    # Bar chart of metrics at selected scale
    output$scale_metric_bars <- plotly::renderPlotly({
      sc <- selected_scale()
      if (is.null(sc)) {
        # Show all scales as a simple bar chart of RMSE
        p <- ggplot2::ggplot(
          scale_metrics |> dplyr::arrange(area_ha_num),
          ggplot2::aes(x = reorder(scale, area_ha_num), y = rmse,
                       fill = pct_improved,
                       text = paste0("Scale: ", scale, "\nRMSE: ", round(rmse, 1)))
        ) +
          ggplot2::geom_col() +
          ggplot2::scale_fill_viridis_c(name = "% Improved") +
          ggplot2::labs(x = "Scale", y = "RMSE (Mg/ha)",
                        title = "RMSE by Scale") +
          ggplot2::coord_flip() +
          theme_fia_nefin()
        return(plotly::ggplotly(p, tooltip = "text"))
      }

      # Show comparative metrics for selected scale
      row <- dplyr::filter(scale_metrics, scale == sc)
      if (nrow(row) == 0) return(NULL)

      metric_data <- tibble::tibble(
        Metric = c("RMSE", "MAE", "NDVI RMSE", "Temp RMSE", "Precip RMSE"),
        Value  = c(row$rmse, row$mae, row$ndvi_rmse, row$temp_rmse, row$ppt_rmse),
        Group  = c("Biomass", "Biomass", "Covariate", "Covariate", "Covariate")
      )

      p <- ggplot2::ggplot(metric_data,
        ggplot2::aes(x = reorder(Metric, -Value), y = Value,
                     fill = Group,
                     text = paste0(Metric, ": ", round(Value, 3)))) +
        ggplot2::geom_col(alpha = 0.85) +
        ggplot2::scale_fill_manual(
          values = c("Biomass" = "#009E73", "Covariate" = "#56B4E9")
        ) +
        ggplot2::labs(x = NULL, y = "Error metric",
                      title = paste0("Metrics at ", sc)) +
        theme_fia_nefin()

      plotly::ggplotly(p, tooltip = "text")
    })

    # -- Bootstrap variance ----------------------------------------------------
    output$bootstrap_table <- renderTable({
      if (is.null(bootstrap_variance)) {
        return(data.frame(
          Dataset = "No data",
          `Bootstrap Variance` = NA,
          `Change from Baseline` = NA,
          `Change (%)` = NA,
          check.names = FALSE
        ))
      }
      bootstrap_variance |>
        dplyr::mutate(
          `Bootstrap Variance` = round(var_bootstrap, 4),
          `Change from Baseline` = round(delta_var, 4),
          `Change (%)` = round(delta_var_pct, 2)
        ) |>
        dplyr::select(Dataset = dataset,
                      `Bootstrap Variance`, `Change from Baseline`, `Change (%)`)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # -- All-scales metrics chart ----------------------------------------------
    output$all_metrics_chart <- plotly::renderPlotly({
      data <- scale_metrics |>
        dplyr::arrange(area_ha_num) |>
        dplyr::select(scale, area_ha_num, rmse, pct_improved, rel_improvement) |>
        tidyr::pivot_longer(
          cols      = c(rmse, pct_improved),
          names_to  = "metric",
          values_to = "value"
        ) |>
        dplyr::mutate(
          metric_label = dplyr::if_else(metric == "rmse",
                                        "RMSE (Mg/ha)", "% Hexagons Improved")
        )

      p <- ggplot2::ggplot(data,
        ggplot2::aes(x = area_ha_num, y = value,
                     color = metric_label, group = metric_label,
                     text = paste0(scale, ": ", round(value, 1)))) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2) +
        ggplot2::scale_x_log10(labels = scales::label_comma()) +
        ggplot2::scale_color_manual(
          values = c("RMSE (Mg/ha)" = "#D55E00", "% Hexagons Improved" = "#009E73")
        ) +
        ggplot2::facet_wrap(~metric_label, scales = "free_y", nrow = 1) +
        ggplot2::labs(x = "Area (ha, log)", y = NULL, color = NULL) +
        theme_fia_nefin() +
        ggplot2::theme(legend.position = "none",
                       strip.text = ggplot2::element_text(size = 9))

      plotly::ggplotly(p, tooltip = "text")
    })

  })
}
