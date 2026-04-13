# ============================================================================
# mod_spatial.R -- Tab 3: Spatial Explorer
# Three sub-tabs: A) Plot Locations & Fuzzing, B) Hexagon Overview,
#                 C) Chittenden Detail (10m Predictions)
# ============================================================================

# -- UI ------------------------------------------------------------------------

spatial_ui <- function(id) {
  ns <- NS(id)

  navset_card_tab(
    # Sub-tab A ----------------------------------------------------------------
    nav_panel(
      title = tagList(bsicons::bs_icon("geo"), " Plot Locations & Fuzzing"),
      value = "subtab_plots",
      layout_sidebar(
        sidebar = sidebar(
          width = 290,
          h5("FIA Settings", class = "text-primary mt-0 mb-2"),
          sliderInput(ns("n_fia"), "Sample FIA plots:",
                      min = 100, max = 2000, value = 500, step = 100),
          sliderInput(ns("fuzz_radius"), "Uncertainty radius (km):",
                      min = 0.5, max = 1.6, value = 1.0, step = 0.1),
          hr(),
          h5("NEFIN Color", class = "text-primary mt-0 mb-2"),
          radioButtons(ns("nefin_color"), NULL,
            choices = c(
              "Measurement Year" = "measyear",
              "Biomass (Mg/ha)"  = "biomass",
              "State"            = "state"
            ),
            selected = "measyear"
          ),
          hr(),
          tags$small(class = "text-muted",
            "FIA coordinates are pre-fuzzed up to 1 mile. Circles show the",
            "covariate extraction uncertainty radius, not the displacement itself."
          )
        ),
        # Main content
        leafletOutput(ns("plot_map"), height = "500px"),
        uiOutput(ns("mc_panel"))
      )
    ),

    # Sub-tab B ----------------------------------------------------------------
    nav_panel(
      title = tagList(bsicons::bs_icon("hexagon"), " Hexagon Overview"),
      value = "subtab_hex",
      layout_sidebar(
        sidebar = sidebar(
          width = 290,
          h5("Hexagon Scale", class = "text-primary mt-0 mb-2"),
          selectInput(ns("hex_scale"), NULL,
            choices = c(
              "100 ha (Warning: slow)" = "hex_100ha.geojson",
              "500 ha"                 = "hex_500ha.geojson",
              "1 kha"                  = "hex_1kha.geojson",
              "2.4 kha"                = "hex_2_4kha.geojson",
              "5 kha"                  = "hex_5kha.geojson",
              "10 kha"                 = "hex_10kha.geojson",
              "50 kha"                 = "hex_50kha.geojson",
              "64 kha"                 = "hex_64kha.geojson",
              "100 kha"                = "hex_100kha.geojson"
            ),
            selected = "hex_64kha.geojson"
          ),
          hr(),
          h5("Display Layer", class = "text-primary mt-0 mb-2"),
          radioButtons(ns("hex_layer"), NULL,
            choices = c(
              "Biomass  - FIA (Mg/ha)"       = "fia_biomass_mean",
              "Biomass  - Augmented (Mg/ha)"  = "aug_biomass_mean",
              "Difference (Aug  - FIA)"       = "biomass_change",
              "NEFIN % of plots"                 = "pct_nefin",
              "Total plot density"               = "n_plots_total"
            ),
            selected = "aug_biomass_mean"
          ),
          hr(),
          sliderInput(ns("min_plots"), "Min. plots per hex:",
                      min = 1, max = 20, value = 1, step = 1),
          checkboxInput(ns("show_states"), "Show state boundaries", value = TRUE)
        ),
        leafletOutput(ns("hex_map"), height = "520px"),
        uiOutput(ns("hex_summary_strip"))
      )
    ),

    # Sub-tab C ----------------------------------------------------------------
    nav_panel(
      title = tagList(bsicons::bs_icon("layers"), " Chittenden Detail (10m)"),
      value = "subtab_chittenden",
      tags$script(HTML("
        Shiny.addCustomMessageHandler('show-spinner', function(id) {
          $('#' + id).stop(true).fadeIn(200);
        });
      ")),
      layout_sidebar(
        sidebar = sidebar(
          width = 260,
          h5("Prediction Panels", class = "text-primary mt-0 mb-2"),
          selectInput(ns("scenario_a"), "Top-Left:",
            choices  = c("FIA Only" = "fia_only",
                         "NEFIN Only" = "nefin_only",
                         "Pooled (FIA+NEFIN)" = "pooled"),
            selected = "fia_only"
          ),
          selectInput(ns("scenario_b"), "Top-Right:",
            choices  = c("FIA Only" = "fia_only",
                         "NEFIN Only" = "nefin_only",
                         "Pooled (FIA+NEFIN)" = "pooled"),
            selected = "pooled"
          ),
          sliderInput(ns("opacity_pred"), "Prediction Opacity:",
                      min = 0.1, max = 1.0, value = 0.8, step = 0.1),
          hr(),
          h5("Covariate Layer", class = "text-primary mt-0 mb-2"),
          selectInput(ns("covariate_layer"), "Variable:",
            choices = c(
              "Canopy Height"       = "fine_canopy_height_chittenden_display.tif",
              "Elevation"           = "fine_elevation_chittenden_display.tif",
              "Slope"               = "fine_slope_chittenden_display.tif",
              "Aspect"              = "fine_aspect_chittenden_display.tif",
              "NDVI (Sentinel-2)"   = "fine_ndvi_chittenden_display.tif",
              "NDWI (Sentinel-2)"   = "fine_ndwi_chittenden_display.tif",
              "EVI (Sentinel-2)"    = "fine_evi_chittenden_display.tif",
              "NBR (Sentinel-2)"    = "fine_nbr_chittenden_display.tif",
              "Mean Temperature"    = "fine_tmean_chittenden_display.tif",
              "Min Temperature"     = "fine_tmin_chittenden_display.tif",
              "Max Temperature"     = "fine_tmax_chittenden_display.tif",
              "Precipitation"       = "fine_ppt_chittenden_display.tif"
            ),
            selected = "fine_canopy_height_chittenden_display.tif"
          ),
          sliderInput(ns("opacity_cov"), "Covariate Opacity:",
                      min = 0.1, max = 1.0, value = 0.8, step = 0.1),
          hr(),
          h5("Display", class = "text-primary mt-0 mb-2"),
          radioButtons(ns("basemap"), "Basemap:",
            choices = c(
              "Light (CartoDB)"  = "CartoDB.Positron",
              "Satellite (ESRI)" = "Esri.WorldImagery",
              "Topo (OSM)"       = "OpenTopoMap"
            ),
            selected = "CartoDB.Positron"
          ),
          tags$small(class = "text-muted",
            "All 4 panels are synchronized: pan/zoom one to move all."
          ),
          hr(),
          uiOutput(ns("chittenden_stats"))
        ),
        # Main content: 4-panel synced maps
        div(
          class = "position-relative",
          div(
            id = ns("map_spinner"),
            class = "position-absolute top-50 start-50 translate-middle text-center",
            style = "z-index:999;",
            div(class = "spinner-border text-primary", role = "status",
                style = "width:3rem; height:3rem;"),
            tags$p("Loading rasters...", class = "mt-2 text-muted")
          ),
          uiOutput(ns("sync_maps"))
        )
      )
    )
  )
}


# -- Server --------------------------------------------------------------------

spatial_server <- function(id, fia_plots, nefin_plots, plot_uncertainty,
                            hex_1kha, states_sf, cv_results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Sub-tab A: Plot Locations & Fuzzing -----------------------------------

    # FIA subsample (fixed seed for reproducibility)
    fia_sample <- reactive({
      set.seed(42)
      n <- min(input$n_fia, nrow(fia_plots))
      fia_plots[sample(nrow(fia_plots), n), ]
    })

    # Render base map once
    output$plot_map <- renderLeaflet({
      leaflet() |>
        addProviderTiles("CartoDB.Positron") |>
        setView(lng = -72.0, lat = 44.2, zoom = 6)
    })

    # Update FIA markers + uncertainty circles
    observe({
      data <- fia_sample()

      proxy <- leafletProxy("plot_map", session)
      proxy |>
        clearGroup("fia_circles") |>
        clearGroup("fia_points")

      proxy |>
        addCircles(
          data    = data,
          lng     = ~lon,
          lat     = ~lat,
          radius  = input$fuzz_radius * 1000,  # km -> m
          color   = "#E69F00",
          fill    = FALSE,
            opacity = 0.25,
            weight  = 1,
            group   = "fia_circles"
          ) |>
          addCircleMarkers(
            data         = data,
            lng          = ~lon,
            lat          = ~lat,
            radius       = 4,
            color        = "#E69F00",
            fillColor    = "#E69F00",
            fillOpacity  = 0.7,
            stroke       = FALSE,
            group        = "fia_points",
            layerId      = ~as.character(CN),
            popup        = ~paste0(
              "<b>FIA Plot</b> (pre-fuzzed)<br>",
              "CN: ", CN, "<br>",
              "State: ", state, "<br>",
              "Year: ", MEASYEAR, "<br>",
              "Biomass: ", round(biomass, 1), " Mg/ha<br>",
              "<i>Click for Monte Carlo uncertainty</i>"
            )
          )
    }) |>
      bindEvent(fia_sample(), input$fuzz_radius)

    # Update NEFIN markers
    observe({
      proxy <- leafletProxy("plot_map", session)
      proxy |> clearGroup("nefin_points")

      color_col <- input$nefin_color
      pal <- switch(color_col,
        measyear = colorNumeric("viridis", domain = unname(nefin_plots$MEASYEAR), reverse = FALSE),
        biomass  = colorNumeric("viridis", domain = unname(nefin_plots$biomass),  na.color = "#ccc"),
        state    = colorFactor("Set1",     levels = unique(nefin_plots$state))
      )
      colors <- switch(color_col,
        measyear = pal(nefin_plots$MEASYEAR),
        biomass  = pal(nefin_plots$biomass),
        state    = pal(nefin_plots$state)
      )
      legend_title <- switch(color_col,
        measyear = "Meas. Year",
        biomass  = "Biomass",
        state    = "State"
      )

      proxy |>
        addCircleMarkers(
          data        = nefin_plots,
          lng         = ~lon,
          lat         = ~lat,
          radius      = 5,
          color       = colors,
          fillColor   = colors,
          fillOpacity = 0.8,
          stroke      = FALSE,
          group       = "nefin_points",
          popup       = ~paste0(
            "<b>NEFIN Plot</b><br>",
            "CN: ", CN, "<br>",
            "State: ", state, "<br>",
            "Year: ", MEASYEAR, "<br>",
            "Biomass: ", round(biomass, 1), " Mg/ha"
          )
        ) |>
        clearControls() |>
        addLegend(
          position = "bottomright",
          pal      = pal,
          values   = switch(color_col,
            measyear = nefin_plots$MEASYEAR,
            biomass  = nefin_plots$biomass,
            state    = nefin_plots$state
          ),
          title    = paste0("NEFIN\n", legend_title),
          opacity  = 0.85
        )
    }) |>
      bindEvent(input$nefin_color, ignoreInit = FALSE)

    # Monte Carlo panel -- triggered by FIA marker click
    selected_cn <- reactiveVal(NULL)

    observeEvent(input$plot_map_marker_click, {
      click <- input$plot_map_marker_click
      req(click$id)
      selected_cn(as.character(click$id))
    })

    output$mc_panel <- renderUI({
      req(selected_cn())
      cn  <- selected_cn()
      unc <- dplyr::filter(plot_uncertainty, CN == cn)
      req(nrow(unc) > 0)

      plot_info <- dplyr::filter(fia_plots, CN == cn)
      plot_label <- if (nrow(plot_info) > 0) {
        paste0("Plot CN ", cn, " -- ", plot_info$state[1], ", Year ", plot_info$MEASYEAR[1])
      } else {
        paste0("Plot CN ", cn)
      }

      card(
        card_header(paste0("Monte Carlo Coordinate Uncertainty  - ", plot_label)),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            div(
              plotlyOutput(ns("mc_chart"), height = "260px")
            ),
            div(
              tags$img(
                src   = "figures/Fig3_Monte_Carlo.png",
                style = "max-width: 100%; border-radius: 4px;"
              )
            )
          ),
          tags$p(
            class = "text-muted small mt-2",
            "Coordinate fuzzing of up to 1 mile means the true covariate value at",
            "this plot could range across the uncertainty shown. Each bar shows the",
            "standard deviation of the covariate extracted across 100 Monte Carlo",
            "jitter replicates within the fuzz radius. NDVI (Sentinel-2) shows the",
            "largest spread (mean \u03c3 = 0.08, max = 0.41)."
          )
        )
      )
    })

    output$mc_chart <- renderPlotly({
      req(selected_cn())
      cn  <- selected_cn()
      unc <- dplyr::filter(plot_uncertainty, CN == cn)
      req(nrow(unc) > 0)

      # Compute tmean and ppt min/max from mean +/- range/2
      bar_data <- tibble::tibble(
        covariate = c(
          "NDVI (Sentinel-2)",
          "NDVI (MODIS)",
          "Temperature (Daymet V4)",
          "Precipitation (Daymet V4)"
        ),
        sd_val  = c(
          unc$ndvi_s2_sd[1],
          unc$ndvi_modis_sd[1],
          unc$tmean_sd[1],
          unc$ppt_sd[1]
        ),
        min_val = c(
          unc$ndvi_s2_min[1],
          unc$ndvi_modis_min[1],
          unc$tmean_mean[1] - unc$tmean_range[1] / 2,
          unc$ppt_mean[1]   - unc$ppt_range[1]   / 2
        ),
        max_val = c(
          unc$ndvi_s2_max[1],
          unc$ndvi_modis_max[1],
          unc$tmean_mean[1] + unc$tmean_range[1] / 2,
          unc$ppt_mean[1]   + unc$ppt_range[1]   / 2
        )
      ) |>
        dplyr::mutate(
          covariate = factor(covariate, levels = rev(covariate)),
          hover_text = paste0(
            covariate, "\n",
            "SD: ", round(sd_val, 3), "\n",
            "Range: [", round(min_val, 2), ", ", round(max_val, 2), "]"
          )
        )

      p <- ggplot2::ggplot(bar_data,
        ggplot2::aes(x = covariate, y = sd_val, fill = sd_val,
                     text = hover_text)) +
        ggplot2::geom_col(show.legend = FALSE) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_viridis_c(option = "plasma", direction = -1) +
        ggplot2::labs(
          x     = NULL,
          y     = "Std. Dev. across 100 MC replicates",
          title = "Covariate Uncertainty"
        ) +
        theme_fia_nefin()

      plotly::ggplotly(p, tooltip = "text")
    })


    # -- Sub-tab B: Hexagon Overview -------------------------------------------

    # Cache for loaded GeoJSON scales (reactiveValues scoped to this module)
    hex_cache <- reactiveValues()
    hex_cache[["hex_1kha.geojson"]] <- hex_1kha  # pre-loaded sf object

    # Reactive: current hex data (with lazy loading and caching)
    hex_data <- reactive({
      fname <- input$hex_scale
      if (is.null(isolate(hex_cache[[fname]]))) {
        showNotification(
          paste0("Loading hexagon layer (", fname, ")..."),
          id       = "hex_load_notif",
          type     = "message",
          duration = NULL
        )
        tryCatch({
          hex_cache[[fname]] <- sf::st_read(
            file.path(HEX_SRC_PATH, fname),
            quiet = TRUE
          )
        }, error = function(e) {
          showNotification(paste("Error loading hexagons:", e$message),
                           type = "error", duration = 10)
          return(NULL)
        })
        removeNotification("hex_load_notif")
      }
      hex_cache[[fname]]
    })

    # Filtered hexagons
    filtered_hex <- reactive({
      req(hex_data())
      hex_data() |> dplyr::filter(n_plots_total >= input$min_plots)
    })

    # Color palette
    hex_pal <- reactive({
      req(filtered_hex())
      layer <- input$hex_layer
      data  <- filtered_hex()
      vals  <- data[[layer]]
      if (layer == "biomass_change") {
        lim <- unname(max(abs(vals), na.rm = TRUE))
        leaflet::colorNumeric("RdBu",  domain = c(-lim, lim), na.color = "#cccccc",
                               reverse = TRUE)
      } else if (layer == "pct_nefin") {
        leaflet::colorNumeric("Blues", domain = unname(vals), na.color = "#cccccc")
      } else if (layer == "n_plots_total") {
        leaflet::colorNumeric("Oranges", domain = unname(vals), na.color = "#cccccc")
      } else {
        leaflet::colorNumeric("viridis", domain = unname(vals), na.color = "#cccccc")
      }
    })

    # Base hex map
    output$hex_map <- renderLeaflet({
      leaflet() |>
        addProviderTiles("CartoDB.Positron") |>
        setView(lng = -72.0, lat = 44.2, zoom = 6)
    })

    # Update polygons via leafletProxy on any relevant change
    observe({
      req(filtered_hex(), hex_pal())
      data  <- filtered_hex()
      layer <- input$hex_layer
      pal   <- hex_pal()

      # Build popup HTML
      popup_html <- paste0(
        "<b>Scale:</b> ", data$scale_name, " | <b>Hex ID:</b> ", data$hex_id,
        "<hr style='margin:4px 0'>",
        "<b>FIA plots:</b> ", data$n_plots_fia,
        " &nbsp; <b>NEFIN plots:</b> ", data$n_plots_nefin,
        " &nbsp; <b>Total:</b> ", data$n_plots_total, "<br>",
        "<b>Biomass (FIA):</b> ", round(data$fia_biomass_mean, 1), " Mg/ha<br>",
        "<b>Biomass (Augmented):</b> ", round(data$aug_biomass_mean, 1), " Mg/ha<br>",
        "<b>Difference:</b> ", round(data$biomass_change, 1), " Mg/ha",
        dplyr::if_else(data$biomass_change > 0, " \u25b2 Aug higher", " \u25bc FIA higher"),
        "<br>",
        "<b>NEFIN %:</b> ", round(data$pct_nefin, 1), "%<br>",
        "<b>Reliable:</b> ", data$reliable
      )

      layer_vals <- data[[layer]]

      proxy <- leafletProxy("hex_map", session)
      proxy |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          data        = data,
          fillColor   = pal(layer_vals),
          fillOpacity = 0.7,
          color       = "white",
          weight      = 0.3,
          popup       = popup_html,
          label       = ~paste0(round(layer_vals, 1)),
          labelOptions = labelOptions(
            style     = list("font-weight" = "normal", "font-size" = "11px"),
            textsize  = "11px",
            direction = "auto"
          )
        ) |>
        addLegend(
          position = "bottomright",
          pal      = pal,
          values   = layer_vals,
          title    = input$hex_layer,
          opacity  = 0.85
        )

      if (isTRUE(input$show_states) && !is.null(states_sf)) {
        proxy |>
          addPolylines(
            data    = states_sf,
            color   = "white",
            weight  = 0.8,
            opacity = 0.6
          )
      }
    }) |>
      bindEvent(filtered_hex(), input$hex_layer, input$show_states)

    # Summary strip below hex map
    output$hex_summary_strip <- renderUI({
      req(filtered_hex())
      data <- filtered_hex()

      n_shown   <- nrow(data)
      mean_diff <- round(mean(data$biomass_change, na.rm = TRUE), 1)
      pct_aug_higher <- round(
        100 * mean(data$biomass_change > 0, na.rm = TRUE), 1
      )

      card(
        card_body(
          class = "py-2",
          tags$small(
            bsicons::bs_icon("info-circle"), " ",
            strong(scales::comma(n_shown)), " hexagons shown at this scale. ",
            "Mean biomass difference (Aug  - FIA): ",
            strong(mean_diff, " Mg/ha"), ". ",
            strong(pct_aug_higher, "%"), " of hexagons show Augmented > FIA biomass."
          )
        )
      )
    })


    # -- Sub-tab C: Chittenden Detail ------------------------------------------

    # Debounced opacity inputs
    opacity_pred_d <- debounce(reactive(input$opacity_pred), 400)
    opacity_cov_d  <- debounce(reactive(input$opacity_cov), 400)

    # Re-show spinner when inputs change
    observeEvent(list(input$scenario_a, input$scenario_b,
                      input$covariate_layer, input$basemap), {
      session$sendCustomMessage("show-spinner", ns("map_spinner"))
    }, ignoreInit = TRUE)

    output$sync_maps <- renderUI({
      req(input$scenario_a, input$scenario_b, input$basemap, input$covariate_layer)

      # Build TIF paths
      tif_a_path <- file.path("data/rasters", paste0("biomass_10m_", input$scenario_a, "_chittenden.tif"))
      tif_b_path <- file.path("data/rasters", paste0("biomass_10m_", input$scenario_b, "_chittenden.tif"))
      tif_cov_path <- file.path("data/rasters/covariates", input$covariate_layer)

      if (!file.exists(tif_a_path) || !file.exists(tif_b_path)) {
        return(card(card_body(
          tags$p(class = "text-warning",
            bsicons::bs_icon("exclamation-triangle"), " ",
            "Chittenden TIF files not found. Run ",
            tags$code("prep_app_data.R"), " first."
          )
        )))
      }

      # Shared biomass color domain
      r_a_tmp <- terra::rast(tif_a_path)
      r_b_tmp <- terra::rast(tif_b_path)
      raster_domain <- unname(range(
        c(terra::values(r_a_tmp, na.rm = TRUE),
          terra::values(r_b_tmp, na.rm = TRUE)),
        na.rm = TRUE
      ))
      rm(r_a_tmp, r_b_tmp)

      pal <- leaflet::colorNumeric("viridis", domain = raster_domain, na.color = "transparent")

      scenario_labels <- c(
        "fia_only"   = "FIA Only",
        "nefin_only" = "NEFIN Only",
        "pooled"     = "Pooled (FIA+NEFIN)"
      )

      chitt <- sf::st_read("data/rasters/chittenden_boundary.geojson", quiet = TRUE)

      # Helper: biomass prediction map
      make_pred_map <- function(tif_path, scenario_key, show_legend = FALSE) {
        m <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) |>
          leaflet::addProviderTiles(input$basemap) |>
          leafem::addGeotiff(
            file         = normalizePath(tif_path, winslash = "/"),
            project      = FALSE,
            opacity      = opacity_pred_d(),
            autozoom     = TRUE,
            colorOptions = leafem::colorOptions(
              palette  = hcl.colors(100, "viridis"),
              na.color = "transparent",
              domain   = raster_domain
            )
          ) |>
          leaflet::addPolylines(data = chitt, color = "black", weight = 1.5, opacity = 0.7) |>
          leaflet::addControl(
            html = paste0(
              "<div style='background:rgba(0,0,0,0.7);color:white;padding:4px 8px;",
              "border-radius:4px;font-size:12px;font-weight:bold;'>",
              scenario_labels[scenario_key], "</div>"
            ),
            position = "topright"
          )
        if (show_legend) {
          m <- m |> leaflet::addLegend(
            position = "bottomright", pal = pal,
            values = seq(raster_domain[1], raster_domain[2], length.out = 100),
            title = "Biomass<br>(Mg/ha)", opacity = 0.9
          )
        }
        m
      }

      # Helper: satellite reference map (bottom-left)
      make_satellite_map <- function() {
        bb <- as.numeric(sf::st_bbox(chitt))  # xmin, ymin, xmax, ymax
        leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) |>
          leaflet::addProviderTiles("Esri.WorldImagery") |>
          leaflet::addPolylines(data = chitt, color = "white", weight = 1.5, opacity = 0.8) |>
          leaflet::addControl(
            html = "<div style='background:rgba(0,0,0,0.7);color:white;padding:4px 8px;border-radius:4px;font-size:12px;font-weight:bold;'>Satellite Reference</div>",
            position = "topright"
          ) |>
          leaflet::fitBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
      }

      # Helper: covariate map (bottom-right)
      make_covariate_map <- function() {
        cov_label <- names(which(c(
          "Canopy Height"       = "fine_canopy_height_chittenden_display.tif",
          "Elevation"           = "fine_elevation_chittenden_display.tif",
          "Slope"               = "fine_slope_chittenden_display.tif",
          "Aspect"              = "fine_aspect_chittenden_display.tif",
          "NDVI (Sentinel-2)"   = "fine_ndvi_chittenden_display.tif",
          "NDWI (Sentinel-2)"   = "fine_ndwi_chittenden_display.tif",
          "EVI (Sentinel-2)"    = "fine_evi_chittenden_display.tif",
          "NBR (Sentinel-2)"    = "fine_nbr_chittenden_display.tif",
          "Mean Temperature"    = "fine_tmean_chittenden_display.tif",
          "Min Temperature"     = "fine_tmin_chittenden_display.tif",
          "Max Temperature"     = "fine_tmax_chittenden_display.tif",
          "Precipitation"       = "fine_ppt_chittenden_display.tif"
        ) == input$covariate_layer))

        m <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) |>
          leaflet::addProviderTiles(input$basemap)

        if (file.exists(tif_cov_path)) {
          # Get covariate color domain
          r_cov <- terra::rast(tif_cov_path)
          cov_domain <- unname(range(terra::values(r_cov, na.rm = TRUE), na.rm = TRUE))
          rm(r_cov)

          cov_pal <- leaflet::colorNumeric(viridis::magma(256), domain = cov_domain, na.color = "transparent")

          m <- m |>
            leafem::addGeotiff(
              file = normalizePath(tif_cov_path, winslash = "/"),
              project = FALSE,
              opacity = opacity_cov_d(),
              autozoom = TRUE,
              colorOptions = leafem::colorOptions(
                palette = viridis::magma(100),
                na.color = "transparent",
                domain = cov_domain
              )
            ) |>
            leaflet::addLegend(
              position = "bottomright", pal = cov_pal,
              values = seq(cov_domain[1], cov_domain[2], length.out = 50),
              title = cov_label, opacity = 0.9
            )
        }

        m |>
          leaflet::addPolylines(data = chitt, color = "black", weight = 1.5, opacity = 0.7) |>
          leaflet::addControl(
            html = paste0(
              "<div style='background:rgba(0,0,0,0.7);color:white;padding:4px 8px;",
              "border-radius:4px;font-size:12px;font-weight:bold;'>",
              cov_label, "</div>"
            ),
            position = "topright"
          )
      }

      # Build 4 maps
      map_tl <- make_pred_map(tif_a_path, input$scenario_a, show_legend = FALSE)
      map_tr <- make_pred_map(tif_b_path, input$scenario_b, show_legend = TRUE)
      map_bl <- make_satellite_map()
      map_br <- make_covariate_map()

      tagList(
        tags$div(
          style = "width:100%; height:700px;",
          leafsync::sync(map_tl, map_tr, map_bl, map_br,
                         ncol = 2, sync = "all")
        ),
        tags$script(HTML(sprintf(
          "$('#%s').fadeOut(400);", ns("map_spinner")
        )))
      )
    })

    # Stats strip below synced maps
    output$chittenden_stats <- renderUI({
      req(input$scenario_a, input$scenario_b)

      # Map internal scenario name to cv_results$scenario values
      sc_map <- c(
        "fia_only"   = "FIA Only",
        "nefin_only" = "NEFIN Only",
        "pooled"     = "Pooled"
      )
      sc_a <- sc_map[input$scenario_a]
      sc_b <- sc_map[input$scenario_b]

      get_rmse <- function(sc) {
        row <- dplyr::filter(cv_results,
                             grepl("Fine|10m", scale, ignore.case = TRUE),
                             grepl(sc, scenario, ignore.case = TRUE))
        if (nrow(row) == 0) return(NA_real_)
        round(min(row$test_rmse, na.rm = TRUE), 1)
      }

      rmse_a <- get_rmse(sc_a)
      rmse_b <- get_rmse(sc_b)

      tags$div(
        style = "font-size: 0.85em; color: #94a3b8;",
        strong(style = "color: #e2e8f0;", "Test RMSE (10m)"), br(),
        tags$span(
          style = paste0("color:", DATASET_COLORS["FIA"], ";"),
          bsicons::bs_icon("square-fill")
        ),
        " ", sc_a, ": ",
        strong(if (is.na(rmse_a)) "N/A" else paste0(rmse_a, " Mg/ha")),
        br(),
        tags$span(
          style = paste0("color:", DATASET_COLORS["Pooled"], ";"),
          bsicons::bs_icon("square-fill")
        ),
        " ", sc_b, ": ",
        strong(if (is.na(rmse_b)) "N/A" else paste0(rmse_b, " Mg/ha"))
      )
    })


  })
}
