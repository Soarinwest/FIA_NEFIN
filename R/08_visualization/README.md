# R/08_visualization

Figure generation scripts for spatial context maps, hexagon GeoJSONs,
and manuscript figures. Run these after analysis phases are complete.

## Scripts

`CREATE_SPATIAL_CONTEXT_MAPS.R`
 Generates study area maps: plot locations, state boundaries,
 hexagon grid illustration, coordinate fuzzing concept figure.
 Output in `data/processed/figures/spatial_context/`.

`CREATE_HEXAGON_GEOJSONS.R`
 Writes hexagon GeoJSON files with aggregated statistics.
 Output in `data/processed/hex_geojson_with_stats/`.

`CREATE_HEXAGON_GEOJSONS_WITH_DECISIONS.R`
 Adds scale recommendation decisions to hex GeoJSON properties.

`GENERATE_ALL_FIGURES.R`
 Runs all visualization scripts in order and copies final figures
 to `manuscript_figures/`.

`fig_chittenden_aerial_panel.R`
 Generates aerial-view biomass panel for Chittenden County, VT.

`fig_paper2_diagnostics.R`
 Diagnostic figures for Paper 2: residuals, variable importance,
 observed vs predicted.

`visualize_scenario_comparison_with_aerial.R`
 Scenario comparison maps with aerial imagery background.
