# Column names used inside ggplot2/dplyr/arrow expressions (non-standard
# evaluation); declared once so R CMD check does not report them as undefined.
utils::globalVariables(c(
  "bbox", "subtype", "x", "y", "value",
  "year", "group", "proportion", "shannon_contribution", "shannon_max", "berger_parker",
  "shape_circle",
  # biomastats_metrics, get_area, land_dist
  "Year", "Avg_frag_area", "Edge_length", "Edge_density", "Class_area", "Aggregation_Index",
  "Number_of_fragments", "area", "land_class",
  # time-series plots
  "area_x", "area_y", "residual", "theoretical", "fitted", "observed", "lower", "upper",
  "transition", "prediction", "label", "conf_low", "conf_high", "estimate", "lag",
  "bound", "exceeds",
  # feature density plot
  "density"))
