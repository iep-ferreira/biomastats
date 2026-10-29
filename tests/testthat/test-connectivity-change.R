change_square <- function(x0, y0, side) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x0 + side, y0), c(x0 + side, y0 + side),
    c(x0, y0 + side), c(x0, y0))))
}
# Tres anos com perda de habitat no mesmo limite: 2000 (4 fragmentos), 2010 (3), 2020 (2).
change_years <- function() {
  year <- function(y, geoms) sf::st_sf(value = 1, fragment_id = seq_along(geoms), year = y,
    geometry = sf::st_sfc(geoms, crs = 32722))
  x <- list(
    `2000` = year(2000, list(change_square(500000, 7400000, 20), change_square(500050, 7400000, 20),
      change_square(500100, 7400040, 20), change_square(500300, 7400200, 35))),
    `2010` = year(2010, list(change_square(500000, 7400000, 20), change_square(500100, 7400040, 20),
      change_square(500300, 7400200, 35))),
    `2020` = year(2020, list(change_square(500000, 7400000, 15), change_square(500300, 7400200, 35))))
  boundary <- sf::st_sfc(change_square(499900, 7399900, 600), crs = 32722)
  for (i in seq_along(x)) attr(x[[i]], "boundary") <- boundary
  x
}

test_that("connectivity_change reports ECA, dA, dECA, rECA and change classes", {
  x <- change_years()
  ch <- connectivity_change(x, max_dist = c(100, 300), metric_patch = c("IIC", "PC"),
                            link_importance = TRUE, build_plot = FALSE)
  expect_s3_class(ch, "biomastats_connectivity_change")
  s <- ch$series
  expect_equal(nrow(s), 3 * 2 * 2)
  expect_setequal(unique(s$metric), c("IIC", "PC"))
  pc <- s[s$metric == "PC" & s$max_dist_m == 100, ]
  expect_equal(pc$year, c(2000, 2010, 2020))
  expect_equal(pc$habitat_ha, c(400 * 3 + 35^2, 400 * 2 + 35^2, 225 + 35^2) / 10000)
  con <- ch$connectivity
  eca <- vapply(con, function(z) z$metrics$ECA_PC[1], 1) / 10000
  expect_equal(pc$ECA_ha, unname(eca))
  expect_equal(pc$ECA_LA, 100 * pc$ECA_ha * 10000 / 600^2)
  expect_true(is.na(pc$dA[1]) && is.na(pc$dECA[1]))
  expect_equal(pc$dA[2:3], 100 * diff(pc$habitat_ha) / pc$habitat_ha[1:2])
  expect_equal(pc$dECA[2:3], 100 * diff(pc$ECA_ha) / pc$ECA_ha[1:2])
  expect_equal(pc$rECA, pc$dECA / pc$dA)
  expect_equal(pc$change_makurhini, c(NA, "+ Habitat loss", "+ Habitat loss"))
  expect_equal(pc$change[1], "Linha de base")
  expect_equal(pc$intra + pc$direct + pc$step, rep(100, 3))
  first <- connectivity_change(con, reference = "first", build_plot = FALSE)$series
  f <- first[first$metric == "PC" & first$max_dist_m == 100, ]
  expect_equal(f$dECA[3], 100 * (f$ECA_ha[3] - f$ECA_ha[1]) / f$ECA_ha[1])
  # Mesmo resultado a partir dos objetos de land_connectivity ja calculados.
  again <- connectivity_change(rev(con), build_plot = FALSE)
  expect_equal(again$series, ch$series)
  expect_equal(nrow(ch$metric_series), 6)
  expect_equal(sort(unique(ch$patch_series$year)), c(2000, 2010, 2020))
  expect_s3_class(ch$patch_series, "sf")
  ls <- ch$link_summary
  expect_true(all(ls$importance_max <= ls$importance_sum + 1e-12))
  expect_equal(sum(ch$link_series$year == 2000 & ch$link_series$metric == "IIC" &
                   ch$link_series$max_dist_m == 100), ls$links[ls$year == 2000 &
                   ls$metric == "IIC" & ls$max_dist_m == 100])
  expect_equal(ch$metadata$years, c(2000, 2010, 2020))
  sub <- connectivity_change(x, years = c(2000, 2020), max_dist = 100, build_plot = FALSE)
  expect_equal(unique(sub$series$year), c(2000, 2020))
  expect_equal(unique(sub$series$metric), "IIC")
})

test_that("connectivity_change classifies changes as Makurhini", {
  cls <- .change_class(c(NA, -10, -10, -10, 0, 5, -10), c(NA, -20, -5, -10, 0, -3, 2))
  expect_equal(cls$makurhini, c(NA, "+ Connectivity loss", "+ Habitat loss", "Equal loss",
    "Habitat or connectivity maintained", "Habitat or connectivity gain",
    "Habitat or connectivity gain"))
  expect_equal(cls$en[2], "Connectivity loss > habitat loss")
})

test_that("connectivity_change builds plots, an animation and the dECA conversion", {
  ch <- connectivity_change(change_years(), max_dist = 100, metric_patch = c("IIC", "PC"),
                            link_importance = TRUE, language = "en")
  for (name in c("eca_plot", "change_plot", "fractions_plot", "map_plot", "link_plot")) {
    expect_s3_class(ch[[name]], "ggplot")
  }
  expect_null(ch$animation)
  expect_equal(ch$metadata$map, list(metric = "PC", max_dist_m = 100))
  expect_equal(ch$series$change[1], "Baseline")
  mk <- as_makurhini(ch, "dECA")
  expect_equal(mk$fun, "MK_dECA")
  expect_length(mk$args[[1]]$nodes, 3)
  expect_equal(mk$metadata$years, c("2000", "2010", "2020"))
  expect_equal(mk$args[[1]]$LA, 600^2)
  roundtrip <- unserialize(serialize(ch, NULL))
  expect_equal(roundtrip$series, ch$series)
})

test_that("connectivity_change returns a gganimate animation when asked", {
  skip_if_not_installed("gganimate")
  ch <- connectivity_change(change_years(), max_dist = 100, metric_patch = "PC", animate = TRUE,
                            build_plot = FALSE)
  expect_s3_class(ch$animation, "gganim")
  expect_null(ch$map_plot)
})

test_that("connectivity_change validates its input", {
  x <- change_years()
  expect_error(connectivity_change(x["2000"], max_dist = 100), "dois anos")
  expect_error(connectivity_change(x, years = 1999), "years")
  con <- lapply(x[1:2], land_connectivity, max_dist = 100, build_plot = FALSE)
  expect_error(connectivity_change(unname(con)), "nomeada")
  expect_error(connectivity_change(con, max_dist = 100), "extras")
  wide <- x
  attr(wide[["2010"]], "boundary") <- sf::st_sfc(change_square(499800, 7399800, 800), crs = 32722)
  expect_warning(connectivity_change(wide, max_dist = 100, build_plot = FALSE), "A_L")
})
