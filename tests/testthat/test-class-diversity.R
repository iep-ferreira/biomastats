diversity_fixture <- function() {
  d <- data.frame(land_class = rep(c(3, 15, 33), 3), year = rep(2000:2002, each = 3),
                  area = c(1, 1, 1,  2, 1, 1,  4, 0, 0))
  attr(d, "metadata") <- list(collection = 10, area_unit = "km2")
  attr(d, "class_dictionary") <- .biomastats_dictionary(10, c(3, 15, 33))
  d
}

test_that("indices match closed-form values", {
  out <- class_diversity(diversity_fixture())
  i <- out$indices
  expect_s3_class(out, "biomastats_class_diversity")
  expect_equal(i$richness, c(3L, 3L, 1L))
  expect_equal(i$shannon[1], log(3))
  expect_equal(i$pielou[1], 1)
  expect_equal(i$hill_1[1], 3)
  expect_equal(i$inv_simpson[1], 3)
  p <- c(.5, .25, .25)
  expect_equal(i$shannon[2], -sum(p * log(p)))
  expect_equal(i$simpson[2], sum(p^2))
  expect_equal(i$simpson_diversity[2], 1 - sum(p^2))
  expect_equal(i$berger_parker[2], .5)
  expect_equal(i$dominant_class[2], 3)
  expect_equal(i$relative_entropy[2], i$shannon[2] / log(3))
  expect_equal(i$shannon[3], 0)
  expect_true(is.na(i$pielou[3]))
  expect_true(all(i$hill_1 <= i$richness + 1e-12 & i$inv_simpson <= i$hill_1 + 1e-12))
})

test_that("contributions decompose Shannon and inherit the dictionary", {
  out <- class_diversity(diversity_fixture())
  sums <- tapply(out$contributions$shannon_contribution, out$contributions$year, sum)
  expect_equal(as.numeric(sums), out$indices$shannon)
  expect_equal(out$class_dictionary$code, c(3, 15, 33))
  expect_equal(out$change$net_change[out$change$index == "richness"], -2)
  for (p in out$plots) expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  en <- class_diversity(diversity_fixture(), language = "en")
  expect_equal(en$plots$hill$labels$x, "Year")
  expect_equal(out$plots$hill$labels$x, "Ano")
})

test_that("base, threshold, class filter and missing years are explicit", {
  d <- diversity_fixture()
  bits <- class_diversity(d, base = 2, build_plot = FALSE)
  expect_equal(bits$indices$shannon[1], log2(3))
  expect_equal(bits$indices$hill_1[1], 3)
  t <- class_diversity(d, min_area = 1, build_plot = FALSE)
  expect_equal(t$indices$richness[2], 1L)
  f <- class_diversity(d, classes = c(3, 15), build_plot = FALSE)
  expect_equal(f$indices$shannon[1], log(2))
  m <- d; m$area[m$year == 2001] <- NA
  expect_true(is.na(class_diversity(m, build_plot = FALSE)$indices$shannon[2]))
  expect_error(class_diversity(d, base = 1), "base")
  expect_error(class_diversity(d, min_area = -1), "min_area")
})

test_that("native flow from get_area result and time_series is identical", {
  areas <- get_area(example_maps())
  a <- class_diversity(areas, build_plot = FALSE)
  b <- class_diversity(areas$time_series, build_plot = FALSE)
  expect_equal(a$indices, b$indices)
  expect_equal(a$class_dictionary, attr(areas, "class_dictionary"))
  expect_equal(nrow(a$indices), length(areas$Years))
  expect_true(all(a$indices$pielou >= 0 & a$indices$pielou <= 1, na.rm = TRUE))
})
