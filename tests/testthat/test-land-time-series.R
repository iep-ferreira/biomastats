series_fixture <- function(y, codes = 3, years = 1985:(1984 + length(y))) {
  d <- data.frame(land_class = codes, year = years, area = y)
  attr(d, "metadata") <- list(collection = 10, area_unit = "km2", years = years)
  attr(d, "class_dictionary") <- .biomastats_dictionary(10, unique(codes))
  d
}

test_that("preparation preserves the native dictionary and data contract", {
  x <- 0:19; a <- series_fixture(100 + x, 3); b <- series_fixture(200 - x, 15)
  d <- rbind(a, b); attr(d, "metadata") <- attr(a, "metadata"); attr(d, "class_dictionary") <- .biomastats_dictionary(10, c(3, 15))
  out <- land_time_series(d)
  expect_s3_class(out, "biomastats_time_series")
  expect_equal(out$class_dictionary$color, attr(d, "class_dictionary")$color)
  expect_equal(out$summary$status, c("ready", "ready"))
  expect_s3_class(ggplot2::ggplot_build(out$plot), "ggplot_built")
})

test_that("correlation compares two chosen series with association and significance", {
  x <- 0:29; a <- series_fixture(100 + x + sin(x), 3); b <- series_fixture(200 - 2 * x - sin(x), 15)
  d <- rbind(a, b); attr(d, "metadata") <- attr(a, "metadata"); attr(d, "class_dictionary") <- .biomastats_dictionary(10, c(3, 15))
  z <- series_correlation(land_time_series(d), 3, 15)
  expect_s3_class(z, "biomastats_series_correlation")
  expect_lt(z$association$estimate, -.99)
  expect_equal(names(z$data), c("year", "area_x", "area_y"))
  expect_equal(names(z$plots), c("series", "association"))
  for (p in z$plots) expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  en <- series_correlation(d, 3, 15, language = "en")
  expect_equal(en$plots$series$labels$x, "Year")
  expect_equal(z$plots$series$labels$x, "Ano")
})

test_that("continuous segmented and probabilistic bent-cable models recover a change", {
  x <- 0:39; yr <- 1985 + x; tau <- 20; scale <- 3
  h <- ifelse(x < tau - scale, 0, ifelse(x > tau + scale, x - tau, (x - tau + scale)^2 / (4 * scale)))
  obj <- land_time_series(series_fixture(100 + x + 3 * h, 3, yr), build_plot = FALSE)
  segmented <- series_fit_regime(obj, 3, model = "segmented", breaks = 1, bootstrap = 30, seed = 2)
  probabilistic <- series_fit_regime(obj, 3, model = "prob_bent_logistic", breaks = 1, bootstrap = 30, seed = 2)
  expect_equal(segmented$model, "segmented")
  expect_equal(probabilistic$model, "prob_bent_logistic")
  expect_lt(abs(probabilistic$change_points$estimate - (1985 + tau)), 2)
  expect_true(all(c("conf_low", "conf_high", "band_low", "band_high") %in% names(probabilistic$change_points)))
  expect_lte(probabilistic$change_points$conf_low, probabilistic$change_points$conf_high)
  expect_s3_class(ggplot2::ggplot_build(probabilistic$plot), "ggplot_built")
})

test_that("multiple transitions are selected by information criterion with kernels and bands", {
  x <- 0:49
  h <- function(tau, s) s * log1p(exp((x - tau) / s))
  y <- 100 + x - 3 * h(15, 1.5) + 4 * h(32, 2) + .05 * sin(3 * x)
  f <- series_fit_regime(series_fixture(y), 3, "prob_bent_logistic", max_breaks = 3, bootstrap = 25,
                         bootstrap_type = "block", seed = 1)
  expect_equal(f$breaks, 2L)
  expect_equal(f$model_table$breaks[f$model_table$selected], 2L)
  expect_lt(max(abs(f$change_points$estimate - (1985 + c(15, 32)))), 1)
  expect_equal(sort(unique(f$kernels$transition)), 1:2)
  expect_equal(nrow(f$transition_bands), 2L)
  expect_true(all(f$transition_bands$lower < f$change_points$estimate & f$change_points$estimate < f$transition_bands$upper))
  expect_equal(f$change_points$slope_after[1], f$change_points$slope_before[2])
  expect_equal(f$bootstrap_type, "block")
  expect_s3_class(ggplot2::ggplot_build(f$interval_plot), "ggplot_built")
  fixed <- series_fit_regime(series_fixture(y), 3, "segmented", breaks = 2, bootstrap = 0)
  expect_equal(nrow(fixed$model_table), 1L)
  expect_equal(nrow(fixed$kernels), 0L)
  auto <- series_fit_regime(series_fixture(y), 3, "auto", max_breaks = 2, bootstrap = 0, build_plot = FALSE)
  expect_true(all(c("segmented", "prob_bent_logistic") %in% auto$model_table$model))
  expect_equal(sum(auto$model_table$selected), 1L)
})

test_that("diagnostics and forecasts use one chosen fit without reselection", {
  x <- 0:24; obj <- land_time_series(series_fixture(50 + 1.5 * x + sin(x), 3), build_plot = FALSE)
  fit <- series_fit_regime(obj, 3, model = "linear", bootstrap = 0); diag <- series_diagnostics(fit); pred <- series_forecast(fit, horizon = 4)
  expect_equal(pred$model, "linear")
  expect_equal(pred$projection$year, 2010:2013)
  expect_true(all(pred$projection$upper > pred$projection$lower))
  expect_true(all(c("ljung_box_p_value", "rmse", "durbin_watson", "residual_acf1") %in% names(diag$quality)))
  for (p in diag$plots) expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("dense curves and recovered threshold kernels agree with their models", {
  x <- 0:39
  y <- 100 + x + 3 * 2 * log1p(exp((x - 20) / 2)) + .1 * sin(x)
  f <- series_fit_regime(series_fixture(y), 3, "prob_bent_logistic", breaks = 1, bootstrap = 0)
  tau <- f$fit$origin + f$fit$taus; s <- f$fit$scales
  expect_gt(nrow(f$curve), length(x))
  expect_true(all((x + 1985) %in% f$curve$year))
  expect_equal(f$curve$fitted, as.numeric(.regime_design(f$curve$year, f$fit) %*% f$fit$coef))
  expect_equal(f$fitted$fitted, f$curve$fitted[match(f$fitted$year, f$curve$year)])
  expect_equal(f$kernels$density, stats::dlogis(f$kernels$year, tau, s))
  expect_equal(stats::plogis(c(f$transition_bands$lower, f$transition_bands$upper), tau, s), c(.1, .9))
  u <- series_fit_regime(series_fixture(y), 3, "prob_bent_uniform", breaks = 1, bootstrap = 0)
  expect_equal(c(u$transition_bands$lower, u$transition_bands$upper),
               u$fit$origin + u$fit$taus + c(-.8, .8) * u$fit$scales)
  expect_s3_class(ggplot2::ggplot_build(f$plot), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(series_forecast(f)$plot), "ggplot_built")
  expect_match(f$plot$labels$y, "\u00c1rea")
})

test_that("serial adjustment, transformations and input checks", {
  set.seed(10); x <- 0:29
  a <- series_fixture(100 + x + rnorm(30)); b <- series_fixture(100 + 2 * x + rnorm(30), 15)
  d <- rbind(a, b); attr(d, "class_dictionary") <- .biomastats_dictionary(10, c(3, 15))
  z <- series_correlation(d, 3, 15, transformation = "detrended")
  expect_equal(z$paired$area_x, as.numeric(residuals(lm(a$area ~ a$year))))
  expect_lte(z$association$n_effective, z$association$n)
  expect_equal(nrow(series_correlation(d, 3, 15, transformation = "difference")$paired), 29)
  k <- series_correlation(d, 3, 15, method = "kendall", build_plot = FALSE)
  expect_true(is.na(k$association$p_serial))
  expect_equal(k$association$estimate, unname(cor(a$area, b$area, method = "kendall")))
  expect_error(series_correlation(d[-5, ], 3, 15), "consecutivos")
  expect_error(series_correlation(d, 3, 3), "distintas")
})
