# Colecao sintetica gravada no pacote (development/make_local_fixtures.R): tres
# anos com grades diferentes (2020 UTM 10 m, 2021 WGS84, 2022 UTM 20 m), area
# focal de 200 x 200 m e legenda em CSV.
local_dir <- function() system.file("extdata", "local_maps", package = "biomastats")
local_area <- function() file.path(local_dir(), "area_sintetica.geojson")
local_maps <- function(...) {
  suppressMessages(load_local_rasters(local_area(), folder = local_dir(), ...))
}
# Copia de parte da colecao numa pasta temporaria, para os casos de erro.
local_copy <- function(years, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  file.copy(file.path(local_dir(), sprintf("sintetico_%d.tif", years)), dir)
  dir
}

test_that("load_local_rasters harmonizes, crops and masks a local collection", {
  expect_message(maps <- load_local_rasters(local_area(), folder = local_dir()), "class_dictionary")
  expect_named(maps, c("shape", "time_range", "raster", "collection", "class_dictionary",
                       "metadata", "source"))
  expect_equal(maps$time_range, c(2020, 2022))
  expect_length(maps$raster, 3)
  expect_equal(maps$collection, "sintetico")
  expect_null(maps$class_dictionary)
  h <- maps$metadata$harmonization
  expect_equal(h$action, c("none", "reprojected (nearest)", "resampled (nearest)"))
  expect_equal(h$file, sprintf("sintetico_%d.tif", 2020:2022))
  ref <- maps$raster[[1]]
  expect_s4_class(ref, "RasterLayer")
  expect_true(raster::inMemory(ref))
  for (r in maps$raster) {
    expect_true(raster::compareRaster(ref, r, extent = TRUE, rowcol = TRUE, crs = TRUE,
                                      res = TRUE, stopiffalse = FALSE))
  }
  expect_equal(raster::res(ref), c(10, 10))
  # Mascara pelo centro das celulas: 20 x 20 celulas na area; a coluna 0 vira NA.
  v <- lapply(maps$raster, raster::getValues)
  expect_equal(sum(!is.na(v[[1]])), 19 * 20)
  expect_true(all(stats::na.omit(unlist(v)) %in% c(3, 15)))
  expect_equal(sum(v[[1]] == 3, na.rm = TRUE), 32)
  expect_lt(sum(v[[2]] == 3, na.rm = TRUE), 32)  # C reduzido; a reprojecao desloca bordas
  expect_gt(sum(v[[3]] == 3, na.rm = TRUE), 0)
  expect_equal(maps$source$loader, "load_local_rasters")
  expect_equal(maps$source$prefix, "sintetico")
  expect_equal(as.numeric(sf::st_area(maps$shape)), 200^2, tolerance = 1e-2)
  keep <- local_maps(nodata = NULL, years = 2020)
  expect_equal(sum(raster::getValues(keep$raster[[1]]) == 0, na.rm = TRUE), 20)
  wgs <- local_maps(years = 2020, crs = 4326)
  expect_true(sf::st_is_longlat(sf::st_crs(wgs$raster[[1]])))
  coarse <- local_maps(years = 2020:2021, resolution = 20)
  expect_equal(raster::res(coarse$raster[[1]]), c(20, 20))
  expect_equal(coarse$metadata$harmonization$action, c("resampled (nearest)",
                                                       "reprojected (nearest)"))
  wide <- local_maps(years = 2020, context_m = 100)
  expect_equal(as.numeric(sf::st_area(wide$focal)), 200^2, tolerance = 1e-2)
  expect_gt(sum(!is.na(raster::getValues(wide$raster[[1]]))), 19 * 20)
  expect_equal(wide$source$context_m, 100)
})

test_that("local maps feed the analysis functions with their dictionary", {
  dic <- class_dictionary(file = file.path(local_dir(), "legenda_sintetica.csv"))
  maps <- local_maps(dictionary = dic)
  expect_equal(maps$class_dictionary$classe, c("Floresta", "Pastagem"))
  a <- get_area(maps)
  dict <- attr(a, "class_dictionary")
  expect_equal(dict$classe[dict$code == 3], "Floresta")
  expect_equal(dict$color[dict$code == 15], "#EDDE8E")
  expect_s3_class(land_vis(maps, 2020), "ggplot")
  expect_s3_class(land_change(maps, years = c(2020, 2021), build_plot = FALSE), "list")
  metrics <- biomastats_metrics(maps, classes = 3, metrics = "aggregation_index")
  expect_equal(metrics$class_dictionary$classe, "Floresta")
  p <- reclass_to_polygons(metrics, years = 2020, save_shp = FALSE)
  expect_equal(unique(p$classe[p$value == 1]), "Floresta")
  con <- land_connectivity(p, max_dist = 60, build_plot = FALSE)
  expect_equal(con$class_dictionary$classe, "Floresta")
  # Codigo conhecido por dict_build() dispensa dicionario.
  mb <- local_maps(collection = 10, years = 2020)
  expect_equal(attr(get_area(mb), "class_dictionary")$classe[1], "Forma\u00e7\u00e3o Florestal")
  # Sem dicionario, nomes genericos.
  plain <- local_maps(years = 2020)
  expect_equal(attr(get_area(plain), "class_dictionary")$classe, c("Classe 3", "Classe 15"))
})

test_that("connectivity_context reloads a local collection for the context", {
  maps <- local_maps(years = 2020)
  ctx <- connectivity_context(maps, classes = 3, max_dist = 60, context = c(0, 100),
                              metric_patch = "IIC", build_plot = FALSE)
  expect_equal(ctx$metadata$maps_source, "load_local_rasters")
  expect_equal(ctx$sensitivity$context_patches, c(0, 1))
  expect_gt(ctx$sensitivity$ECA_ha[2], ctx$sensitivity$ECA_ha[1])
})

test_that("load_local_rasters reports incompatible collections", {
  expect_error(load_local_rasters(local_area(), folder = "nao_existe"), "pasta")
  empty <- withr::local_tempdir()
  expect_error(load_local_rasters(local_area(), folder = empty), "Nenhum GeoTIFF")
  two <- local_copy(2020:2021)
  file.copy(file.path(two, "sintetico_2020.tif"), file.path(two, "outro_2020.tif"))
  expect_error(load_local_rasters(local_area(), folder = two), "Varios prefixos")
  expect_equal(suppressMessages(load_local_rasters(local_area(), folder = two,
                                                   prefix = "outro"))$time_range, c(2020, 2020))
  expect_error(local_maps(years = 2019:2020), "2019")
  gap <- local_copy(c(2020, 2022))
  expect_error(load_local_rasters(local_area(), folder = gap), "consecutivos")
  far <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(-40, -10), c(-39.9, -10),
    c(-39.9, -9.9), c(-40, -9.9), c(-40, -10)))), crs = 4326))
  expect_error(load_local_rasters(far, folder = local_dir()), "nao cobre")
  bad <- withr::local_tempdir()
  r <- terra::rast(file.path(local_dir(), "sintetico_2020.tif"))
  terra::writeRaster(c(r, r), file.path(bad, "duas_2020.tif"))
  expect_error(suppressMessages(load_local_rasters(local_area(), folder = bad)), "unica camada")
  float <- withr::local_tempdir()
  terra::writeRaster(r + 0.5, file.path(float, "real_2020.tif"), datatype = "FLT4S")
  expect_error(suppressMessages(load_local_rasters(local_area(), folder = float)), "inteiros")
  expect_error(local_maps(context_m = -1), "context_m")
  expect_error(local_maps(resolution = -5), "resolution")
})

test_that("class_dictionary builds templates and checks dictionaries", {
  maps <- local_maps(years = 2020)
  tpl <- class_dictionary(maps)
  expect_named(tpl, c("grupo", "group", "classe", "class", "code", "color"))
  expect_equal(tpl$code, c(3, 15))
  expect_equal(tpl$classe, c("Classe 3", "Classe 15"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}", tpl$color)))
  base <- class_dictionary(maps, collection = 10)
  expect_equal(base$class, c("Forest Formation", "Pasture"))
  expect_equal(nrow(class_dictionary(collection = "esa_worldcover")), nrow(dict_build("esa_worldcover")))
  csv <- class_dictionary(maps, file = file.path(local_dir(), "legenda_sintetica.csv"))
  expect_equal(csv$class, c("Forest", "Pasture"))
  expect_equal(csv$color, c("#1F8D49", "#EDDE8E"))
  one <- class_dictionary(file = data.frame(Valor = c(3, 15), Nome = c("Mata", "Pasto")))
  expect_equal(one$classe, one$class)
  expect_equal(one$classe, c("Mata", "Pasto"))
  expect_true(all(nzchar(one$color)))
  expect_warning(miss <- class_dictionary(maps, file = data.frame(code = 3, name = "Mata")),
                 "15")
  expect_equal(miss$classe, c("Mata", "Classe 15"))
  expect_error(class_dictionary(file = data.frame(code = c(3, 3), name = "a")), "repetidos")
  expect_error(class_dictionary(file = data.frame(nome = "a")), "codigo")
  expect_error(class_dictionary(file = data.frame(code = 3, cor = "naoecor")), "Cores")
  expect_error(class_dictionary(file = "nao_existe.csv"), "CSV")
  expect_error(class_dictionary(), "Informe")
})

test_that("the study area is detected as .gpkg or .shp and a shapefile needs its .prj", {
  area <- sf::st_read(local_area(), quiet = TRUE)
  dir <- withr::local_tempdir()
  sf::st_write(area, file.path(dir, "area.shp"), quiet = TRUE)
  ref <- .read_study_area(local_area())
  expect_equal(sf::st_area(.read_study_area(file.path(dir, "area.shp"))), sf::st_area(ref),
               tolerance = 1e-6)
  # Sem extensao e pela pasta: o unico arquivo de area e encontrado.
  expect_equal(.study_area_file(file.path(dir, "area")), file.path(dir, "area.shp"))
  expect_equal(.study_area_file(dir), file.path(dir, "area.shp"))
  # Com .gpkg e .shp de mesmo nome, vale o GeoPackage.
  sf::st_write(area, file.path(dir, "area.gpkg"), quiet = TRUE)
  expect_equal(.study_area_file(file.path(dir, "area")), file.path(dir, "area.gpkg"))
  expect_equal(.study_area_file(dir), file.path(dir, "area.gpkg"))
  sf::st_write(area, file.path(dir, "outra.gpkg"), quiet = TRUE)
  expect_error(.study_area_file(dir), "Varios arquivos de area")
  # Sem .prj, erro explicito.
  lost <- withr::local_tempdir()
  sf::st_write(area, file.path(lost, "sem_crs.shp"), quiet = TRUE)
  unlink(file.path(lost, "sem_crs.prj"))
  expect_error(.read_study_area(file.path(lost, "sem_crs.shp")), "[.]prj")
  expect_error(load_local_rasters(file.path(lost, "sem_crs.shp"), folder = local_dir()), "[.]prj")
  expect_error(.study_area_file(file.path(dir, "nada")), "nao encontrado")
  # A pasta dos mapas pode guardar a propria area.
  maps_dir <- local_copy(2020)
  file.copy(local_area(), maps_dir)
  expect_equal(suppressMessages(load_local_rasters(folder = maps_dir))$time_range, c(2020, 2020))
})

test_that("load_local_rasters refuses maps that cover only part of the area", {
  part <- withr::local_tempdir()
  r <- terra::rast(file.path(local_dir(), "sintetico_2020.tif"))
  terra::writeRaster(terra::crop(r, terra::ext(500000, 500100, 7400000, 7400300)),
                     file.path(part, "metade_2020.tif"))
  expect_error(suppressMessages(load_local_rasters(local_area(), folder = part)),
               "cobre apenas 50")
})

test_that("raster_catalog lists the MapBiomas collections for selective reading", {
  cat <- raster_catalog()
  expect_true(all(c("mapbiomas_brasil_c10", "mapbiomas_brasil_c11", "mapbiomas_brasil_10m_c4",
                    "mapbiomas_chile_c1", "mapbiomas_argentina_c3") %in% cat$id))
  expect_false(anyDuplicated(cat$id) > 0)
  expect_true(all(grepl("^https://storage.googleapis.com/mapbiomas-public/", cat$url)))
  expect_true(all(grepl("{year}", cat$url, fixed = TRUE)))
  expect_true(all(cat$first_year <= cat$last_year))
  expect_equal(unique(cat$dictionary[cat$initiative == "brasil"]), 10)
  expect_true(all(is.na(cat$dictionary[cat$initiative != "brasil"])))
  expect_equal(raster_catalog("mapbiomas_brasil_10m_c4")$resolution_m, 10)
  expect_equal(nrow(raster_catalog(initiative = "brasil")), 4)
  expect_error(raster_catalog("nao_existe"), "catalogo")
})

test_that("load_rasters(method = 'http') reads windows, exports them and they are portable", {
  # Modelo de caminho local no lugar da URL: mesma rotina, sem rede.
  template <- file.path(local_dir(), "sintetico_{year}.tif")
  out <- withr::local_tempdir()
  http <- suppressMessages(load_rasters(local_area(), start = 2020, end = 2022, method = "http",
                                        url = template, export_folder_path = out))
  local <- local_maps()
  for (i in 1:3) expect_equal(raster::getValues(http$raster[[i]]), raster::getValues(local$raster[[i]]))
  expect_equal(http$metadata$harmonization$action, local$metadata$harmonization$action)
  expect_equal(http$source$method, "http")
  expect_equal(http$collection, "sintetico")
  expect_setequal(basename(http$metadata$exported),
                  c(sprintf("sintetico_%d.tif", 2020:2022), "area.gpkg"))
  again <- suppressMessages(load_local_rasters(folder = out))
  for (i in 1:3) expect_equal(raster::getValues(again$raster[[i]]), raster::getValues(http$raster[[i]]))
  expect_true(raster::compareRaster(again$raster[[1]], http$raster[[1]], stopiffalse = FALSE))
  # Dicionario, contexto e recarga do contexto pela mesma URL.
  dic <- file.path(local_dir(), "legenda_sintetica.csv")
  wide <- load_rasters(local_area(), start = 2020, end = 2020, method = "http", url = template,
                       dictionary = dic, context_m = 100)
  expect_equal(wide$class_dictionary$classe, c("Floresta", "Pastagem"))
  expect_false(is.null(wide$focal))
  maps <- load_rasters(local_area(), start = 2020, end = 2020, method = "http", url = template,
                       dictionary = dic)
  ctx <- connectivity_context(maps, classes = 3, max_dist = 60, context = c(0, 100),
                              metric_patch = "IIC", build_plot = FALSE)
  expect_equal(ctx$metadata$maps_source, "load_rasters (http)")
  expect_equal(ctx$sensitivity$context_patches, c(0, 1))
  expect_error(load_rasters(local_area(), start = 2020, end = 2020, method = "http"), "product ou url")
  expect_error(load_rasters(local_area(), start = 2020, end = 2020, method = "http",
                            url = "sem_ano.tif"), "[{]year[}]")
  expect_error(load_rasters(local_area(), start = 1980, end = 1981, method = "http",
                            product = "mapbiomas_brasil_c10"), "1985-2024")
  expect_error(suppressMessages(load_rasters(local_area(), start = 2019, end = 2019,
    method = "http", url = template)), "Nao foi possivel ler")
})
