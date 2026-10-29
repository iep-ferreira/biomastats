# Gera os dados sinteticos leves de inst/extdata/local_maps, usados nos testes de
# load_local_rasters(), class_dictionary() e connectivity_context().
# Uso, na raiz do pacote: Rscript development/make_local_fixtures.R
#
# Cena em UTM 22S (celulas de 10 m, 600 x 300 m): floresta (3) sobre pastagem
# (15), com uma coluna sem dado (0) a oeste. A e C ficam na area focal
# [0, 200] x [0, 200] m, a 80 m uma da outra; B, fora dela, liga as duas; D fica
# longe. Os tres anos tem grades diferentes, para exercitar a harmonizacao:
#   2020 - UTM 22S, 10 m (grade de referencia);
#   2021 - WGS84 (reprojecao), C reduzido a metade;
#   2022 - UTM 22S, 20 m (reamostragem).
out <- file.path("inst", "extdata", "local_maps")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
x0 <- 500000
y0 <- 7400000
scene <- function(c_top = 180) {
  r <- terra::rast(xmin = x0, xmax = x0 + 600, ymin = y0, ymax = y0 + 300, resolution = 10,
                   crs = "EPSG:32722")
  xy <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  x <- xy[, 1] - x0
  y <- xy[, 2] - y0
  inside <- function(a, b, c, d) x > a & x < b & y > c & y < d
  v <- rep(15, terra::ncell(r))
  v[inside(150, 190, 20, 60)] <- 3        # A
  v[inside(150, 190, 140, c_top)] <- 3    # C
  v[inside(220, 260, 80, 120)] <- 3       # B (contexto)
  v[inside(500, 540, 120, 160)] <- 3      # D (contexto distante)
  v[x < 10] <- 0                          # sem dado
  terra::values(r) <- v
  r
}
write <- function(r, year) {
  terra::writeRaster(r, file.path(out, sprintf("sintetico_%d.tif", year)), overwrite = TRUE,
                     datatype = "INT1U", gdal = "COMPRESS=DEFLATE")
}
write(scene(), 2020)
write(terra::project(scene(c_top = 160), "EPSG:4326", method = "near"), 2021)
write(terra::aggregate(scene(), fact = 2, fun = "modal"), 2022)
area <- sf::st_sf(nome = "area focal", geometry = sf::st_sfc(sf::st_polygon(list(rbind(
  c(x0, y0), c(x0 + 200, y0), c(x0 + 200, y0 + 200), c(x0, y0 + 200), c(x0, y0)))),
  crs = 32722))
unlink(file.path(out, c("area_sintetica.gpkg", "area_sintetica.geojson")))
sf::st_write(area, file.path(out, "area_sintetica.geojson"), quiet = TRUE)
writeLines(c("codigo;classe;class;cor", "3;Floresta;Forest;#1f8d49", "15;Pastagem;Pasture;#edde8e"),
           file.path(out, "legenda_sintetica.csv"), useBytes = TRUE)
print(file.info(list.files(out, full.names = TRUE))["size"])
