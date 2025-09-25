#' Construct the dictionary
#'
#' This function builds a dataframe that serves as a dictionary for a classification scheme.
#' The dictionary includes six fields: "group" in Portuguese and English, "class" in Portuguese
#' and English, a numerical "code", and a "color" associated with each class.
#'
#' @param The collection version 
#' @return A dataframe that serves as a dictionary for a classification scheme.
#' @export

dict_build <- function(collection = 10){

if(collection == 10){message("Collection 10 subtitles are loaded as default. If you want a different collection, plase change the 'collection' option") }

if(!collection %in% c(7, 10)){ stop("This collection isn't available") }
# Sub-titles for collection 7  
  dict_7 <- data.frame(rbind(
    c("Nao Catalogada", "Non Cataloged", "Nao Catalogada", "Not cataloged", 0, "#FFFFFF"),
    c("Floresta", "Forest", "Floresta", "Forest", 1, "#129912"),
    c("Floresta", "Forest","Formacao Florestal", "Forest Formation", 3, "#006400"),
    c("Floresta", "Forest", "Formacao Savanica", "Savanna Formation", 4, "#00ff00"),
    c("Floresta", "Forest", "Mangue", "Mangrove", 5, "#687537"),
    c("Floresta", "Forest", "Silvicultura", "Forest Plantation", 9, "#00CC66"),
    c("Formacao Natural Nao Florestal", "Non Forest Natural Formation", "Formacao Natural Nao Florestal", "Non Forest Natural Formation", 10, "#bbfcac"),
    c("Formacao Natural Nao Florestal", "Non Forest Natural Formation", "Campo Alagado", "Wetland", 11, "#45c2a5"),
    c("Formacao Natural Nao Florestal", "Non Forest Natural Formation", "Formacao Campestre", "Grassland", 12, "#b8af4f"),
    c("Formacao Natural Nao Florestal", "Non Forest Natural Formation", "Outras Formacoes Nao Florestais", "Other Non Forest Formations", 13, "#bdb76b"),
    c("Agropecuaria", "Farming", "Agropecuaria", "Farming", 14, "#ffffb2"),
    c("Agropecuaria", "Farming", "Pastagem", "Pasture", 15, "#ffd966"),
    c("Agropecuaria", "Farming", "Agricultura", "Agriculture", 18, "#e974ed"),
    c("Agropecuaria", "Farming", "Lavoura temporaria", "Temporary Crop", 19,"#d5a6bd"),
    c("Agropecuaria", "Farming", "Cana", "Sugar Cane", 20, "#c27ba0"),
    c("Agropecuaria", "Farming", "Mosaico de Agricultura e Pastagem", "Mosaic of Uses", 21, "#fff3bf"),
    c("Area Nao Vegetada", "Non Vegetated Area", "Area Nao Vegetada", "Non Vegetated Area", 22, "#ea9999"),
    c("Area Nao Vegetada", "Non Vegetated Area", "Praia, Duna e Areial", "Beach, Dune and Sand Spot", 23, "#dd7e6b"),
    c("Area Nao Vegetada", "Non Vegetated Area", "Area Urbanizada", "Urban Area", 24, "#af2a2a"),
    c("Area Nao Vegetada", "Non Vegetated Area", "Outras Areas nao Vegetadas", "Other Non Vegetated Areas", 25, "#ff99ff"),
    c("Corpo D'agua", "Water", "Corpo D'agua", "Water", 26, "#0000ff"),
    c("Nao Observado", "Non Observed", "Nao Observado", "Non Observed", 27, "#D5D5E5"),
    c("Formacao Natural Nao Florestal", "Non Forest Natural Formation", "Afloramento Rochoso", "Rocky Outcrop", 29, "#ff8C00"),
    c("Area Nao Vegetada", "Non Vegetated Area", "Mineracao", "Mining", 30, "#8a2be2"),
    c("Corpo D'agua", "Water", "Aquicultura", "Aquaculture", 31, "#29eee4"),
    c("Formacao Natural Nao Florestal", "Non Forest Natural Formation", "Apicum", "Salt Flat", 32, "#968c46"),
    c("Corpo D'agua", "Water", "Rio, Lago e Oceano", "River, Lake and Ocean", 33, "#0000ff"),
    c("Agropecuaria", "Farming", "Lavoura Perene", "Perennial Crop", 36, "#f3b4f1"),
    c("Agropecuaria", "Farming", "Soja", "Soybean", 39, "#c59ff4"),
    c("Agropecuaria", "Farming", "Arroz", "Rice", 40, "#982c9e"),
    c("Agropecuaria", "Farming", "Outras Lavouras Temporarias", "Other Temporary Crops", 41, "#CCFFCC"),
    c("Agropecuaria", "Farming", "Cafe", "Coffee", 46, "#cca0d4"),
    c("Agropecuaria", "Farming", "Citrus", "Citrus", 47, "#d082de"),
    c("Agropecuaria", "Farming", "Outras Lavouras Perenes", "Other Perennial Crops", 48, "#cd49e4"),
    c("Floresta", "Forest", "Restinga Arborizada", "Wooded Sandbank Vegetation", 49, "#6b9932"),
    c("Formacao Natural Nao Florestal", "Non Forest Natural Formation", "Restinga Herbacea", "Herbaceous Sandbank Vegetation", 50, "#66ffcc"),
    c("Agropecuaria", "Farming", "Algodao", "Cotton", 62, "#660066")
    ))
# Sub-titles for collection 10  
  dict_10 <- data.frame(rbind(
    c("Floresta", "Forest", "Floresta", "Forest", 1, "#1f8d49"),
    c("Floresta", "Forest", "Forma\u00e7\u00e3o Florestal", "Forest Formation", 3, "#1f8d49"),
    c("Floresta", "Forest", "Forma\u00e7\u00e3o Sav\u00e2nica", "Savanna Formation", 4, "#7dc975"),
    c("Floresta", "Forest", "Mangue", "Mangrove", 5, "#04f8fd"),
    c("Floresta", "Forest", "Floresta Alag\u00e1vel", "Floodable Forest", 6, "#007785"),
    c("Floresta", "Forest", "Restinga Arb\u00f3rea", "Wooded Sandbank Vegetation", 49, "#02d659"),
    c("Vegeta\u00e7\u00e3o Herb\u00e1cea e Arbustiva", "Herbaceous and Shrubby Vegetation", "Vegeta\u00e7\u00e3o Herb\u00e1cea e Arbustiva", "Herbaceous and Shrubby Vegetation", 10, "#d6bc74"),
    c("Vegeta\u00e7\u00e3o Herb\u00e1cea e Arbustiva", "Herbaceous and Shrubby Vegetation", "Campo Alagado e \u00c1rea Pantanosa", "Wetland", 11, "#519799"),
    c("Vegeta\u00e7\u00e3o Herb\u00e1cea e Arbustiva", "Herbaceous and Shrubby Vegetation", "Forma\u00e7\u00e3o Campestre", "Grassland", 12, "#d6bc74"),
    c("Vegeta\u00e7\u00e3o Herb\u00e1cea e Arbustiva", "Herbaceous and Shrubby Vegetation", "Apicum", "Hypersaline Tidal Flat", 32, "#fc8114"),
    c("Vegeta\u00e7\u00e3o Herb\u00e1cea e Arbustiva", "Herbaceous and Shrubby Vegetation", "Afloramento Rochoso", "Rocky Outcrop", 29, "#ffaa5f"),
    c("Vegeta\u00e7\u00e3o Herb\u00e1cea e Arbustiva", "Herbaceous and Shrubby Vegetation", "Restinga Herb\u00e1cea", "Herbaceous Sandbank Vegetation", 50, "#ad5100"),
    c("Agropecu\u00e1ria", "Farming", "Agropecu\u00e1ria", "Farming", 14, "#ffefc3"),
    c("Agropecu\u00e1ria", "Farming", "Pastagem", "Pasture", 15, "#edde8e"),
    c("Agropecu\u00e1ria", "Farming", "Agricultura", "Agriculture", 18, "#F974ED"),
    c("Agropecu\u00e1ria", "Farming", "Lavoura Tempor\u00e1ria", "Temporary Crop", 19, "#C27BA0"),
    c("Agropecu\u00e1ria", "Farming", "Soja", "Soybean", 39, "#f5b3c8"),
    c("Agropecu\u00e1ria", "Farming", "Cana", "Sugar cane", 20, "#db7093"),
    c("Agropecu\u00e1ria", "Farming", "Arroz", "Rice", 40, "#c71585"),
    c("Agropecu\u00e1ria", "Farming", "Algod\u00e3o", "Cotton", 62, "#ff69b4"),
    c("Agropecu\u00e1ria", "Farming", "Outras Lavouras Tempor\u00e1rias", "Other Temporary Crops", 41, "#f54ca9"),
    c("Agropecu\u00e1ria", "Farming", "Lavoura Perene", "Perennial Crop", 36, "#d082de"),
    c("Agropecu\u00e1ria", "Farming", "Caf\u00e9", "Coffee", 46, "#d68fe2"),
    c("Agropecu\u00e1ria", "Farming", "Citrus", "Citrus", 47, "#9932cc"),
    c("Agropecu\u00e1ria", "Farming", "Dend\u00ea", "Palm Oil", 35, "#9065d0"),
    c("Agropecu\u00e1ria", "Farming", "Outras Lavouras Perenes", "Other Perennial Crops", 48, "#e6ccff"),
    c("Agropecu\u00e1ria", "Farming", "Silvicultura", "Forest Plantation", 9, "#7a5900"),
    c("Agropecu\u00e1ria", "Farming", "Mosaico de Usos", "Mosaic of Uses", 21, "#ffefc3"),
    c("\u00c1rea n\u00e3o Vegetada", "Non vegetated area", "\u00c1rea n\u00e3o Vegetada", "Non vegetated area", 22, "#d4271e"),
    c("\u00c1rea n\u00e3o Vegetada", "Non vegetated area", "Praia, Duna e Areal", "Beach, Dune and Sand Spot", 23, "#ffa07a"),
    c("\u00c1rea n\u00e3o Vegetada", "Non vegetated area", "\u00c1rea Urbanizada", "Urban Area", 24, "#d4271e"),
    c("\u00c1rea n\u00e3o Vegetada", "Non vegetated area", "Minera\u00e7\u00e3o", "Mining", 30, "#9c0027"),
    c("\u00c1rea n\u00e3o Vegetada", "Non vegetated area", "Usina Fotovoltaica", "Photovoltaic Power Plant", 75, "#c12100"),
    c("\u00c1rea n\u00e3o Vegetada", "Non vegetated area", "Outras \u00c1reas n\u00e3o Vegetadas", "Other non Vegetated Areas", 25, "#db4644"),
    c("Corpo D'\u00e1gua", "Water", "Corpo D'\u00e1gua", "Water", 26, "#2532e4"),
    c("Corpo D'\u00e1gua", "Water", "Rio, Lago e Oceano", "River, Lake and Ocean", 33, "#2532e4"),
    c("Corpo D'\u00e1gua", "Water", "Aquicultura", "Aquaculture", 31, "#091077"),
    c("N\u00e3o observado", "Not Observed", "N\u00e3o observado", "Not Observed", 27, "#ffffff")
))

  # auxiliar object
if (collection == 7) {
    dict <- dict_7
  } else {
    dict <- dict_10
  }

  
  colnames(dict) <- c("grupo", "group", "classe", "class", "code", "color")

  dict$code <- as.numeric(dict$code)

  dict <- dict[order(-dict$code), ]

  return(dict)
}




