#' Construct the dictionary
#'
#' This function builds a dataframe that serves as a dictionary for a classification scheme.
#' The dictionary includes six fields: "group" in Portuguese and English, "class" in Portuguese
#' and English, a numerical "code", and a "color" associated with each class.
#'
#' @return A dataframe that serves as a dictionary for a classification scheme.

dict_build <- function(){

  dict <- data.frame(rbind(
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


  colnames(dict) <- c("grupo", "group", "classe", "class", "code", "color")

  dict$code <- as.numeric(dict$code)

  dict <- dict[order(-dict$code), ]

  return(dict)
}

