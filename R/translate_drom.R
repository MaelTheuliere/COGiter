#' Fonction interne de translation des DROM pour construire la carte france métro et DROM
#'
#' @param code_dom code departement du drom
#' @param destination x,y du lieu de destination dans l'epsg défini
#' @param scale facteur de redimentionnement du territoire
#' @param angle facteur de rotation du territoire
#' @param epsg code epsg dans lequel effectuer la transformation
#' @importFrom sf st_read
#' @importFrom sf st_transform
#' @importFrom sf st_centroid
#' @importFrom sf st_geometry
#' @importFrom sf st_set_geometry
#' @importFrom sf st_crs
#' @importFrom sf st_set_crs
#' @importFrom dplyr summarise
#' @return un spatial dataframe

translate_drom <- function(code_dom,destination,scale=1,angle=0,epsg=2154) {

  if (code_dom != '976'){

    com_dom <- st_read(paste0("data-raw/source/2019/adminexpress/",code_dom,"/COMMUNE_CARTO.shp")) %>%
      st_transform(epsg)
  }
  else {

    com_dom <- st_read(paste0("data-raw/source/2019/adminexpress/",code_dom,"/COMMUNE.shp")) %>%
      st_transform(epsg)
  }

  # centroid d'origine
  centroid_com_dom_sfc <- st_centroid(st_geometry(com_dom %>%
                                                    summarise()))
  origine <- centroid_com_dom_sfc[[1]]

  com_dom_sfc <- st_geometry(com_dom)
  rotation = function(a){
    r = a * pi / 180 #degrees to radians
    matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
  }

  com_dom_sfc_middle <- (com_dom_sfc-centroid_com_dom_sfc)*scale* rotation(angle) + centroid_com_dom_sfc
  com_dom_sfc_trans <- com_dom_sfc_middle + c(destination[1]-origine[1], destination[2]-origine[2])
  com_dom_trans <- st_set_geometry(com_dom, com_dom_sfc_trans)
  st_set_crs(com_dom_trans, st_crs(com_dom))

  return(com_dom_trans)
}
