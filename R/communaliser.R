communaliser <- function(.data, supra=F) {
  
  stopifnot("sf" %in% class(.data))
  stopifnot("sfc_POINT" %in% class(st_geometry(.data)))
  
  if("DEPCOM" %in% names(.data)) {result <- remane(.data, DEPCOM_old=DEPCOM)}
  
  if (supra) {
    result <- st_transform(.data, crs=3857) %>%
      st_join(left_join(communes_geo_world, communes, by="DECOM"), join=st_intersects, left = T, largest = F)
  } else {
    result <- st_transform(.data, crs=3857) %>%
      st_join(communes_geo_world, join=st_intersects, left = T, largest = F)
  }
  
  print(paste0(nrow(filter(result,!is.na(DEPCOM))), " objets sur ", nrow(.data), " disposent maintenant d'un code commune à jour."))
  
  result
  
}


### exemple

step2<-communaliser(step)
rm(step2)

