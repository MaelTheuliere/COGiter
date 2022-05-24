test_that("filtrer_cog_geo fonctionne", {

  nantes_metropole <- filtrer_cog_geo(epci = '244400404')
  expect_true(is.list(nantes_metropole))
  expect_true(length(nantes_metropole) == 2)
  expect_true("sf" %in% class(nantes_metropole$communes))
  expect_true(sf::st_crs(nantes_metropole$epci)$input == "EPSG:2154")

  la_reunion <- filtrer_cog_geo(dep = '974')
  expect_true(is.list(la_reunion))
  expect_true(length(la_reunion) == 3)
  expect_true("sf" %in% class(la_reunion$communes))
  expect_true(sf::st_crs(la_reunion$epci)$input == "EPSG:2975")

  expect_error(filtrer_cog_geo(dep = 44))
})
