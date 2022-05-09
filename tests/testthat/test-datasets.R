test_that("les tables communes et communes_geo ont les mêmes identifiants", {
  cc <- communes %>% pull(DEPCOM) %>% as.character() %>% sort()
  ccc <- communes_geo %>% pull(DEPCOM) %>% as.character() %>% sort()
  expect_equal(cc, ccc)
})

test_that("les tables epci et epci_geo ont les mêmes identifiants", {
  cc <- epci %>% pull(EPCI) %>% as.character() %>% sort()
  ccc <- epci_geo %>% pull(EPCI) %>% as.character() %>% sort()
  expect_equal(cc, ccc)
})

test_that("les tables departements et departements_geo ont les mêmes identifiants", {
  cc <- departements %>% pull(DEP) %>% as.character() %>% sort()
  ccc <- departements_geo %>% pull(DEP) %>% as.character() %>% sort()
  expect_equal(cc, ccc)
})

test_that("les tables regions et regions_geo ont les mêmes identifiants", {
  cc <- regions %>% pull(REG) %>% as.character() %>% sort()
  ccc <- regions_geo %>% pull(REG) %>% as.character() %>% sort()
  expect_equal(cc, ccc)
})

test_that("les tables geo communales des doms et france metro représentent France entiere", {
  expect_true(nrow(communes_geo) == nrow(communes_metro_geo) + nrow(communes_971_geo) + nrow(communes_972_geo) + nrow(communes_973_geo) +
                                            nrow(communes_974_geo) + nrow(communes_976_geo))
})

test_that("les tables géo EPCI des doms et france metro représentent France entiere", {
  expect_true(nrow(epci_geo) == nrow(epci_metro_geo) + nrow(epci_971_geo) + nrow(epci_972_geo) + nrow(epci_973_geo) + nrow(epci_974_geo) + nrow(epci_976_geo))
})

test_that("les tables geo départementales des doms et france metro représentent France entiere", {
  expect_true(nrow(departements_geo) == nrow(departements_metro_geo) + nrow(departements_971_geo) + nrow(departements_972_geo) + nrow(departements_973_geo) +
                                                nrow(departements_974_geo) + nrow(departements_976_geo))
})

test_that("les tables geo régionales des doms et france metro représentent France entiere", {
  expect_true(nrow(regions_geo) == nrow(regions_metro_geo) + nrow(regions_971_geo) + nrow(regions_972_geo) + nrow(regions_973_geo) +
                                            nrow(regions_974_geo) + nrow(regions_976_geo))
})
