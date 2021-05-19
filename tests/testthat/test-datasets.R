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
