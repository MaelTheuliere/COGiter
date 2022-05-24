test_that("nom_zone() fonctionne", {
  a <- nom_zone(c("Communes", "Communes", "Communes"), c("75056", "44109", "59080"))
  expect_equal(length(a), 3)
  expect_true("Nantes" == a[2])
})

test_that("code_zone() fonctionne", {
  expect_equal(code_zone("Communes", "Nantes"), "44109")
})

test_that("trouver_zone() fonctionne", {
  expect_true(trouver_zone(pattern = "Belleville") %>%  is_tibble())
})
