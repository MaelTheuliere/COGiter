test_that("cogifier conserve les totaux", {
  expect_equal(pop2015 %>% COGiter::cogifier(na.rm = TRUE) %>% dplyr::filter(TypeZone == "Communes") %>% dplyr::pull(pop2015) %>% sum(), sum(pop2015$pop2015))
})
