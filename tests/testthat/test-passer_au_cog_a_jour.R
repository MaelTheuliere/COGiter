test_that("passer_au_cog_a_jour conserve les totaux", {
  expect_equal(pop2015 %>% COGiter::passer_au_cog_a_jour(na.rm = TRUE) %>% dplyr::pull(pop2015) %>% sum(), sum(pop2015$pop2015))
  expect_equal(pop2015 %>% COGiter::passer_au_cog_a_jour(na.rm = FALSE) %>% dplyr::pull(pop2015) %>% sum(), sum(pop2015$pop2015))
})
