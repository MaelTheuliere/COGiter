test_that("ajouter_typologie fonctionne", {
  zonage_aav <- charger_zonage("CATEAAV2020")
  cogpop2015 <- cogifier(pop2015,metrodrom = T)
  cogpop2015aav <- ajouter_typologie(cogpop2015,
                                     zonage_df = zonage_aav)
  sum_avv <- cogpop2015aav %>%
    filter(TypeZone == "CATEAAV2020 - Départements") %>%
    pull(pop2015) %>%
    sum()
  sum_pop2015 <- sum(pop2015$pop2015)
  expect_equal(sum_avv, sum_pop2015)
})


test_that("ajouter_typologie fonctionne", {
  zonage_aav <- charger_zonage("AAV2020")
  cogpop2015 <- cogifier(pop2015,metrodrom = T)
  cogpop2015aav <- ajouter_zonage(cogpop2015,
                                     zonage_df = zonage_aav)
  sum_avv <- cogpop2015aav %>%
    filter(TypeZone == "AAV2020") %>%
    pull(pop2015) %>%
    sum()
  sum_pop2015 <- sum(pop2015$pop2015)
  expect_equal(sum_avv, sum_pop2015)
})
