test_that("filtrer_cog fonctionne", {
  pop2015_cogifiee <- cogifier(pop2015,  communes = TRUE, code_commune = DEPCOM, epci = TRUE, departements = TRUE, regions = TRUE, metro = TRUE, as_df = TRUE)

  pop2015_cogifiee_Corse_1 <- pop2015_cogifiee %>%
       filtrer_cog(reg = "94", garder_supra = '>')

  pop2015_cogifiee_Corse_2 <- pop2015_cogifiee %>%
    filtrer_cog(reg = "94", garder_supra = '>=')

  pop2015_cogifiee_Corse_3 <- pop2015_cogifiee %>%
    filtrer_cog(reg = "94", garder_supra = 'non')

  expect_true("data.frame" %in% class(pop2015_cogifiee_Corse_1))
  expect_true(nrow(pop2015_cogifiee_Corse_2) > nrow(pop2015_cogifiee_Corse_1))
  expect_true(nrow(pop2015_cogifiee_Corse_3) < nrow(pop2015_cogifiee_Corse_1))

})
