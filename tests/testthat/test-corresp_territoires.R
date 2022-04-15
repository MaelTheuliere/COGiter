test_that("code_reg_existe works", {

  expect_false(code_reg_existe("00"))
  expect_true(code_reg_existe("04"))
})

test_that("code_dep_existe works", {
  expect_false(code_dep_existe("00"))
  expect_true(code_dep_existe("974"))
})

test_that("code_epci_existe works", {
  expect_false(code_epci_existe("00"))
  expect_true(code_epci_existe("249740077"))
})

test_that("code_com_existe works", {
  expect_false(code_com_existe("00"))
  expect_true(code_com_existe("97416"))
})

test_that("list_dep_in_reg works", {
  expect_false(code_com_existe("00"))
  expect_equal(list_dep_in_reg("04"), "974")
})

test_that("code_reg_of_dep works", {
  expect_equal(code_reg_of_dep("974"), "04")
})

test_that("list_epci_in_dep works", {
  expect_length(list_epci_in_dep("974"), 5)
})

test_that("code_dep_of_epci works", {
  expect_equal(unlist(code_dep_of_epci("249740077")), "974")
})

test_that("list_com_in_epci works", {
  expect_length(list_com_in_epci("249740077"), 6)
})

test_that("code_epci_of_com works", {
  expect_equal(code_epci_of_com("97416"), "249740077")
})

test_that("code_dep_of_com works", {
  expect_equal(code_dep_of_com("97416"), "974")
})

test_that("list_com_in_dep works", {
  expect_length(list_com_in_dep("974"), 24)
})
