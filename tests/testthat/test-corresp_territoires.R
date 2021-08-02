context("code_reg_existe")

test_that("code_reg_existe works", {

  expect_warning(code_reg_existe("00"))
  expect_equal(code_reg_existe("04"), "OK")
})


context("code_dep_existe")

test_that("code_dep_existe works", {

  expect_warning(code_dep_existe("00"))
  expect_equal(code_dep_existe("974"), "OK")
})


context("code_epci_existe")

test_that("code_epci_existe works", {

  expect_warning(code_epci_existe("00"))
  expect_equal(code_epci_existe("249740077"), "OK")
})


context("code_com_existe")

test_that("code_com_existe works", {

  expect_warning(code_com_existe("00"))
  expect_equal(code_com_existe("97416"), "OK")
})


context("list_dep_in_reg")

test_that("list_dep_in_reg works", {

  expect_warning(code_com_existe("00"))
  expect_equal(list_dep_in_reg("04"), "974")
})


context("code_reg_of_dep")

test_that("code_reg_of_dep works", {

  expect_warning(code_com_existe("00"))
  expect_equal(code_reg_of_dep("974"), "04")
})


context("list_epci_in_dep")

test_that("list_epci_in_dep works", {

  expect_warning(code_com_existe("00"))
  expect_length(list_epci_in_dep("974"), 5)
})


context("code_dep_of_epci")

test_that("code_dep_of_epci works", {

  expect_warning(code_com_existe("00"))
  expect_equal(code_dep_of_epci("249740077"), "974")
})


context("list_com_in_epci")

test_that("list_com_in_epci works", {

  expect_warning(code_com_existe("00"))
  expect_length(list_com_in_epci("249740077"), 6)
})


context("code_epci_of_com")

test_that("code_epci_of_com works", {

  expect_warning(code_com_existe("00"))
  expect_equal(code_epci_of_com("97416"), "249740077")
})


context("code_dep_of_com")

test_that("code_dep_of_com works", {

  expect_warning(code_com_existe("00"))
  expect_equal(code_dep_of_com("97416"), "974")
})


context("list_com_in_dep")

test_that("list_com_in_dep works", {

  expect_warning(code_com_existe("00"))
  expect_length(list_com_in_dep("974"), 24)
})
