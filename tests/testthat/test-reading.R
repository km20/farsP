context("Reading")
test_that("Reading files works", {
  filename <- make_filename(2013)
  expect_equal(filename, "accident_2013.csv.bz2")

  d<- fars_read(filename)
  expect_equal(length(d), 50)
})

