library(rtemplate)
context("Tests of RTchunks")

test_that("One-liner", {
  lines = c("<?R a=7 ?>")
  ret = RTchunks(lines)
  expect_equal(ret$start.line, c(1))
  expect_equal(ret$start.char, c(4))
  expect_equal(ret$end.line, c(1))
  expect_equal(ret$end.char, c(8))
  expect_equal(as.character(ret$tag), c("R"))
})

test_that("Only text", {
  lines = c("Hello world!")
  ret = RTchunks(lines)
  expect_equal(ret$start.line, c(1))
  expect_equal(ret$start.char, c(1))
  expect_equal(ret$end.line, c(1))
  expect_equal(ret$end.char, c(12))
  expect_equal(as.character(ret$tag), c(""))
})

test_that("Multi-liner", {
  lines = c("Hello world!", "<?R a=7 ?>", "Bye world!")
  ret = RTchunks(lines)
  expect_equal(ret$start.line, c(1,2,2))
  expect_equal(ret$start.char, c(1,4,11))
  expect_equal(ret$end.line, c(2,2,3))
  expect_equal(ret$end.char, c(0,8,10))
  expect_equal(as.character(ret$tag), c("","R",""))
})

test_that("Multi-liner-span", {
  lines = c("<?R a=7", "b=7", "c=7 ?>")
  ret = RTchunks(lines)
  expect_equal(ret$start.line, c(1))
  expect_equal(ret$start.char, c(4))
  expect_equal(ret$end.line, c(3))
  expect_equal(ret$end.char, c(4))
  expect_equal(as.character(ret$tag), c("R"))
})

test_that("Evil Multi-liner-span", {
  lines = c("<?R", "b=7", "?>")
  ret = RTchunks(lines)
  expect_equal(ret$start.line, c(1))
  expect_equal(ret$start.char, c(4))
  expect_equal(ret$end.line, c(3))
  expect_equal(ret$end.char, c(0))
  expect_equal(as.character(ret$tag), c("R"))
})

test_that("percent syntax", {
  lines = c("<?%d a ?>")
  ret = RTchunks(lines)
  expect_equal(ret$start.line, c(1))
  expect_equal(ret$start.char, c(5))
  expect_equal(ret$end.line, c(1))
  expect_equal(ret$end.char, c(7))
  expect_equal(as.character(ret$tag), c("%d"))
})

test_that("Tag mismatch 1", {
  expect_that(RTchunks(c("<?R")), throws_error())
  expect_that(RTchunks(c("?>")), throws_error())
  expect_that(RTchunks(c("<?R <?R ?>")), throws_error())
  expect_that(RTchunks(c("<?R <?R ?> ?>")), throws_error())
  expect_that(RTchunks(c("<?R ?> ?>")), throws_error())

})
