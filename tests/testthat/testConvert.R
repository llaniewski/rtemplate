library(rtemplate)
context("RT tests")

RTeval = function(x) {
  exp = RTconvert(x)
  exp = parse(text=exp)
  con = textConnection("str","w")
  sink(con)
  ret = try(eval(exp),silent=TRUE)
  sink()
  close(con)
  if ("try-error" %in% class(ret)) stop(ret)
  str
}


test_that("Sanity 0", {
  expect_equal(RTeval("a"),"a")
  expect_equal(RTeval(c("a","b","c")),c("a","b","c"))
})


test_that("Sanity 1", {
  expect_equal(RTeval("<?R cat('a') ?>"),"a")
  expect_equal(RTeval(c("<?R","cat('a')","?>")),"a")
})

test_that("Sanity 2", {
  expect_error(RTeval("<?R"), 'Non matching')
  expect_error(RTeval("?>"), 'Non matching')
  expect_error(RTeval("<?R stop('error') ?>"), 'error')
  expect_equal(RTeval("stop('aa')"), "stop('aa')")
})

test_that("Var", {
  expect_equal(RTeval(c("<?R","a='a'","cat(a)","?>")),"a")
  expect_equal(RTeval(c("<?R","a='a'","?><?R","cat(a)","?>")),c("a"))
  expect_equal(RTeval(c("<?R","a='a'","?>","<?R","cat(a)","?>")),c("","a"))
})

test_that("If", {
  expect_equal(RTeval(c("<?R","b=3","if (b == 3) {","cat('a')","} else {","cat('b')","}","?>")),"a")
  expect_equal(RTeval(c("<?R","b=4","if (b == 3) {","cat('a')","} else {","cat('b')","}","?>")),"b")
  expect_equal(RTeval(c("<?R","b=3","if (b == 3) { ?>","a","<?R } else {?>","b","<?R }","?>")),c("","a",""))
  expect_equal(RTeval(c("<?R","b=4","if (b == 3) { ?>","a","<?R } else {?>","b","<?R }","?>")),c("","b",""))
})


