context("Alignment by class")

test_that("single classes give expected alignment", {
  expect_equal(topic_long_table_alignment.numeric(), "S")
  expect_equal(topic_long_table_alignment.numeric(5), "S")
  expect_equal(topic_long_table_alignment.default(), "l")
  expect_equal(topic_long_table_alignment.default("A"), "l")
  expect_equal(topic_long_table_alignment.default(NA), "l")
  expect_equal(topic_long_table_alignment.default(TRUE), "l")
})

test_that("matrix alignment occurs by class and returns the right size output", {
  expect_equal(
    topic_long_table_alignment(matrix(rep(1, 4), ncol=1), topic_cols=1),
    "|FS|",
    info="single column"
  )
  expect_equal(
    topic_long_table_alignment(matrix(rep(1, 4), ncol=4), topic_cols=1),
    "|FS|S|S|S|",
    info="multiple column, one topic"
  )
  expect_equal(
    topic_long_table_alignment(matrix(rep(1, 4), ncol=4), topic_cols=1),
    "|FS|S|S|S|",
    info="single column"
  )
  expect_equal(
    topic_long_table_alignment(matrix(rep(1, 4), ncol=4), topic_cols=3),
    "|FS|TS|TS|S|",
    info="single column"
  )
  expect_equal(
    topic_long_table_alignment(data.frame(A=1, B="A", C=TRUE, D=NA_integer_), topic_cols=3),
    "|FS|Tl|Tl|S|",
    info="single column"
  )
})

context("Alignment cleaning")

test_that("Valid input provides valid output", {
  expect_equal(
    clean_align(data.frame(A=1), align="l"),
    "l",
    info="single column, single specification, single result"
  )
  expect_equal(
    clean_align(data.frame(A=1, B=2), align="l"),
    rep("l", 2),
    info="multiple column, single specification, multiple result"
  )
})

test_that("Invalid input provides an error", {
  expect_error(
    clean_align(data.frame(A=1), align=c("l", "l")),
    info="single column, multiple specification"
  )
  expect_error(
    clean_align(data.frame(A=1, B=2), align=c("l", "l", "l")),
    info="multiple column, different multiple specification"
  )
  expect_error(
    clean_align(data.frame(A=1), align="z"),
    info="invalid alignment"
  )
})
