test_that("single classes give expected alignment", {
  expect_equal(topic_long_table_alignment.numeric(), "r")
  expect_equal(topic_long_table_alignment.numeric(5), "r")
  expect_equal(topic_long_table_alignment.default(), "l")
  expect_equal(topic_long_table_alignment.default("A"), "l")
  expect_equal(topic_long_table_alignment.default(NA), "l")
  expect_equal(topic_long_table_alignment.default(TRUE), "l")
})

test_that("topic_long_table_alignment.data.frame", {
  expect_equal(
    topic_long_table_alignment.data.frame(verbatim = "abc"),
    "abc"
  )
  expect_error(
    topic_long_table_alignment.data.frame(verbatim = 1),
    "`verbatim` must be a character scalar."
  )
  expect_error(
    topic_long_table_alignment.data.frame(verbatim = c("a", "b")),
    "`verbatim` must be a character scalar."
  )
})

test_that("matrix alignment occurs by class and returns the right size output", {
  expect_equal(
    topic_long_table_alignment(matrix(rep(1, 4), ncol=1), topic_cols=1),
    "|Fr|",
    info="single column"
  )
  expect_equal(
    topic_long_table_alignment(matrix(rep(1, 4), ncol=4), topic_cols=1),
    "|Fr|Tr|Tr|Tr|",
    info="multiple column, one topic"
  )
  expect_equal(
    topic_long_table_alignment(matrix(rep(1, 4), ncol=4), topic_cols=1),
    "|Fr|Tr|Tr|Tr|",
    info="single column"
  )
  expect_equal(
    topic_long_table_alignment(matrix(rep(1, 4), ncol=4), topic_cols=3),
    "|Fr|Tr|Tr|Tr|",
    info="single column"
  )
  expect_equal(
    topic_long_table_alignment(data.frame(A=1, B="A", C=TRUE, D=NA_integer_), topic_cols=3),
    "|Fr|Tl|Tl|Tr|",
    info="single column"
  )
})

test_that("Alignment: Valid input provides valid output", {
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

test_that("Alignment: Invalid input provides an error", {
  expect_error(
    clean_align(data.frame(A=1), align=c("l", "l")),
    info="single column, multiple specification"
  )
  expect_error(
    clean_align(data.frame(A=1, B=2), align=c("l", "l", "l")),
    info="multiple column, different multiple specification"
  )
  expect_warning(
    clean_align(data.frame(A=1), align="z"),
    info="unknown alignment"
  )
})
