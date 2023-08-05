test_that("normal headers work", {
  expect_equal(
    topic_long_table_header(data.frame(A=1, B=2)),
    "\\hline A & B \\\\\n\\hline\n\\endfirsthead\n\\multicolumn{2}{@{}l}{\\ldots continued} \\\\\n\\hline A & B \\\\\n\\hline\n\\endhead",
    info="basic function, two columns"
  )
  expect_equal(
    topic_long_table_header(data.frame(A=1)),
    "\\hline A \\\\\n\\hline\n\\endfirsthead\n\\multicolumn{1}{@{}l}{\\ldots continued} \\\\\n\\hline A \\\\\n\\hline\n\\endhead",
    info="basic function, one column"
  )
  expect_equal(
    topic_long_table_header(matrix(1:2, ncol=1)),
    "\\hline  \\\\\n\\hline\n\\endfirsthead\n\\multicolumn{1}{@{}l}{\\ldots continued} \\\\\n\\hline  \\\\\n\\hline\n\\endhead",
    info="no header text, one column"
  )
  expect_equal(
    topic_long_table_header(matrix(1:2, ncol=2)),
    "\\hline  \\\\\n\\hline\n\\endfirsthead\n\\multicolumn{2}{@{}l}{\\ldots continued} \\\\\n\\hline  \\\\\n\\hline\n\\endhead",
    info="no header text, one column"
  )
})

test_that("arguments are respected", {
  expect_equal(
    topic_long_table_header(data.frame(A=1), col_names=c("B")),
    "\\hline B \\\\\n\\hline\n\\endfirsthead\n\\multicolumn{1}{@{}l}{\\ldots continued} \\\\\n\\hline B \\\\\n\\hline\n\\endhead"
  )
  expect_error(topic_long_table_header(data.frame(A=1), col_names=c("B", "C")))
  expect_equal(
    topic_long_table_header(data.frame(A=1), above_col_names="foo"),
    "foo A \\\\\n\\hline\n\\endfirsthead\n\\multicolumn{1}{@{}l}{\\ldots continued} \\\\\nfoo A \\\\\n\\hline\n\\endhead"
  )
  expect_equal(
    topic_long_table_header(data.frame(A=1), below_col_names="foo"),
    "\\hline A \\\\\nfoo\n\\endfirsthead\n\\multicolumn{1}{@{}l}{\\ldots continued} \\\\\n\\hline A \\\\\nfoo\n\\endhead"
  )
  expect_equal(
    topic_long_table_header(data.frame(A=1), subsequent_page_notification="baz"),
    "\\hline A \\\\\n\\hline\n\\endfirsthead\n\\multicolumn{1}{@{}l}{baz} \\\\\n\\hline A \\\\\n\\hline\n\\endhead"
  )
  expect_warning(
    result <- topic_long_table_header(data.frame(A=1), latex_header="foo"),
    regexp = "`above_col_names`, `below_col_names`, `subsequent_page_notification` were provided but will be ignored in favor of `latex_header`"
  )
  expect_equal(result, "foo")
  expect_warning(
    expect_error(
      topic_long_table_header(data.frame(A=1), latex_header=1:2),
      regexp = "latex_header must have length == 1 (for no header, use an empty string, '').",
      fixed = TRUE
    ),
    "`above_col_names`, `below_col_names`, `subsequent_page_notification` were provided but will be ignored in favor of `latex_header`."
  )
})

test_that("topic_long_table_header verbatim", {
  expect_equal(
    topic_long_table_header(verbatim = "foo"),
    "foo"
  )
  expect_error(
    topic_long_table_header(verbatim = 1),
    "`verbatim` must be a character scalar."
  )
  expect_error(
    topic_long_table_header(verbatim = c("a", "b")),
    "`verbatim` must be a character scalar."
  )
})

test_that("topic_long_table_header col_names_part", {
  expect_equal(
    topic_long_table_header(matrix(1:4, nrow = 2)),
    "\\hline\n\\endfirsthead\n\\multicolumn{2}{@{}l}{\\ldots continued} \\\\\n\\hline\n\\endhead"
  )
  expect_equal(
    topic_long_table_header(matrix(1:4, nrow = 2), below_col_names = "foo"),
    "\\hline \\\\\nfoo\n\\endfirsthead\n\\multicolumn{2}{@{}l}{\\ldots continued} \\\\\n\\hline \\\\\nfoo\n\\endhead"
  )
})

test_that("topic_long_table_header col_names_part", {
  expect_equal(
    topic_long_table_header(data.frame(A=1), subsequent_page_notification = NULL),
    "\\hline A \\\\\n\\hline\n\\endhead"
  )
})

test_that("standard footer generation", {
  expect_equal(
    topic_long_table_footer(x=data.frame(A=1)),
    "\\hline\n\\multicolumn{1}{r@{}}{continued \\ldots} \\\\\n\\endfoot\n\\hline\n\\endlastfoot"
  )
  expect_equal(
    topic_long_table_footer(x=matrix(1:4, ncol=1)),
    "\\hline\n\\multicolumn{1}{r@{}}{continued \\ldots} \\\\\n\\endfoot\n\\hline\n\\endlastfoot"
  )
  expect_equal(
    topic_long_table_footer(x=matrix(1:4, ncol=4)),
    "\\hline\n\\multicolumn{4}{r@{}}{continued \\ldots} \\\\\n\\endfoot\n\\hline\n\\endlastfoot"
  )
})

test_that("footer options", {
  expect_equal(
    topic_long_table_footer(x=data.frame(A=1), bottom_border="foo"),
    "foo\n\\multicolumn{1}{r@{}}{continued \\ldots} \\\\\n\\endfoot\nfoo\n\\endlastfoot"
  )
  expect_equal(
    topic_long_table_footer(x=data.frame(A=1), bottom_all_pages="foo"),
    "\\hline\nfoo\n\\multicolumn{1}{r@{}}{continued \\ldots} \\\\\n\\endfoot\n\\hline\nfoo\n\\endlastfoot"
  )
  expect_equal(
    topic_long_table_footer(x=data.frame(A=1), bottom_last_page="foo"),
    "\\hline\n\\multicolumn{1}{r@{}}{continued \\ldots} \\\\\n\\endfoot\n\\hline\nfoo\n\\endlastfoot"
  )
  expect_equal(
    topic_long_table_footer(x=data.frame(A=1), subsequent_page_notification="foo"),
    "\\hline\n\\multicolumn{1}{r@{}}{foo} \\\\\n\\endfoot\n\\hline\n\\endlastfoot"
  )
})

test_that("topic_long_table_footer verbatim", {
  expect_equal(
    topic_long_table_footer(verbatim = "foo"),
    "foo"
  )
  expect_error(
    topic_long_table_footer(verbatim = 1),
    "`verbatim` must be a character scalar."
  )
  expect_error(
    topic_long_table_footer(verbatim = c("a", "b")),
    "`verbatim` must be a character scalar."
  )
})
