context("table body")

test_that("simple table body generation", {
  expect_equal(
    topic_long_table_body(data.frame(A=1), topic_cols=1),
    "\\TopicLine \\Topic[1] \\\\"
  )
  expect_equal(
    topic_long_table_body(data.frame(A=1, B=2), topic_cols=1),
    "\\TopicLine \\Topic[1] & 2 \\\\"
  )
  expect_equal(
    topic_long_table_body(data.frame(A=1:2, B=3:4), topic_cols=1),
    "\\TopicLine \\Topic[1] & 3 \\\\\n\\TopicLine \\Topic[2] & 4 \\\\"
  )
})

test_that("body arguments are respected", {
  expect_equal(
    topic_long_table_body(data.frame(A=1), topic_cols=1, row_border="foo"),
    "foo \\Topic[1] \\\\"
  )
  expect_equal(
    topic_long_table_body(data.frame(A=1:2, B=3:4), topic_cols=1, row_border="foo"),
    "foo \\Topic[1] & 3 \\\\\nfoo \\Topic[2] & 4 \\\\"
  )
})