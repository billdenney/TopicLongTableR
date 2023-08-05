test_that("caption standard use", {
  expect_equal(
    topic_long_table_caption(structure(data.frame(), caption="foo"), text="bar", label="baz"),
    "\\caption{bar\\label{baz}} \\\\"
  )
  expect_equal(
    topic_long_table_caption(structure(data.frame(), caption="foo"), text="bar"),
    "\\caption{bar} \\\\"
  )
  expect_equal(
    topic_long_table_caption(structure(data.frame(), caption="foo")),
    "\\caption{foo} \\\\"
  )
  expect_equal(
    topic_long_table_caption(data.frame()),
    ""
  )
})

test_that("topic_long_table_caption verbatim", {
  expect_equal(
    topic_long_table_caption(verbatim = "foo"),
    "foo"
  )
  expect_error(
    topic_long_table_caption(verbatim = 1),
    "`verbatim` must be a character scalar."
  )
  expect_error(
    topic_long_table_caption(verbatim = c("a", "b")),
    "`verbatim` must be a character scalar."
  )
})

test_that("topic_long_table_caption latex_clean", {
  expect_equal(
    topic_long_table_caption(x = data.frame(), text = "a^2"),
    "\\caption{a$^{2}$} \\\\"
  )
  expect_equal(
    topic_long_table_caption(x = data.frame(), text = "a^2", latex_clean = FALSE),
    "\\caption{a^2} \\\\"
  )
})

test_that("topic_long_table_caption short/long", {
  expect_equal(
    topic_long_table_caption(x = data.frame(), text = c("a", "b")),
    "\\caption[a]{a b} \\\\"
  )
  expect_equal(
    topic_long_table_caption(x = data.frame(), text = c("a", "b"), combine_short_long = FALSE),
    "\\caption[a]{b} \\\\"
  )
  expect_error(
    topic_long_table_caption(x = data.frame(), text = c("a", "b", "c")),
    "Cannot handle caption text with length: 3"
  )
})
