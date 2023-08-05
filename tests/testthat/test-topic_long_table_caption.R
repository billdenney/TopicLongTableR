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
