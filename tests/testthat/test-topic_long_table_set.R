test_that("topic_long_table_set", {
  expect_error(topic_long_table_set())
  expect_error(topic_long_table_set(vpos="foo"))
  expect_equal(
    topic_long_table_set(continuation_code="foo"),
    "\\TopicSetContinuationCode{foo}"
  )
  expect_equal(
    topic_long_table_set(vpos="t"),
    "\\TopicSetVPos(t)"
  )
  expect_equal(
    topic_long_table_set(continuation_code="foo", vpos="t"),
    "\\TopicSetVPos(t)\n\\TopicSetContinuationCode{foo}"
  )
  expect_equal(
    topic_long_table_set(reset=TRUE),
    "\\TopicSetVPos(t)\n\\TopicSetContinuationCode{}"
  )
})

test_that("topic_long_table_set expected warnings and errors", {
  expect_error(
    topic_long_table_set(continuation_code = 1:2),
    regexp = "`continuation_code` must be a scalar."
  )
  expect_warning(
    topic_long_table_set(continuation_code = 1),
    regexp = "`continuation_code` was converted to a character."
  )
})
