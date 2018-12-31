context("settings")

test_that("standard settings", {
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
