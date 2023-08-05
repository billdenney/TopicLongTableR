test_that("topic_long_table works", {
  expect_equal(
    topic_long_table(data.frame(A=rep(1:2, each=4), B=rep(rep(3:4, each=2), 2), C=5:12), topic_cols=2),
    structure(
      "\\begin{topiclongtable}{|Fr|Tr|Tr|}\n\n\n\\hline A & B & C \\\\\n\\hline\n\\endfirsthead\n\\multicolumn{3}{@{}l}{\\ldots continued} \\\\\n\\hline A & B & C \\\\\n\\hline\n\\endhead\n\\hline\n\\multicolumn{3}{r@{}}{continued \\ldots} \\\\\n\\endfoot\n\\hline\n\\endlastfoot\n\\TopicLine \\Topic[1] & \\Topic[3] &  5 \\\\\n\\TopicLine \\Topic[1] & \\Topic[3] &  6 \\\\\n\\TopicLine \\Topic[1] & \\Topic[4] &  7 \\\\\n\\TopicLine \\Topic[1] & \\Topic[4] &  8 \\\\\n\\TopicLine \\Topic[2] & \\Topic[3] &  9 \\\\\n\\TopicLine \\Topic[2] & \\Topic[3] & 10 \\\\\n\\TopicLine \\Topic[2] & \\Topic[4] & 11 \\\\\n\\TopicLine \\Topic[2] & \\Topic[4] & 12 \\\\\n\\end{topiclongtable}",
      class="topic_long_table"
    )
  )
})
