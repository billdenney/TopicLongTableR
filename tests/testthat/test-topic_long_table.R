test_that("topic_long_table works", {
  expect_equal(
    topic_long_table(data.frame(A=rep(1:2, each=4), B=rep(rep(3:4, each=2), 2), C=5:12), topic_cols=2),
    structure(
      paste(
        "\\begin{topiclongtable}{|Fr|Tr|Tr|}",
        "",
        "",
        "\\hline A & B & C \\\\",
        "\\hline",
        "\\endfirsthead",
        "\\multicolumn{3}{@{}l}{\\ldots continued} \\\\",
        "\\hline A & B & C \\\\",
        "\\hline",
        "\\endhead",
        "\\hline",
        "\\multicolumn{3}{r@{}}{continued \\ldots} \\\\",
        "\\endfoot",
        "\\hline",
        "\\endlastfoot",
        "\\TopicLine \\Topic[1] & \\Topic[3] &  5 \\\\\\nopagebreak[4]",
        "\\TopicLine \\Topic[1] & \\Topic[3] &  6 \\\\\\pagebreak[2]",
        "\\TopicLine \\Topic[1] & \\Topic[4] &  7 \\\\\\nopagebreak[4]",
        "\\TopicLine \\Topic[1] & \\Topic[4] &  8 \\\\\\pagebreak[3]",
        "\\TopicLine \\Topic[2] & \\Topic[3] &  9 \\\\\\nopagebreak[4]",
        "\\TopicLine \\Topic[2] & \\Topic[3] & 10 \\\\\\pagebreak[2]",
        "\\TopicLine \\Topic[2] & \\Topic[4] & 11 \\\\\\nopagebreak[4]",
        "\\TopicLine \\Topic[2] & \\Topic[4] & 12 \\\\",
        "\\end{topiclongtable}",
        sep = "\n"
      ),
      class="topic_long_table"
    )
  )
})

test_that("topic_long_table_document_headers", {
  expect_output(
    topic_long_table_document_headers(),
    regexp = "header-includes:\n - \\usepackage{siunitx}\n - \\usepackage{topiclongtable}",
    fixed = TRUE
  )
})

test_that("knit_print.topic_long_table", {
  expect_equal(
    knit_print.topic_long_table("a"),
    knitr::asis_output("a", cacheable = TRUE)
  )
})
