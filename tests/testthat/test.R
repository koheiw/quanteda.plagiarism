
toks <- tokens(c("a b c d", "z z", "a b c"))

test_that("textstat_plagiarism is working", {

    expect_error(textstat_plagiarism(toks, size = 5))

    plag <- textstat_plagiarism(
        toks,
        size = 3,
        min_simil = 0.9
    )

    expect_equal(
        names(plag),
        c("plagiarism", "segment", "dimnames", "call")
    )

    expect_equal(
        as.matrix(plag$plagiarism),
        matrix(c(FALSE, FALSE, TRUE,
                 FALSE, FALSE, FALSE,
                 TRUE, FALSE, FALSE),
               nrow = 3,
               dimnames = list(c("text1.1", "text1.2", "text3.1"),
                              c("text1.1", "text1.2", "text3.1")))
    )

})

test_that("textstat_plagiarism is working", {

    plag <- textstat_plagiarism(
        toks,
        size = 3,
        min_simil = 0.9
    )

    expect_equal(
        names(as.list(plag, upper = TRUE)),
        c("text1.1", "text3.1")
    )
    expect_equal(
        names(as.list(plag, upper = FALSE)),
        c("text3.1")
    )

    expect_equal(
        as.matrix(as.matrix(plag, upper = TRUE)),
        matrix(c(0, 1, 1, 0),
               nrow = 2,
               dimnames = list(c("text1", "text3"),
                               c("text1", "text3")))
    )
    expect_equal(
        as.matrix(as.matrix(plag, upper = FALSE)),
        matrix(c(0, 1, 0, 0),
               nrow = 2,
               dimnames = list(c("text1", "text3"),
                               c("text1", "text3")))
    )
})
