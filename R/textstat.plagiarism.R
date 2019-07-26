#' A function to detects reused text segments
#'
#' This is an example of how to make a custom function based on quanteda.
#' @param x a \link[quanteda]{tokens} object in which texts are resued
#' @param y a \link[quanteda]{tokens} object from which texts are taken
#' @param size size of the reused text segments specified in number of tokens
#' @param min_simil minimum similarity of text segments that are treated as reuse
#' @examples
#'
#' require(quanteda)
#' quanteda_options(threads = 8)
#' corp <- data_corpus_inaugural
#' toks <- tokens(corp, remove_punct = TRUE) %>%
#'     tokens_remove(stopwords("en")) %>%
#'     tokens_select("^[\\p{L}]+$", valuetype = "regex")
#'
#' # unigram
#' tstat_plag <- textstat_plagiarism(
#'     toks[1:10],
#'     min_simil = 0.5
#' )
#' print(tstat_plag)
#' as.list(tstat_plag, upper = FALSE) %>% head()
#' as.matrix(tstat_plag, upper = FALSE)
#'
#' # bigram
#' tstat_plag2 <- textstat_plagiarism(
#'     tokens_ngrams(toks, 2),
#'     size = 5,
#'     min_simil = 0.5
#' )
#'
#' print(tstat_plag2)
#' as.list(tstat_plag2, upper = FALSE) %>% head()
#' as.matrix(tstat_plag2)
#' @export
#' @import quanteda
textstat_plagiarism <- function(x, y = NULL, size = 10, min_simil = 0.9) {

    if (all(ntoken(x) < size))
        stop("size is too large")

    x <- tokens_chunk(x, size, size - 1, use_docvars = FALSE)
    x <- segment <- x[lengths(x) >= size]
    x <- dfm(x, remove = "")
    if (!is.null(y)) {
        y <- tokens_chunk(y, size, size - 1, use_docvars = FALSE)
        y <- y[lengths(y) >= size]
        y <- dfm(y, remove = "")
    } else {
        y <- x
    }
    temp <- textstat_simil(x, y, margin = "document",
                           min_simil = min_simil, method = "cosine")
    diag(temp) <- 0
    result <- list(plagiarism = temp > min_simil,
                   segment = segment,
                   dimnames = list(
                       structure(docvars(x, "_document"),
                                 names = docnames(x)),
                       structure(docvars(y, "_document"),
                                 names = docnames(y))
                   ),
                   call = match.call())
    class(result) <- "textstat_plagiarism"
    return(result)
}

#' Returns reused text segments
#' @param x \code{textstat_plagiarism} object
#' @param upper if \code{TRUE}, cosider upper-triangular pairs. Set \code{FALSE}
#'   when documents are in chronological order.
#' @param ... not used
#' @method as.list textstat_plagiarism
#' @export
as.list.textstat_plagiarism <- function(x, upper = TRUE, ...) {
    if (!upper)
        x$plagiarism <- Matrix::tril(x$plagiarism)
    s <- sort(Matrix::rowSums(x$plagiarism), decreasing = TRUE)
    s <- s[s > 0]
    return(x$segment[names(s)])
}

#' Returns the number of reused text segments
#' @param x \code{textstat_plagiarism} object
#' @param upper if \code{TRUE}, cosider upper-triangular pairs. Set \code{FALSE}
#'   when documents are in chronological order.
#' @param ... not used
#' @method as.matrix textstat_plagiarism
#' @export
#' @import quanteda
as.matrix.textstat_plagiarism <- function(x, upper = TRUE, ...) {
    if (!upper)
        x$plagiarism <- Matrix::tril(x$plagiarism)
    temp <- as.dfm(x$plagiarism)
    rownames(temp) <- x$dimnames[[1]][rownames(temp)]
    colnames(temp) <- x$dimnames[[2]][colnames(temp)]
    result <- dfm_compress(temp)
    names(dimnames(result)) <- NULL
    as(result, "dgCMatrix")
}

#' @noRd
#' @method print textstat_plagiarism
#' @export
print.textstat_plagiarism <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        "Test: ",
        nrow(x$plagiarism), " chunks against ",
        ncol(x$plagiarism), " targets.",
        "\n",
        sep = "")
}
