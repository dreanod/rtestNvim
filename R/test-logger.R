#$ test_logger("tests/testthat/test-mytest.R")
#' @export
test_logger <- function(test_fn) {
  res <- testthat::test_file(test_fn, reporter = testthat::ListReporter)
  output <- list()
  idx <- 1

  for (i in seq_along(res)) {
    test_res <- res[[i]]
    for (j in seq_along(test_res$results)) {
      expect_res <- test_res$results[[j]]
      item <- list(
        line_number = expect_res$srcref[1],
        result = class(expect_res)[1],
        message = expect_res$message
      )
      output[[idx]] <- item
      idx <- idx + 1
    }
  }

  out_fn <- paste0(test_fn, ".json")

  jsonlite::toJSON(output, pretty = TRUE, auto_unbox = TRUE) |>
    readr::write_file(out_fn)
}
