#' Merge multiple responses for the pages of the same GitHub query
#'
#' Often there are more than 100 entries stored on the GitHub API but only 100
#' entries will be returned regardless of what value is passed to `per_page`.
#' Thus, this function combines the information returned from multiple calls to
#' the API for the same endpoint.
#'
#' @param res Result of the original query, or the result of the previous
#'   `merge_gh_pages()` call because this function is often used recursively.
#' @param res2 Result of the next page.
#' @return
#' Merged `res` and `res`, in the same format as the original response, with
#' the attributes from `res2`.
#' @inheritSection gh_pg author
#' @seealso
#' * [gh_pg()]
#' @noRd
merge_gh_pages <- function(res, res2) {
  res3 <- c(res, res2)
  attributes(res3) <- attributes(res2)
  res3
}

#' GitHub API query with conditional pagination
#'
#' Often there are more than 100 entries stored on the GitHub API but only 100
#' entries will be returned regardless of what value is passed to `per_page`.
#' Thus, this function repeats the call to the endpoint until all the pages
#' have been found. This search is limited by the function passed to `cond`.
#'
#' @param ... Parameters to pass to [gh::gh()], where the main argument will be
#'   `endpoint`, which contains the call sent to `GET`.
#' @param cond A callback function that is called with the response from
#'   [gh::gh()]. If it returns `TRUE` and there is a `next` page, then
#'   the next page is also requested.
#'
#' @return
#' The complete result, with all pages of the pagination merged.
#'
#' @author
#' GU+00E1bor CsU+00E1rdi (gaborcsardi) created this function in the myweek
#' package on GitHub. The function remains the same but some of the
#' documentation has been updated here.
#' @seealso
#' * [merge_gh_pages()]
#' @noRd
gh_pg <- function(..., cond = function(...) {TRUE}) {
  res <- r1 <- gh::gh(...)
  while (isTRUE(cond(r1)) && gh:::gh_has_next(r1)) {
    r1 <- gh::gh_next(r1)
    res <- merge_gh_pages(res, r1)
  }
  res
}
