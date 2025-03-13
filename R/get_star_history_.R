#' Get the history of stars for a repository
#'
#' Get the history of when a repository was starred and return it as a tibble.
#' This information is useful for plotting.
#'
#' @param org A string providing the name of the GitHub organization that the
#'   `repo` is located in.
#' @param repo A string providing the name of the GitHub repository that you
#'   are interested in getting information for.
#'
#' @export
#' @return
#' A tibble with the following four columns is returned:
#' * date: A date in the format of `yyyy-mm-dd`.
#' * day: The number of days from the initial star as an integer.
#' * stars: The number of stars that were added on that day.
#' * cumulative_stars: The number of total stars since the first day the
#'   repository was starred.
#' @examples
#' stars <- get_star_history_repo("NOAA-FIMS", "FIMS")
#' plot(
#'   x = stars[["day"]],
#'   y = stars[["cumulative_stars"]],
#'   type = "l",
#'   xlab = "Day since first star",
#'   ylab = "Cumulative stars"
#' )
get_star_history_repo <- function(org, repo) {
  stars <- gh::gh(
    endpoint = "GET /repos/{org}/{repo}/stargazers",
    org = org,
    repo = repo,
    .accept = "application/vnd.github.v3.star+json",
    .limit = Inf
  )
  stars |>
    tibble::as_tibble_col(column_name = "data") |>
    dplyr::mutate(
      date_time = purrr::map_vec(data, purrr::pluck, "starred_at"),
      date = as.Date(date_time),
      day = as.integer(date - date[1])
    ) |>
    dplyr::group_by(date, day) |>
    dplyr::count(name = "stars") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      cumulative_stars = cumsum(stars)
    )
}
