# To remove the WARNING
# no visible binding for global variable
utils::globalVariables(c(
  "org", "repo", "day", "stars"
))

#' Calculate the cumulative number of stars
#'
#' For each combination of organization and repository, calculate the
#' cumulative number of stars from the date of the first star. Where, stars
#' represent when a GitHub user "stars" a repository.
#'
#' @param data A data frame returned from [get_stargazers()].
#' @export
#' @return
#' A tibble with the following six columns is returned:
#' * org: A string, specifying the GitHub organization.
#' * repo: A string, specifying the GitHub repository.
#' * date: A date in the format of `yyyy-mm-dd`.
#' * day: The number of days from the initial star as an integer.
#' * stars: The number of stars that were added on that day.
#' * cumulative_stars: The number of total stars since the first day the
#'   repository was starred.
calculate_cumulative_stars <- function(data) {
  data |>
    dplyr::group_by(org, repo, date, day) |>
    dplyr::count(name = "stars") |>
    dplyr::ungroup() |>
    dplyr::group_by(org, repo) |>
    dplyr::mutate(
      cumulative_stars = cumsum(stars)
    ) |>
    dplyr::ungroup()
}
