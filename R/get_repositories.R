#' Get a list of repositories for a given user or organization
#'
#' @param user A string providing the GitHub user name or organization name of
#'   interest.
#' @return
#' A tibble is returned with the following three columns:
#' * user: A string with the provided GitHub user name or organization.
#' * name: A string giving the name of the repository.
#' * fork: A logical, specifying if the repository is a fork.
#'
#' @export
#' @examples
#' get_repositories("kellijohnson-NOAA")
#' data <- purrr::map_df(
#'   c("kellijohnson-NOAA", "msupernaw"),
#'   get_repositories
#' )
#' FIMS_repos <- get_repositories("NOAA-FIMS", "orgs")
get_repositories <- function(x, type = c("users", "orgs")) {
  type <- match.arg(type)
  git_data <- gh::gh(
    endpoint = "GET /{type}/{x}/repos",
    type = type,
    x = x
  )
  git_data |>
    tibble::as_tibble_col(column_name = "data") |>
    dplyr::mutate(
      user = x,
      name = purrr::map_chr(data, "name"),
      fork = purrr::map_lgl(data, "fork")
    ) |>
    dplyr::select(-data)
}
