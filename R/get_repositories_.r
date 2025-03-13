#' Get a list of repositories for a given user
#'
#' @param user A string providing the GitHub user name of interest.
#' @return
#' A tibble is returned with the following three columns:
#' * user: A string with the provided GitHub user name.
#' * name: A string giving the name of the repository.
#' * fork: A logical, specifying if the repository is a fork.
#'
#' @export
#' @examples
#' get_repositories_user("kellijohnson-NOAA")
#' data <- purrr::map_df(
#'   c("kellijohnson-NOAA", "msupernaw"),
#'   get_repositories_user
#' )
get_repositories_user <- function(user) {
  git_data <- gh::gh(
    endpoint = "GET /users/{user_name}/repos",
    user_name = user
  )
  git_data |>
    tibble::as_tibble_col(column_name = "data") |>
    dplyr::mutate(
      user = user,
      name = purrr::map_chr(data, "name"),
      fork = purrr::map_lgl(data, "fork")
    ) |>
    dplyr::select(-data)
}
