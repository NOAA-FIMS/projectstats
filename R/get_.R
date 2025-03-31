# To remove the WARNING
# no visible binding for global variable
utils::globalVariables(c(
  "owner", "starred_at"
))

#' Query the Github API for a list of issues associated with a repository
#'
#' The issues for a given repository and all of their metadata are returned as
#' a tibble. The table can then be used to summarize information about a given
#' repository such as how many open issues there are. Information about closed
#' issues is not returned.
#'
#' @param org The name of the organization that contains the `repo`. This can
#'   also be a user name rather than an organization. The default is
#' `"NOAA-FIMS"`.
#' @param repo The name of the GitHub repository. The default is `"FIMS"`.
#' @return
#' A 33-column tibble, that was created from the `json` list that is returned
#' from [gh::gh()], is returned. The returned tibble contains the following
#' columns:
#' * url
#' * repository_url
#' * labels_url
#' * comments_url
#' * events_url
#' * html_url
#' * id
#' * node_id
#' * number
#' * title
#' * user
#' * labels
#' * state
#' * locked
#' * assignee
#' * assignees
#' * milestone
#' * comments
#' * created_at
#' * updated_at
#' * closed_at
#' * author_association
#' * sub_issues_summary
#' * active_lock_reason
#' * draft
#' * pull_request: A five-column data frame with information about the Pull
#'   Request if the issue was a actually a Pull Request and not an issue. All
#'   entries are NULL if it was in fact an issue. Columns include `url`,
#'   `html_url`, `diff_url`, `patch_url`, and `merged_at`.
#' * body
#' * closed_by
#' * reactions
#' * timeline_url
#' * performed_via_github_app
#' * state_reason
#' @examples
#' \dontrun{
#' get_issues(repo = "FIMS", org = "NOAA-FIMS")
#' }
#' @export
get_issues <- function(org = "NOAA-FIMS", repo = "FIMS") {
  # Get the data from GitHub
  req <- gh::gh(
    endpoint = "GET /repos/{org}/{repo}/issues",
    org = org,
    repo = repo,
    .limit = Inf
  )

  # Convert to a data.frame
  jsonlite::fromJSON(jsonlite::toJSON(req)) |>
    tibble::as_tibble()
}

#' Get a list of repositories for a given user or organization
#'
#' @inheritParams get_issues
#' @param type A string specifying if the `org` is an organization or if it
#'   is a user name. The default is an organization but users can also choose
#'   `"users"`.
#' @return
#' A tibble is returned with eighty-two columns. The main columns of interest
#' are:
#' * login: A string with the provided GitHub user name or organization.
#' * name: A string giving the name of the repository.
#' * fork: A logical, specifying if the repository is a fork.
#'
#' @export
#' @examples
#' get_repositories("kellijohnson-NOAA", type = "users")
#' data <- purrr::map_df(
#'   c("kellijohnson-NOAA", "msupernaw"),
#'   get_repositories,
#'   type = "users"
#' )
#' FIMS_repos <- get_repositories("NOAA-FIMS", "orgs")
get_repositories <- function(
    org = "NOAA-FIMS",
    type = c("orgs", "users")) {
  type <- match.arg(type)
  res <- gh::gh(
    endpoint = "GET /{type}/{org}/repos",
    type = type,
    org = org,
    .limit = Inf
  )

  # Make the json a tibble by selecting the columns one wants to unnest
  # then passing those columns to reduce/unnest
  temp <- jsonlite::fromJSON(jsonlite::toJSON(res))
  if (length(temp) == 0) {
    return(NULL)
  }
  temp |>
    dplyr::select(
      # Leave all data frames nested, i.e., owner, mirror_url, license,
      # permissions, and security_and_analysis
      dplyr::where(\(x) !is.data.frame(x)),
      # Leave these list of vector or NULL nested
      -c("description", "homepage", "language", "topics")
    ) |>
    names() |>
    purrr::reduce(tidyr::unnest, .init = temp) |>
    dplyr::mutate(
      login = unlist(purrr::pluck(owner, "login"))
    )
}

#' Get the history of stars for a repository
#'
#' Get the history of when a repository was starred and return it as a tibble.
#'
#' @inheritParams get_issues
#'
#' @export
#' @return
#' A tibble with 24 columns is returned. The first twenty columns are from the
#' JSON data that is returned from the GitHub API and the next four columns are
#' added internally, specifying the GitHub organization (org), the GitHub
#' repository (repo), the date the star was added, and the days since the first
#' star was added to the repository compared when this star was added (day).
#' @examples
#' stars <- get_stargazers("NOAA-FIMS", "FIMS")
#' plot(
#'   x = stars[["day"]],
#'   y = stars[["cumulative_stars"]],
#'   type = "l",
#'   xlab = "Day since first star",
#'   ylab = "Cumulative stars"
#' )
get_stargazers <- function(org = "NOAA-FIMS", repo = "FIMS") {
  res <- gh::gh(
    endpoint = "GET /repos/{org}/{repo}/stargazers",
    org = org,
    repo = repo,
    .accept = "application/vnd.github.v3.star+json",
    .limit = Inf
  )

  # Create a temporary object so the column names can be passed
  # to purrr::reduce to unlist every column where the initial unlist
  # of the data frame stored in user leads to a data frame of lists
  temp <- tidyr::unnest(
    jsonlite::fromJSON(jsonlite::toJSON(res)),
    c("starred_at", "user")
  )
  colnames(temp) |>
    purrr::reduce(.f = tidyr::unnest, .init = temp) |>
    dplyr::mutate(
      org = org,
      repo = repo,
      date = as.Date(starred_at),
      day = as.integer(date - date[1])
    )
}

get_milestones <- function(org = "NOAA-FIMS", repo = "FIMS") {
  res <- gh::gh(
    endpoint = "GET /repos/{org}/{repo}/milestones",
    org = org,
    repo = repo,
    .accept = "application/vnd.github.v3.star+json",
    .limit = Inf
  )
  res |>
    jsonlite::toJSON() |>
    jsonlite::fromJSON() |>
    tibble::as_tibble()
}
