% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_.R
\name{get_repositories}
\alias{get_repositories}
\title{Get a list of repositories for a given user or organization}
\usage{
get_repositories(org = "NOAA-FIMS", type = c("orgs", "users"))
}
\arguments{
\item{org}{The name of the organization that contains the \code{repo}. This can
also be a user name rather than an organization. The default is
\code{"NOAA-FIMS"}.}

\item{type}{A string specifying if the \code{org} is an organization or if it
is a user name. The default is an organization but users can also choose
\code{"users"}.}
}
\value{
A tibble is returned with eighty-two columns. The main columns of interest
are:
\itemize{
\item login: A string with the provided GitHub user name or organization.
\item name: A string giving the name of the repository.
\item fork: A logical, specifying if the repository is a fork.
}
}
\description{
Get a list of repositories for a given user or organization
}
\examples{
get_repositories("kellijohnson-NOAA", type = "users")
data <- purrr::map_df(
  c("kellijohnson-NOAA", "msupernaw"),
  get_repositories,
  type = "users"
)
FIMS_repos <- get_repositories("NOAA-FIMS", "orgs")
}
