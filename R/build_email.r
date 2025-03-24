#' Builds and sends the email
#'
#' This function compiles the tables with some email text and sends it
#' using `blastula::smtp_send()`.
#'
#' @param from_ sender email address
#' @param to_ recipient email address
#' @param creds A credential file of the type supported by \pkg{blastula}
#' @param body_ The text to precede the table
#' @param table_iss the issue table
#' @param table_pr The PR table
#' @param footer_ the text to follow the table
#' @seealso
#' * [blastula::creds()]
#' @return
#' A string with the email body.
#' @export
build_email <- function(
    from_,
    to_,
    creds,
    body_,
    table_iss,
    table_pr,
    footer_) {
  email <- blastula::compose_email(
    body = blastula::blocks(
      blastula::block_text(body),
      blastula::block_text(table_iss), blastula::block_text(table_pr)
    ),
    footer = blastula::md(footer_)
  )
  blastula::smtp_send(
    from = from_, to = to_, credentials = creds,
    email = email
  )
  return(email)
}
