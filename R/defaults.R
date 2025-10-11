#' Defaults
#'

par_list <- list(
  mar = c(2.2, 2.2, 0.5, 0.5),
  cex = 1.5,
  tcl = -0.1,
  mgp = c(1.2, 0.2, 0)
)

palette <- c(
  "#832424", "#2a9d8f", "#2a9dff"
)

#' Set alpha channel of colour
#' @param colour A hex colour code
#' @param alpha A value between 0 and 1
#' @return A hex colour code with the alpha channel set
#' @examples
#' set_alpha("#832424", 0.5)
#' @export

set_alpha <- function(colour, alpha = 1) {
  stopifnot(
    is.character(colour),
    grepl("^#[0-9a-fA-F]{6,8}$", colour),
    alpha >= 0,
    alpha <= 1
  )

  colour <- substr(colour, 1, 7)
  paste0(colour, as.hexmode(floor(alpha * 255)), collapse = "")
}
