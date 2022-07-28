#' Require Packages to be Installed
#'
#' `assert_installed()` is essentially `requireNamespace(quietly = FALSE)`,
#' but it can handle multiple packages as a vector and outputs more helpful
#' error messages.
#'
#' @param pkgs `[character]` Packages to check for installation status
#'
#' @return invisible `NULL`; errors if any `pkgs` are not installed
#'
#' @export
assert_installed <- function(pkgs) {
  checkmate::assert_character(pkgs)
  not_inst <- setdiff(pkgs, installed.packages())
  n <- length(not_inst)
  not_inst <- paste0(not_inst, collapse = ", ")
  if (n > 0L) {
    stop(
      "Package", if (n > 1L) "s", "{", not_inst, "}",
      " must be installed. Please run `install.packages(",
      if (n > 1L) "c(", not_inst, if (n > 1L) ")", ")` to continue."
    )
  }
}
