
# Ensure package is installed
ensure <- function(package) {
  pkg <- package |> substitute() |> deparse() # allows 'package` to be passed unquoted
  if (length(find.package(pkg, quiet = TRUE)) == 0) {
    msg <- paste("The package", pkg, "is required but not found")
    if ( sys.nframe() >= 1) msg <- paste0(msg, "\nNote: ensure(", pkg, ") called from ", deparse(sys.calls()[[sys.nframe()-1]]))
    warning(msg)
  }
}
