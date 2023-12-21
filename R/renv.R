using_renv <- function(path = getwd()) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    return(FALSE)
  }
  project <- renv::project(default = NULL)
  if (is.null(project)) {
    return(FALSE)
  }
  fs::path_has_parent(path, project)
}
