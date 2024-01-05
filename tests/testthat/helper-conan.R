bootstrap_library <- function(pkg) {
  if (length(pkg) == 0) {
    .libPaths()[[1]]
  } else {
    dirname(find.package(pkg))
  }
}


example_installations <- function(path, n = NULL) {
  fs::dir_create(file.path(path, ".conan"))
  d <- readRDS("examples/installations.rds")
  n <- n %||% length(d)
  nms <- names(d)[seq_len(n)]
  for (nm in nms) {
    saveRDS(d[[nm]], file.path(path, ".conan", nm))
  }
  nms
}
