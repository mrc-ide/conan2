`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


glue_whisker <- function(template, data) {
  transformer <- function(...) {
    ## This transformer prevents a NULL entry destroying the string
    glue::identity_transformer(...) %||% ""
  }
  glue::glue(template, .envir = data, .open = "{{", .close = "}}",
             .trim = FALSE, .transformer = transformer)
}


conan_file <- function(path) {
  system.file(path, package = "conan2", mustWork = TRUE)
}


read_string <- function(path) {
  paste(readLines(path), collapse = "\n")
}


squote <- function(x) {
  sprintf("'%s'", x)
}


vector_to_str <- function(x) {
  str <- paste(sprintf('"%s"', x), collapse = ", ")
  if (length(x) > 1) {
    str <- sprintf("c(%s)", str)
  }
  str
}


collapseq <- function(x, last = NULL) {
  paste(squote(x), collapse = ", ")
}


dir_create <- function(path) {
  dir.create(path, FALSE, TRUE)
}


vcapply <- function(...) {
  vapply(..., FUN.VALUE = "")
}


vlapply <- function(...) {
  vapply(..., FUN.VALUE = TRUE)
}


deparse_fn <- function(nm, indent = 0) {
  value <- trimws(deparse(get(nm)), "right")
  value[[1]] <- sprintf("%s <- %s", nm, value[[1]])
  paste0(strrep(" ", indent), value, collapse = "\n")
}
