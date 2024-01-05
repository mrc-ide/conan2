##' List installations that have been carried out by conan into a
##' library
##'
##' @title List conan installations
##'
##' @param path_lib The library that conan has installed into
##'
##' @return Invisibly, a character vector of names
##'
##' @export
conan_list_installations <- function(path_lib) {
  contents <- sort(dir(file.path(path_lib, ".conan")))
  if (length(contents) == 0) {
    cli::cli_alert_warning("No conan installations recorded")
  } else {
    cli::cli_alert_info("{length(contents)} conan installation{?s} recorded")
    ago <- vcapply(contents, function(x) {
      time <- readRDS(file.path(path_lib, ".conan", x))$description$time
      prettyunits::time_ago(time)
    })
    i <- seq_along(contents)
    j <- i - length(contents)
    txt <- sprintf("%d: {.strong %s} (%s) [%d]", i, contents, ago, j)
    cli::cli_li(txt)
  }
  invisible(contents)
}


##' Compare conan installations.
##'
##' @title Compare conan installations
##'
##' @inheritParams conan_list_installations
##'
##' @param curr The previous installation to compare against. Can be a
##'   name (see [conan_list_installations] to get names), a negative
##'   number where `-n` indicates "`n` installations ago" or a
##'   positive number where `n` indicates "the `n`th
##'   installation". The default value of 0 corresponds to the current
##'   installation.
##'
##' @param prev The previous installation to compare against. Can be a
##'   name (see [conan_list_installations] to get names), a negative
##'   number where `-n` indicates "`n` installations ago" or a
##'   positive number where `n` indicates "the `n`th installation".
##'   The default of -1 indicates the previous installation. Must
##'   refer to an installation before `curr`. Use `NULL` or -Inf` if
##'   you want to compare against the empty installation.
##'
##' @return An object of class `conan_compare`, which can be printed
##'   nicely.
##'
##' @export
conan_compare <- function(path_lib, curr = 0, prev = -1) {
  contents <- sort(dir(file.path(path_lib, ".conan")))
  if (length(contents) == 0) {
    cli::cli_abort(
      "No conan installations found at '{path_lib}'")
  }
  i_curr <- select_compare(curr, contents, "curr", rlang::current_env())
  i_prev <- select_compare(prev %||% -Inf, contents, "prev",
                           rlang::current_env())
  if (i_curr < i_prev) {
    cli::cli_abort(
      "'curr' (installation {i_curr}) is earlier than 'prev' ({i_prev})")
  }
  if (i_curr < i_prev) {
    cli::cli_abort("'curr' (installation {i_curr}) is the same as 'prev'")
  }

  info_curr <- readRDS(file.path(path_lib, ".conan", contents[[i_curr]]))
  curr <- list(name = contents[[i_curr]],
               index = i_curr,
               age = i_curr - length(contents),
               time = info_curr$description$time,
               method = info_curr$method,
               packages = as.data.frame(info_curr$description))

  if (i_prev == 0) {
    prev <- NULL
    status <- set_names(rep("added", nrow(curr$packages)),
                        curr$packages$package)
  } else {
    info_prev <- readRDS(file.path(path_lib, ".conan", contents[[i_prev]]))
    prev <- list(name = contents[[i_prev]],
                 index = i_prev,
                 age = i_prev - length(contents),
                 time = info_prev$description$time,
                 method = info_prev$method,
                 packages = as.data.frame(info_prev$description))

    nms <- sort(union(curr$packages$package, prev$packages$package))
    status <- set_names(rep("updated", length(nms)), nms)
    status[setdiff(curr$packages$package, prev$packages$package)] <- "added"
    status[setdiff(prev$packages$package, curr$packages$package)] <- "removed"
    is_unchanged <- (apply(prev$packages, 1, paste, collapse = "\r") %in%
                     apply(curr$packages, 1, paste, collapse = "\r"))
    status[prev$packages$package[is_unchanged]] <- "unchanged"
  }

  ret <- list(curr = curr, prev = prev, status = status)
  class(ret) <- "conan_compare"
  ret
}


select_compare <- function(x, contents, name, call) {
  assert_scalar(x, name = name, call = call)
  if (is.character(x)) {
    i <- match(x, contents)
    if (is.na(i)) {
      cli::cli_abort(
        c("Invalid entry '{x}' for '{name}",
          i = "Valid entries include: {squote(utils::tail(contents))}"),
        call = call, arg = name)
    }

  } else if (is.numeric(x)) {
    if (x <= 0) {
      i <- max(0, length(contents) + x)
    } else {
      if (i > length(contents)) {
        cli::cli_abort(
          c("Invalid entry '{x}' for '{name}",
            i = "Maximum allowed value is: {length(contents)}"),
          call = call, arg = name)
      }
      i <- x
    }
  }
  i
}


##' @export
print.conan_compare <- function(x, ..., show_unchanged = FALSE) {
  ## Unfortunately, this theme does not always seem to activate, and I
  ## don't understand what the difference is between when it does and
  ## when it does not. However, it's only meant to emphasise things,
  ## and the text should be interpretable without any emphasis.
  cli::cli_div(
    theme = list(
      span.old = list(color = "grey", "font-style" = "italic"),
      span.new = list(color = "hotpink", "font-weight" = "bold"),
      h2 = list("margin-top" = 1, "margin-bottom" = 0),
      h1 = list("margin-top" = 0, "margin-bottom" = 0)))
  print_conan_compare_header(x$prev, x$curr)

  format_source <- function(source, details) {
    if (is.na(details)) {
      src <- source
    } else {
      src <- sprintf("%s: %s", source, details)
    }
  }

  is_unchanged <- x$status == "unchanged"
  if (any(is_unchanged)) {
    cli::cli_h2("{sum(is_unchanged)} unchanged package{?s}")
    if (show_unchanged) {
      i <- x$curr$packages$package %in% names(x$status)[is_unchanged]
      for (j in which(i)) {
        p <- as.list(x$curr$packages[j, ])
        src <- format_source(p$source, p$details)
        cli::cli_li("{.strong {p$package}} ({p$version}) {src}")
      }
    } else {
      cli::cli_alert_info(
        "To show unchanged packages, print with 'show_changed = TRUE'")
    }
  }

  is_added <- x$status == "added"
  if (any(is_added)) {
    cli::cli_h2("{sum(is_added)} added package{?s}")
    i <- x$curr$packages$package %in% names(x$status)[is_added]
    for (j in which(i)) {
      p <- as.list(x$curr$packages[j, ])
      src <- format_source(p$source, p$details)
      cli::cli_li("{.strong {p$package}} ({.new {p$version}}) {src}")
    }
  }

  is_removed <- x$status == "removed"
  if (any(is_removed)) {
    cli::cli_h2("{sum(is_removed)} removed package{?s}")
    i <- x$prev$packages$package %in% names(x$status)[is_removed]
    for (j in which(i)) {
      p <- as.list(x$prev$packages[j, ])
      src <- format_source(p$source, p$details)
      cli::cli_li("{.strong {p$package}} ({.old {p$version}}) {.old {src}}")
    }
  }

  is_updated <- x$status == "updated"
  if (any(is_updated)) {
    cli::cli_h2("{sum(is_updated)} updated package{?s}")
    for (nm in names(x$status)[is_updated]) {
      p_prev <- as.list(x$prev$packages[x$prev$packages$package == nm, ])
      p_curr <- as.list(x$curr$packages[x$curr$packages$package == nm, ])
      notes <- NULL
      if (p_prev$version == p_curr$version) {
        version <- p_curr$version
        notes <- c(notes, c("!" = "Version number unchanged between updates"))
      } else {
        version <- "{.old {p_prev$version}} -> {.new {p_curr$version}}"
      }

      if (p_prev$source != p_curr$source) {
        src <- sprintf("{.old %s} -> {.new %s}",
                       format_source(p_prev$source, p_prev$details),
                       format_source(p_curr$source, p_curr$details))
      } else if (identical(p_prev$details, p_curr$details)) {
        notes <- c(
          notes,
          c("!" = "Different reported build times, but details unchanged?"))
        src <- format_source(p_curr$source, p_curr$details)
      } else {
        details <- details_changes(p_prev$details, p_curr$details)
        src <- format_source(p_curr$source, details)
      }
      cli::cli_li(sprintf("{.strong {p_curr$package}} (%s) %s",
                          version, src))
      if (!is.null(notes)) {
        cli::bullets(notes)
      }
    }
  }

  invisible(x)
}


print_conan_compare_header <- function(prev, curr) {
  ord <- function(n) {
    if (n == 1 || n > 20 && n %% 10 == 1) {
      suffix <- "st"
    } else if (n == 2 || n > 20 && n %% 10 == 2) {
      suffix <- "nd"
    } else if (n == 3 || n > 20 && n %% 10 == 3) {
      suffix <- "rd"
    } else {
      suffix <- "th"
    }
    paste0(n, suffix)
  }

  age <- function(n) {
    if (n == 0) {
      "current installation"
    } else if (n == -1) {
      "previous installation"
    } else {
      paste(-n, "installations ago")
    }
  }

  cli::cli_h1("Comparing conan installations")
  if (is.null(prev)) {
    cli::cli_li("({.old empty installation})")
  } else {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_li(paste(
      "{.strong {prev$name}} {.old {ord(prev$index)}; {age(prev$age)}",
      "({prettyunits::time_ago(prev$time)})}"))
  }
  cli::cli_li(paste(
    "{.strong {curr$name}} {.new {ord(curr$index)}; {age(curr$age)}",
    "({prettyunits::time_ago(curr$time)})}"))
}


details_changes <- function(prev, curr) {
  parts <- strsplit(c(prev, curr), "(?=[^[:alnum:]])", perl = TRUE)
  for (i in seq_len(min(lengths(parts)))) {
    if (parts[[1]][[i]] != parts[[2]][[i]]) {
      break
    }
  }
  if (i == 1) {
    sprintf("{.old %s} -> {.new %s}", prev, curr)
  } else {
    shared <- paste(parts[[1]][seq_len(i - 1)], collapse = "")
    prev <- paste(utils::tail(parts[[1]], -i + 1), collapse = "")
    curr <- paste(utils::tail(parts[[2]], -i + 1), collapse = "")
    sprintf("%s{{{.old %s} -> {.new %s}}}", shared, prev, curr)
  }
}