% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run.R
\name{conan_run}
\alias{conan_run}
\title{Run a conan installation}
\usage{
conan_run(config, show_log = TRUE)
}
\arguments{
\item{config}{Conan config, from \code{\link[=conan_configure]{conan_configure()}}}

\item{show_log}{Show output from running the script (passed
through to \link[callr:rscript]{callr::rscript} as \code{show})}
}
\value{
Nothing
}
\description{
Run a conan installation, in another process, blocking from this
process.
}
