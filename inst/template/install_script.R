## We need a CRAN mirror set or nothing works. The user is free to
## replace this with something else if they want within their script,
## but this saves every script needing one.
options(repos = {{repos}})

message("Logs from your installation script '{{script}}' follow:")
message()
message(strrep("-", 79))
message()
source("{{script}}", echo = TRUE, max.deparse.length = Inf)
message()
message(strrep("-", 79))
