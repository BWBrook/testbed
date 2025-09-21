# Helper setup for tests
testthat::local_edition(3)
set.seed(1)

r_dir <- normalizePath(file.path("..", "..", "R"), mustWork = TRUE)
pkg_env <- parent.env(environment())
r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_files, sys.source, envir = pkg_env))
