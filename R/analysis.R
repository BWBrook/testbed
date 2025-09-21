#' Identify missing files listed in the manifest
#'
#' @param manifest Tibble produced by `read_manifest()`.
#' @return Tibble of missing entries with columns `id`, `path`, `abs_path`.
#' @export
manifest_missing_files <- function(manifest) {
  import::from("tibble", tibble)
  import::from("rlang", abort)

  if (!all(c("id", "path", "abs_path") %in% names(manifest))) {
    abort("`manifest` must contain id, path, abs_path columns.", class = "manifest_missing_bad_cols")
  }

  missing <- manifest[!file.exists(manifest$abs_path), , drop = FALSE]
  if (nrow(missing) == 0L) {
    return(tibble::tibble())
  }
  tibble::as_tibble(missing)
}

#' Summarize numeric columns in a tibble
#'
#' @param data Tibble, typically from `combine_manifest_csvs()`.
#' @return Tibble with columns `column`, `n`, `mean`, `sd`, `min`, `max`.
#' @export
summarize_numeric_columns <- function(data) {
  import::from("dplyr", summarise, across, where, n)
  import::from("tidyr", pivot_longer)
  import::from("purrr", map_dfr)
  import::from("rlang", .data)

  if (!tibble::is_tibble(data)) {
    data <- tibble::as_tibble(data)
  }

  numeric_cols <- purrr::keep(names(data), ~ is.numeric(data[[.x]]))
  if (length(numeric_cols) == 0L) {
    return(tibble::tibble())
  }

  stats <- data |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(numeric_cols),
        list(
          n = ~ sum(!is.na(.x)),
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ stats::sd(.x, na.rm = TRUE),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE)
        ),
        .names = "{.col}.{.fn}"
      ),
      .groups = "drop"
    )

  tidyr::pivot_longer(
    stats,
    cols = dplyr::everything(),
    names_to = c("column", ".value"),
    names_sep = "\\."
  )
}

#' Write combined data to a CSV file
#'
#' @param data Tibble to write.
#' @param path Output CSV path.
#' @return Invisibly returns the path written.
#' @export
write_combined_data <- function(data, path) {
  import::from("fs", dir_create)

  dir <- dirname(path)
  fs::dir_create(dir, recurse = TRUE)
  utils::write.csv(data, file = path, row.names = FALSE)
  invisible(path)
}
