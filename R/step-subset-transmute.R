#' Create new columns, dropping old
#'
#' This is a method for the dplyr [transmute()] generic. It is translated to
#' the `j` argument of `[.data.table`.
#'
#' @param .data A [lazy_dt()].
#' @inheritParams mutate.dtplyr_step
#' @importFrom dplyr transmute
#' @export
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dt <- lazy_dt(dplyr::starwars)
#' dt %>% transmute(name, sh = paste0(species, "/", homeworld))
transmute.dtplyr_step <- function(.data, ...) {
  dots <- capture_dots(.data, ...)
  nested <- nested_vars(.data, dots, .data$vars)

  groups <- group_vars(.data)
  if (!is_empty(groups)) {
    # TODO could check if there is actually anything mutated, e.g. to avoid
    # DT[, .(x = x)]
    is_group_var <- names(dots) %in% groups
    group_dots <- dots[is_group_var]

    .data <- mutate(ungroup(.data), !!!group_dots)
    .data <- group_by(.data, !!!syms(groups))

    dots <- dots[!is_group_var]
  }

  new_vars <- mark_temp_vars(dots)

  if (is_empty(dots)) {
    # grouping variables have been removed from `dots` so `select()` would
    # produce a message "Adding grouping vars".
    # As `dplyr::transmute()` doesn't generate a message when adding group vars
    # we can also leave it away here
    return(select(.data, !!!group_vars(.data)))
  }

  if (!nested & !any(new_vars$is_temp)) {
    j <- call2(".", !!!dots)
  } else {
    j <- mutate_braces_expr(new_vars)$expr
  }
  vars <- union(group_vars(.data), names(new_vars$new_vars)[!new_vars$is_temp])
  step_subset_j(.data, vars = vars, j = j)
}

#' @export
transmute.data.table <- function(.data, ...) {
  .data <- lazy_dt(.data)
  transmute(.data, ...)
}
