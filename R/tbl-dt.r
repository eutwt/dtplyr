#' Create a data table tbl.
#'
#' A data table tbl wraps a local data table.
#'
#' @export
#' @param data a data table
#' @param copy If the input is a data.table, copy it?
#' @aliases .datatable.aware
#' @examples
#' ds <- tbl_dt(mtcars)
#' ds
#' data.table::as.data.table(ds)
#'
#' library(dplyr, warn.conflicts = FALSE)
#' if (require("nycflights13")) {
#' flights2 <- tbl_dt(flights)
#' flights2 %>% filter(month == 1, day == 1, dest == "DFW")
#' flights2 %>% select(year:day)
#' flights2 %>% rename(Year = year)
#' flights2 %>%
#'   summarise(
#'     delay = mean(arr_delay, na.rm = TRUE),
#'     n = length(arr_delay)
#'   )
#' flights2 %>%
#'   mutate(gained = arr_delay - dep_delay) %>%
#'   select(ends_with("delay"), gained)
#' flights2 %>%
#'   arrange(dest, desc(arr_delay))
#'
#' by_dest <- group_by(flights2, dest)
#'
#' filter(by_dest, arr_delay == max(arr_delay, na.rm = TRUE))
#' summarise(by_dest, arr = mean(arr_delay, na.rm = TRUE))
#'
#' # Normalise arrival and departure delays by airport
#' by_dest %>%
#'   mutate(arr_z = scale(arr_delay), dep_z = scale(dep_delay)) %>%
#'   select(starts_with("arr"), starts_with("dep"))
#'
#' arrange(by_dest, desc(arr_delay))
#' select(by_dest, -(day:tailnum))
#' rename(by_dest, Year = year)
#'
#' # All manip functions preserve grouping structure, except for summarise
#' # which removes a grouping level
#' by_day <- group_by(flights2, year, month, day)
#' by_month <- summarise(by_day, delayed = sum(arr_delay > 0, na.rm = TRUE))
#' by_month
#' summarise(by_month, delayed = sum(delayed))
#'
#' # You can also manually ungroup:
#' ungroup(by_day)
#' }
tbl_dt <- function(data, copy = TRUE) {
  if (is.grouped_dt(data)) {
    return(ungroup(data))
  }

  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  } else if (copy) {
    data <- data.table::copy(data)
  }

  data.table::setattr(data, "class", c("tbl_dt", "tbl", "data.table", "data.frame"))
  data
}

#' @export
#' @importFrom dplyr as.tbl
as.tbl.data.table <- function(x, ...) {
  tbl_dt(x)
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.tbl_dt <- function(x) data.table::copy(names(x))

#' @importFrom dplyr groups
#' @export
groups.tbl_dt <- function(x) {
  NULL
}

#' @importFrom dplyr ungroup
#' @export
ungroup.tbl_dt <- function(x, ...) x

#' @export
ungroup.data.table <- function(x, ...) x

#' @importFrom dplyr same_src
#' @export
same_src.tbl_dt <- function(x, y) {
  data.table::is.data.table(y)
}

#' @importFrom dplyr auto_copy
#' @export
auto_copy.tbl_dt <- function(x, y, copy = FALSE, ...) {
  data.table::as.data.table(as.data.frame(y))
}

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_dt <- function(x, row.names = NULL, optional = FALSE, ...) {
  NextMethod()
}

#' @export
print.tbl_dt <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: local data table ", dplyr::dim_desc(x), "\n", sep = "")
  cat("\n")
  print(dplyr::trunc_mat(x, n = n, width = width))

  invisible(x)
}

#' @export
dimnames.tbl_dt <- function(x) data.table::copy(NextMethod())

#' @importFrom utils head
#' @export
head.tbl_dt <- function(x, ...) tbl_dt(NextMethod())

#' @importFrom utils tail
#' @export
tail.tbl_dt <- function(x, ...) tbl_dt(NextMethod())

#' @export
#' @method all.equal tbl_dt
all.equal.tbl_dt <- function(target, current, ignore_col_order = TRUE,
                             ignore_row_order = TRUE, convert = FALSE, ...) {
  dplyr::all_equal(target, current, ignore_col_order = ignore_col_order,
    ignore_row_order = ignore_row_order, convert = convert, ...)
}

#' @export
.datatable.aware <- TRUE

# Filter -----------------------------------------------------------------------

and_expr <- function(exprs) {
  stopifnot(is.list(exprs))
  if (length(exprs) == 0) {
    return(TRUE)
  }
  if (length(exprs) == 1) {
    return(exprs[[1]])
  }

  left <- exprs[[1]]
  for (i in 2:length(exprs)) {
    left <- substitute(left & right, list(left = left, right = exprs[[i]]))
  }
  left
}

# The S3 method is registered manually in .onLoad() to avoid an R CMD
# check warning

# Is there something similar in rlang?

# first version, adapted from lazyeval
# common_env <- function (dots){
#   if (length(dots) == 0)
#       return(baseenv())
#   env <- get_env(dots[[1]])
#   if (length(dots) == 1)
#       return(env)
#   for (i in 2:length(dots)) {
#       if (!identical(env, get_env(dots[[i]]))) {
#           return(baseenv())
#       }
#   }
#   env
# }

# but this still does not take care of:

# a) Literals. We want to fall back to baseenv() if we dont have a unique env
# that is not emptyenv().

# b) Nested environments. If a function is called within a function, we want to
# evaluate the data.table call in the same environments. Some quosures may have
# an environment that is a parent of this environment, so we need to figure
# out the 'deepest' environment. We also need to check that all other
# environments are parents of this environment.

# third version
common_env <- function (dots){
  stopifnot(inherits(dots, "quosures"))
  if (length(dots) == 0) return(baseenv())
  if (length(dots) == 1){
    env <- get_env(dots[[1]])
  } else {
    # evaluate common env
    envs <- lapply(dots, get_env)

    # use deepest env as a candidate for common env
    env <- envs[[which.max(vapply(envs, env_depth, 0L))]]
    # fail if some environments are not in search path
    if (!all(envs %in% c(env_parents(env), env))){
      stop("Conflicting environment tree. Does this ever arise?")
    }
  }

  # from ?quosure:
  #   Literals are enquosed with the empty environment because they can
  #   be evaluated anywhere.
  # But we don't have `[` in emptyenv, which we need in the data.table call.
  # Changing to baseenv which was also returned in lazyeval::all_dots()
  if (identical(env, emptyenv())){
    env <- baseenv()
  }
  env
}

#' @importFrom dplyr filter
filter.grouped_dt <- function(.data, ...) {
  grouped_dt(NextMethod(), groups(.data), copy = FALSE)
}
filter.tbl_dt <- function(.data, ...) {
  tbl_dt(NextMethod(), copy = FALSE)
}
filter.data.table <- function(.data, ...) {
  dots <- quos(...)
  env <- common_env(dots)

  # http://stackoverflow.com/questions/16573995/subset-by-group-with-data-table
  # expr <- lapply(dots, `[[`, "expr")
  expr <- lapply(dots, get_expr)
  j <- substitute(list(`_row` = .I[expr]), list(expr = and_expr(expr)))
  indices <- dt_subset(.data, , j, env)$`_row`

  .data[indices[!is.na(indices)]]
}

# Summarise --------------------------------------------------------------------

#' @importFrom dplyr summarise
summarise.grouped_dt <- function(.data, ...) {
  grouped_dt(NextMethod(), drop_last(groups(.data)), copy = FALSE)
}
summarise.tbl_dt <- function(.data, ...) {
  tbl_dt(NextMethod(), copy = FALSE)
}
summarise.data.table <- function(.data, ...) {
  dots <- quos(..., .named = TRUE)

  env <- common_env(dots)
  exprs <- lapply(dots, get_expr)

  j <- as.call(c(quote(list), exprs))

  dt_subset(.data, , j, env = env)
}

# Mutate -----------------------------------------------------------------------

mutate.grouped_dt <- function(.data, ...) {
  grouped_dt(NextMethod(), groups(.data), copy = FALSE)
}
mutate.tbl_dt <- function(.data, ...) {
  tbl_dt(NextMethod(), copy = FALSE)
}

#' @importFrom dplyr mutate
mutate.data.table <- function(.data, ...) {
  dots <- quos(..., .named = TRUE)
  names <- names(dots)

  # Never want to modify in place
  .data <- data.table::copy(.data)

  for(i in seq_along(dots)) {
    # For each new variable, generate a call of the form df[, new := expr]
    j <- substitute(lhs := rhs, list(lhs = names[[i]], rhs = get_expr(dots[[i]])))

    env <- common_env(dots[i])

    .data <- dt_subset(.data, , j,  env)
  }

  # Need to use this syntax to make the output visible (#11).
  .data[]
}

# Arrange ----------------------------------------------------------------------

#' @importFrom dplyr arrange
arrange.grouped_dt <- function(.data, ..., .by_group = FALSE) {
  if (.by_group) {
    dots <- quos(!!!groups(.data), ...)
  } else {
    dots <- quos(...)
  }

  arrange_impl(.data, dots)
}
arrange.tbl_dt <- function(.data, ...) {
  tbl_dt(NextMethod(), copy = FALSE)
}
arrange.data.table <- function(.data, ...) {
  dots <- quos(...)

  arrange_impl(.data, dots)
}

arrange_impl <- function(.data, dots) {
  exprs <- lapply(dots, get_expr)
  env <- common_env(dots)

  i <- as.call(c(quote(order), exprs))

  dt_subset(.data, i, , env = env)
}

# Select -----------------------------------------------------------------------

# not exported from dplyr
ensure_group_vars <- function(vars, data, notify = TRUE) {
  group_names <- group_vars(data)
  missing <- setdiff(group_names, vars)

  if (length(missing) > 0) {
    if (notify) {
      inform(paste0("Adding missing grouping variables: ",
                    "`", missing, "`", collapse = ", ")
             )
    }
    vars <- c(set_names(missing, missing), vars)
  }

  vars
}


#' @importFrom dplyr select
#' @importFrom tidyselect vars_select vars_rename
select.grouped_dt <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! quos(...))
  vars <- ensure_group_vars(vars, .data)

  out <- .data[, vars, drop = FALSE, with = FALSE]

  if (nrow(out) > 0) data.table::setnames(out, names(vars))

  grouped_dt(out, groups(.data), copy = FALSE)

}
select.data.table <- function(.data, ...) {
  vars <- tidyselect::vars_select(names(.data), !!! quos(...))

  out <- .data[, vars, drop = FALSE, with = FALSE]

  if (nrow(out) > 0) data.table::setnames(out, names(vars))

  out
}
select.tbl_dt <- function(.data, ...) {
  tbl_dt(NextMethod(), copy = FALSE)
}

# Rename -----------------------------------------------------------------------

#' @importFrom dplyr rename
rename.grouped_dt <- function(.data, ...) {
  vars <- tidyselect::vars_rename(names(.data), !!! quos(...))

  out <- .data[, vars, drop = FALSE, with = FALSE]

  if (nrow(out) > 0) data.table::setnames(out, names(vars))

  grouped_dt(out, groups(.data), copy = FALSE)
}
rename.data.table <- function(.data, ...) {
  vars <- tidyselect::vars_rename(names(.data), !!! quos(...))

  out <- .data[, vars, drop = FALSE, with = FALSE]

  if (nrow(out) > 0) data.table::setnames(out, names(vars))

  out
}
rename.tbl_dt <- function(.data, ...) {
  tbl_dt(NextMethod(), copy = FALSE)
}


# Slice ------------------------------------------------------------------------

#' @importFrom dplyr slice
slice.grouped_dt <- function(.data, ...) {
  grouped_dt(NextMethod(), groups(.data), copy = FALSE)
}
slice.tbl_dt <- function(.data, ...) {
  tbl_dt(NextMethod(), copy = FALSE)
}
slice.data.table <- function(.data, ...) {

  dots <- quos(...)
  env <- common_env(dots)

  exprs <- lapply(dots, get_expr)

  i <- as.call(c(quote(c), exprs))

  j <- substitute(.SD[rows], list(rows = i))
  dt_subset(.data, , j, env)
}
