check_argument <- function(x, var_type, var_length = 1) {
  if (!typeof(x) %in% var_type && length(x) == var_length) stop(
    sprintf("Invalid %s argument which should be %s of length %s",
            deparse(substitute(x)),
            paste(var_type, collapse = " or "),
            var_length),
    call. = FALSE
    )
  x
}

