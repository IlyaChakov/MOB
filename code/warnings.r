validate_iot <- function(x, Z = NULL, A = NULL) {
  x <- as.numeric(x)

  if (!is.null(Z)) {
    if (any(Z < 0)) warning("В Z есть отрицательные элементы.")
    if (any(colSums(Z) > x)) warning("По некоторым столбцам промежуточные затраты больше выпуска.")
    if (any(rowSums(Z) > x)) warning("По некоторым строкам промежуточное использование больше выпуска.")
  }

  if (!is.null(A)) {
    if (any(A < 0)) warning("В A есть отрицательные элементы.")
    test <- tryCatch(solve(diag(length(x)) - A), error = function(e) NULL)
    if (is.null(test)) warning("Матрица (I - A) необратима.")
  }

  invisible(TRUE)
}
