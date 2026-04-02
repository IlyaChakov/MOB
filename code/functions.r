# =========================
# МЕЖОТРАСЛЕВОЙ БАЛАНС (R)
# =========================

# Проверка имен отраслей
.check_names <- function(sectors, n) {
  if (is.null(sectors)) {
    sectors <- paste0("Отрасль_", seq_len(n))
  }
  if (length(sectors) != n) {
    stop("Длина вектора sectors должна совпадать с размерностью.")
  }
  sectors
}

# -------------------------
# СЛУЧАЙ A: заданы x и A
# x = выпуск по отраслям
# A = матрица прямых затрат
# -------------------------
build_iot_from_A <- function(x, A, sectors = NULL, tol = 1e-8) {
  x <- as.numeric(x)
  n <- length(x)

  if (!is.matrix(A)) A <- as.matrix(A)
  if (!all(dim(A) == c(n, n))) {
    stop("Матрица A должна иметь размер n x n, где n = length(x).")
  }

  sectors <- .check_names(sectors, n)

  # Матрица промежуточного потребления:
  # z_ij = a_ij * x_j
  Z <- sweep(A, 2, x, `*`)

  # Промежуточное потребление по строкам (сколько продукции отрасли i
  # потреблено всеми отраслями)
  intermediate_sales <- rowSums(Z)

  # Промежуточные затраты по столбцам (затраты отрасли j на промежуточные продукты)
  intermediate_inputs <- colSums(Z)

  # Конечный спрос: y = x - Ax = x - rowSums(Z)
  y <- x - intermediate_sales

  # Добавленная стоимость по столбцам:
  # v_j = x_j - sum_i z_ij
  v <- x - intermediate_inputs

  # Проверка баланса
  # По строкам: x_i = sum_j z_ij + y_i
  row_balance <- x - (intermediate_sales + y)

  # По столбцам: x_j = sum_i z_ij + v_j
  col_balance <- x - (intermediate_inputs + v)

  if (max(abs(row_balance)) > tol || max(abs(col_balance)) > tol) {
    warning("Баланс не выполняется в пределах tol.")
  }

  rownames(Z) <- sectors
  colnames(Z) <- sectors
  names(x) <- sectors
  names(y) <- sectors
  names(v) <- sectors
  names(intermediate_sales) <- sectors
  names(intermediate_inputs) <- sectors

  list(
    sectors = sectors,
    x = x,
    A = A,
    Z = Z,
    y = y,
    v = v,
    intermediate_sales = intermediate_sales,
    intermediate_inputs = intermediate_inputs,
    row_balance = row_balance,
    col_balance = col_balance
  )
}

# -------------------------
# СЛУЧАЙ B: заданы x и Z
# восстанавливаем A, y, v
# -------------------------
build_iot_from_Z <- function(x, Z, sectors = NULL, tol = 1e-8) {
  x <- as.numeric(x)
  n <- length(x)

  if (!is.matrix(Z)) Z <- as.matrix(Z)
  if (!all(dim(Z) == c(n, n))) {
    stop("Матрица Z должна иметь размер n x n, где n = length(x).")
  }

  sectors <- .check_names(sectors, n)

  # A = Z %*% diag(1/x) по столбцам
  A <- sweep(Z, 2, x, `/`)

  intermediate_sales <- rowSums(Z)
  intermediate_inputs <- colSums(Z)

  y <- x - intermediate_sales
  v <- x - intermediate_inputs

  row_balance <- x - (intermediate_sales + y)
  col_balance <- x - (intermediate_inputs + v)

  if (max(abs(row_balance)) > tol || max(abs(col_balance)) > tol) {
    warning("Баланс не выполняется в пределах tol.")
  }

  rownames(Z) <- sectors
  colnames(Z) <- sectors
  rownames(A) <- sectors
  colnames(A) <- sectors
  names(x) <- sectors
  names(y) <- sectors
  names(v) <- sectors
  names(intermediate_sales) <- sectors
  names(intermediate_inputs) <- sectors

  list(
    sectors = sectors,
    x = x,
    A = A,
    Z = Z,
    y = y,
    v = v,
    intermediate_sales = intermediate_sales,
    intermediate_inputs = intermediate_inputs,
    row_balance = row_balance,
    col_balance = col_balance
  )
}

# -------------------------
# Матрица полных затрат
# L = (I - A)^(-1)
# -------------------------
leontief_inverse <- function(A) {
  A <- as.matrix(A)
  n <- nrow(A)
  if (n != ncol(A)) stop("A должна быть квадратной матрицей.")
  solve(diag(n) - A)
}

# -------------------------
# По заданному конечному спросу
# получаем выпуск:
# x = (I - A)^(-1) y
# -------------------------
output_from_final_demand <- function(A, y) {
  L <- leontief_inverse(A)
  as.vector(L %*% y)
}

# -------------------------
# СЛУЧАЙ C: заданы A и y
# восстанавливаем x, Z, v
# -------------------------
build_iot_from_Ay <- function(A, y, sectors = NULL, tol = 1e-8) {
  y <- as.numeric(y)

  if (!is.matrix(A)) A <- as.matrix(A)
  n <- length(y)

  if (!all(dim(A) == c(n, n))) {
    stop("Матрица A должна иметь размер n x n, где n = length(y).")
  }

  x <- output_from_final_demand(A, y)
  build_iot_from_A(x = x, A = A, sectors = sectors, tol = tol)
}

# -------------------------
# Универсальный диспетчер
# case$type:
#   "x_A" - заданы x и A
#   "x_Z" - заданы x и Z
#   "A_y" - заданы A и y
# -------------------------
solve_iot_case <- function(case, tol = 1e-8) {
  if (!is.list(case) || is.null(case$type)) {
    stop("case должен быть списком с полем type.")
  }

  sectors <- case$sectors

  switch(
    case$type,
    "x_A" = build_iot_from_A(
      x = case$x,
      A = case$A,
      sectors = sectors,
      tol = tol
    ),
    "x_Z" = build_iot_from_Z(
      x = case$x,
      Z = case$Z,
      sectors = sectors,
      tol = tol
    ),
    "A_y" = build_iot_from_Ay(
      A = case$A,
      y = case$y,
      sectors = sectors,
      tol = tol
    ),
    stop("Неизвестный тип сценария. Используйте: 'x_A', 'x_Z', 'A_y'.")
  )
}
