# =========================
# MAIN: межотраслевой баланс
# =========================

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_match <- grep(file_arg, args)

  if (length(file_match) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", args[file_match[1]]))))
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }

  getwd()
}

script_dir <- get_script_dir()

source(file.path(script_dir, "code", "functions.r"))
source(file.path(script_dir, "code", "table.r"))
source(file.path(script_dir, "code", "warnings.r"))
source(file.path(script_dir, "data.r"))

# -------------------------
# Что необходимо рассчитать:
#   "x_A" - расчет по заданным x и A
#   "x_Z" - расчет по заданным x и Z
#   "A_y" - расчет по заданным A и y
# -------------------------
if (!ACTIVE_CASE %in% c("x_A", "x_Z", "A_y")) {
  stop("ACTIVE_CASE должен быть одним из значений: 'x_A', 'x_Z', 'A_y'.")
}

# -------------------------
# Формирование задачи на расчет
# -------------------------
if (ACTIVE_CASE == "x_A") {
  if (is.null(x) || is.null(A)) {
    stop("Для сценария 'x_A' необходимо задать x и A.")
  }

  case <- list(
    type = "x_A",
    sectors = sectors,
    x = x,
    A = A
  )
} else if (ACTIVE_CASE == "x_Z") {
  if (is.null(x) || is.null(Z)) {
    stop("Для сценария 'x_Z' необходимо задать x и Z.")
  }

  case <- list(
    type = "x_Z",
    sectors = sectors,
    x = x,
    Z = Z
  )
} else if (ACTIVE_CASE == "A_y") {
  if (is.null(A) || is.null(y)) {
    stop("Для сценария 'A_y' необходимо задать A и y.")
  }

  case <- list(
    type = "A_y",
    sectors = sectors,
    y = y,
    A = A
  )
}

iot <- solve_iot_case(case)
validate_iot(x = iot$x, Z = iot$Z, A = iot$A)
balance_table <- make_balance_table(iot, digits = ROUND_DIGITS)

cat("Активный сценарий:", ACTIVE_CASE, "\n")
cat("Тип входных данных:", case$type, "\n\n")

cat("Выпуск x:\n")
print(round(iot$x, ROUND_DIGITS))
cat("\nКонечный спрос y:\n")
print(round(iot$y, ROUND_DIGITS))
cat("\nДобавленная стоимость v:\n")
print(round(iot$v, ROUND_DIGITS))

if (PRINT_TABLE) {
  cat("\nТаблица межотраслевого баланса:\n")
  print(balance_table)
}

if (SAVE_TABLE_TO_CSV) {
  write.csv(balance_table, OUTPUT_CSV, row.names = TRUE, fileEncoding = "UTF-8")
  cat("\nТаблица сохранена в:", OUTPUT_CSV, "\n")
}
