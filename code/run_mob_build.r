if (!exists("project_dir") || is.null(project_dir)) {
  get_current_script_dir <- function() {
    file_args <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(file_args) > 0) {
      return(dirname(normalizePath(sub("^--file=", "", file_args[1]))))
    }

    frame_files <- vapply(
      sys.frames(),
      function(env) {
        if (exists("ofile", envir = env, inherits = FALSE)) {
          get("ofile", envir = env, inherits = FALSE)
        } else {
          NA_character_
        }
      },
      character(1)
    )
    frame_files <- frame_files[!is.na(frame_files)]

    if (length(frame_files) > 0) {
      return(dirname(normalizePath(frame_files[length(frame_files)])))
    }

    getwd()
  }

  runner_dir <- get_current_script_dir()
  project_dir <- if (basename(runner_dir) == "code") dirname(runner_dir) else runner_dir
}

source(file.path(project_dir, "code", "functions.r"))
source(file.path(project_dir, "code", "table.r"))
source(file.path(project_dir, "code", "warnings.r"))

if (!exists("OUTPUT_CSV") || is.null(OUTPUT_CSV)) {
  OUTPUT_CSV <- file.path(project_dir, "output", "balance_table.csv")
}

coefficients_output_csv <- file.path(
  dirname(OUTPUT_CSV),
  paste0(tools::file_path_sans_ext(basename(OUTPUT_CSV)), "_A.csv")
)

if (!ACTIVE_CASE %in% c("x_A", "x_Z", "A_y")) {
  stop("ACTIVE_CASE должен быть одним из значений: 'x_A', 'x_Z', 'A_y'.")
}

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
} else {
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
coefficients_table <- make_coefficients_table(iot, digits = ROUND_DIGITS)

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

  if (ACTIVE_CASE == "x_Z") {
    cat("\nВосстановленная таблица коэффициентов A:\n")
  } else {
    cat("\nТаблица коэффициентов A:\n")
  }
  print(coefficients_table)
}

if (SAVE_TABLE_TO_CSV) {
  write.csv(balance_table, OUTPUT_CSV, row.names = TRUE, fileEncoding = "UTF-8")
  write.csv(coefficients_table, coefficients_output_csv, row.names = TRUE, fileEncoding = "UTF-8")
  cat("\nТаблица сохранена в:", OUTPUT_CSV, "\n")
  cat("Таблица коэффициентов сохранена в:", coefficients_output_csv, "\n")
}
