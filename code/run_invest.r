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

RUN_MOB_BUILD <- FALSE
source(file.path(project_dir, "scripts", "mob_build.r"))

base_case <- prepare_iot_case(
  ACTIVE_CASE = ACTIVE_CASE,
  sectors = sectors,
  x = x,
  A = A,
  Z = Z,
  y = y
)

base_iot <- solve_iot_case(base_case)
validate_iot(x = base_iot$x, Z = base_iot$Z, A = base_iot$A)

impact <- investment_impact(
  iot = base_iot,
  sector = INVEST_SECTOR,
  investment = ADDITIONAL_INVESTMENT
)

investment_table <- make_investment_table(base_iot, impact, digits = ROUND_DIGITS)
stimulated_balance_table <- make_balance_table(impact$stimulated_iot, digits = ROUND_DIGITS)

if (!exists("INVEST_OUTPUT_CSV") || is.null(INVEST_OUTPUT_CSV)) {
  INVEST_OUTPUT_CSV <- file.path(project_dir, "output", "investment_impact.csv")
}

if (!exists("STIMULATED_OUTPUT_CSV") || is.null(STIMULATED_OUTPUT_CSV)) {
  STIMULATED_OUTPUT_CSV <- file.path(project_dir, "output", "stimulated_balance_table.csv")
}

cat("Сценарий базового МОБ:", ACTIVE_CASE, "\n")
cat("Стимулируемая отрасль:", impact$sector_name, "\n")
cat("Дополнительные инвестиции:", round(impact$investment, ROUND_DIGITS), "\n")
cat("Совокупный мультипликатор выпуска:", round(impact$output_multiplier, ROUND_DIGITS), "\n\n")

cat("Таблица отдачи от стимулирования:\n")
print(investment_table)

cat("\nНовая таблица межотраслевого баланса после стимулирования:\n")
print(stimulated_balance_table)

if (!exists("SAVE_TABLE_TO_CSV") || isTRUE(SAVE_TABLE_TO_CSV)) {
  write.csv(investment_table, INVEST_OUTPUT_CSV, row.names = TRUE, fileEncoding = "UTF-8")
  write.csv(stimulated_balance_table, STIMULATED_OUTPUT_CSV, row.names = TRUE, fileEncoding = "UTF-8")
  cat("\nТаблица отдачи сохранена в:", INVEST_OUTPUT_CSV, "\n")
  cat("Новая таблица МОБ сохранена в:", STIMULATED_OUTPUT_CSV, "\n")
}
