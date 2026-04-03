# =========================
# ИСХОДНЫЕ ДАННЫЕ
# Заполняйте все данные, которые у вас есть.
# Если каких-то данных нет, оставьте NULL.
# =========================

sectors <- c("Сельское хозяйство", "Промышленность", "Услуги")

x <- c(120, 200, 180)

A <- matrix(
  c(
    0.10, 0.20, 0.05,
    0.15, 0.10, 0.10,
    0.05, 0.15, 0.10
  ),
  nrow = 3,
  byrow = TRUE
)

Z <- matrix(
  c(
    12, 40, 9,
    18, 20, 18,
    6, 30, 18
  ),
  nrow = 3,
  byrow = TRUE
)

y <- c(59, 144, 126)

# =========================
# НАСТРОЙКИ ЗАПУСКА
# =========================

ACTIVE_CASE <- "x_A"
ROUND_DIGITS <- 2
PRINT_TABLE <- TRUE
SAVE_TABLE_TO_CSV <- TRUE
OUTPUT_CSV <- NULL

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

project_dir <- dirname(get_current_script_dir())

if (!exists("RUN_MOB_BUILD") || isTRUE(RUN_MOB_BUILD)) {
  source(file.path(project_dir, "code", "run_mob_build.r"))
}
