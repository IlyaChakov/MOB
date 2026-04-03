# =========================
# ИНВЕСТИЦИОННЫЙ СЦЕНАРИЙ
# =========================

INVEST_SECTOR <- "Промышленность"
ADDITIONAL_INVESTMENT <- 50

INVEST_OUTPUT_CSV <- NULL
STIMULATED_OUTPUT_CSV <- NULL

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

source(file.path(project_dir, "code", "run_invest.r"))
