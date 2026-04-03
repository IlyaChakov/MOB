make_balance_table <- function(iot, digits = 2) {
  Z <- iot$Z
  y <- iot$y
  x <- iot$x
  v <- iot$v
  sectors <- iot$sectors
  n <- length(sectors)

  # Верхний блок: Z + конечный спрос + выпуск
  upper <- cbind(Z, "Конечный спрос" = y, "Выпуск" = x)

  # Нижняя строка: добавленная стоимость + пусто + итог по столбцам
  lower_v <- c(v, sum(y), sum(x))
  lower_total <- c(colSums(Z), sum(y), sum(x))

  table <- rbind(
    upper,
    "Добавленная стоимость" = lower_v,
    "Итого затраты" = lower_total
  )

  table <- round(table, digits)
  as.data.frame(table, check.names = FALSE)
}

make_coefficients_table <- function(iot, digits = 4) {
  A <- round(iot$A, digits)
  as.data.frame(A, check.names = FALSE)
}

make_investment_table <- function(iot, impact, digits = 2) {
  result <- cbind(
    "Базовый выпуск" = iot$x,
    "Доп. конечный спрос" = impact$delta_y,
    "Прирост выпуска" = impact$delta_x,
    "Новый выпуск" = impact$stimulated_iot$x,
    "Отдача на 1 ед. инвестиций" = impact$delta_x / impact$investment
  )

  result <- round(result, digits)
  as.data.frame(result, check.names = FALSE)
}
