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