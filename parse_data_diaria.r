library("tidyverse")

data <- read.csv("raw_data.csv", header = TRUE, sep = ",", dec = ",")

data_diaria <- aggregate(data$VALOR, by = list(data$FECHA), FUN = sum)

data_diaria <- data_diaria %>%
  transmute(
    FECHA = as.Date(Group.1, format = "%d/%m/%Y"),
    PRECIPITACION = x
  ) %>%
  arrange(FECHA)

missing <- anti_join(
  data.frame(
    FECHA = seq.Date(
      from = min(data_diaria$FECHA),
      to = max(data_diaria$FECHA),
      by = "day"
    )
  ),
  data_diaria,
  by = "FECHA"
)

data_diaria <- full_join(data_diaria, missing, by = "FECHA") %>%
  arrange(FECHA) %>%
  mutate(PRECIPITACION = ifelse(is.na(PRECIPITACION), NA, PRECIPITACION)) %>%
  write.csv("data_diaria.csv", row.names = FALSE)
