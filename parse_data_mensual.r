library("tidyverse")

data_diaria <- read.csv("data_diaria.csv", header = TRUE, sep = ",", dec = ".")

data_mensual <- data_diaria %>%
  mutate(
    MONTH = format(as.Date(FECHA), "%m"),
    HYDRO_YEAR = ifelse(
      as.numeric(format(as.Date(FECHA), "%m")) <= 8,
      as.numeric(format(as.Date(FECHA), "%Y")) - 1,
      as.numeric(format(as.Date(FECHA), "%Y"))
    )
  ) %>%
  group_by(HYDRO_YEAR, MONTH) %>%
  summarise(
    PRECIPITACION = sum(PRECIPITACION)
  ) %>%
  spread(MONTH, PRECIPITACION) %>%
  select(
    HYDRO_YEAR,
    SEP = `09`,
    OCT = `10`,
    NOV = `11`,
    DIC = `12`,
    ENE = `01`,
    FEB = `02`,
    MAR = `03`,
    ABR = `04`,
    MAY = `05`,
    JUN = `06`,
    JUL = `07`,
    AGO = `08`,
  ) %>%
  # delete first row
  filter(HYDRO_YEAR != 1963) %>%
  write.csv("data_mensual.csv", row.names = FALSE)
