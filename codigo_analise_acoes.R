library(tidyverse)
library(readxl)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

base = read_xlsx(path = "Dados para atividade.xlsx")

base_pivo = pivot_longer(data = base, cols = c("ABEV3","BBAS3","BBDC4","ITUB4","JBSS3",
                                               "KLBN3","PETR4","SANB3","VALE3","WEGE3"),
                         names_to = "ticker", values_to = "preco")

base_pivo <- base_pivo %>%
  mutate(preco = gsub(preco, pattern = ",", replacement = ".")) |>
  mutate(preco = as.numeric(preco))

base_pivo$Data <- as.Date(base_pivo$Data, format = "%Y-%m-%d")

base_1 = base_pivo |>
  fill(ticker) |>
  filter(preco !="")

str(base_1)

base_organizada = base_1 |>
  arrange(ticker, Data) |> 
  group_by(ticker) |>
  mutate(r_diario = (preco - lag(preco)) / lag(preco)) |>
  ungroup()

base_organizada <- base_organizada %>%
  group_by(ticker) %>%
  mutate(r_diario = if_else(is.na(r_diario), 0, r_diario),
         valor_investido = 100 * cumprod(1 + r_diario)) %>%
  ungroup()

ggplot(base_organizada, aes(x = Data, y = valor_investido, color = ticker)) + 
  geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-02-28"), ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.4) + 
  labs(x = "Data", y = "Valor Investido", title = "Retorno Acumulado ao Longo do Tempo por Ação", color = "Ticker")
