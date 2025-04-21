source("load_data.R")

# dient nur dazu um die ggplot2-zu testen

# Filtere nach benötigtem Renderplot (Server)
df_gefiltert <- df |>
  filter(ZFA == "extern_kunde")

# Erstelle ein PieChart-Diagramm basierend auf dem DataFrame `df_gefiltert`
ggplot(df_gefiltert, aes(x = 2, fill = `16.04.2025`)) +
  geom_bar(stat = "count", color = "white") +
  coord_polar("y", start = 0, direction = -1) +
  xlim(c(1, 2.5)) + 
  scale_fill_manual(values = farben, na.translate = FALSE) +
  labs(
    title = "chart_title",
    fill = NULL
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 22,
      face = "bold",
      color = "grey44"),
    legend.position = "bottom",
    legend.justification = "right",
  )
