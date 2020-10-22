custom_theme <-
  theme(
    axis.title = element_text(color = "#8B9BA8"),
    axis.ticks = element_line(color = "#8B9BA8"),
    axis.text = element_text(color = "#8B9BA8"),

    legend.background = element_rect(fill = "#272B30", color = "#8B9BA8", size = .1),
    legend.text = element_text(color = "#8B9BA8"),
    legend.title = element_text(color = "#8B9BA8"),
    legend.key = element_rect(fill = "#272B30", size = 0),

    panel.grid = element_line(colour = "#8B9BA8"),
    panel.background = element_rect(fill = "#272B30", size = 0),
    panel.border = element_blank(),

    plot.background = element_rect(fill = "#272B30", size = 0),
    plot.title = element_text(color = "#8B9BA8", face = "bold"),
    plot.subtitle = element_text(color = "#8B9BA8"),
    plot.caption = element_text(color = "#8B9BA8", face = "italic"),

    strip.background = element_rect(fill = "#272B30"),
    strip.text = element_text(color = "#8B9BA8", face = "bold"),

    text = element_text(family = "FranklinGothic-Book")
  )

text_color <- "#8B9BA8" # bluish grey

custom_palette <-
  c(
    "#F9C80E", # gold
    "#F86624", # orange
    "#52A4D3", # sky blue
    "#21D19F", # mint
    "#FC3C0C", # red
    # "#EA3546", # red
    "#FF938C", # pink
    "#58267F", # purple
    "#F3FFBD", # light yellow
    "#39E547" # green
  )
