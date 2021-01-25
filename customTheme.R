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

    text = element_text(family = "FranklinGothic-Book",
                        angle = 55)
  )

text_color <- "#8B9BA8" # bluish grey

palette <-
  list(
    gold = "#F9C80E",
    orange = "#F86624",
    skyblue = "#52A4D3",
    mint = "#21D19F",
    red = "#FC3C0C",
    # "#EA3546", # red
    pink = "#FF938C", 
    purple = "#58267F", 
    lightyellow = "#F3FFBD", 
    green = "#39E547",
    lightorange = "#FCCC0C",
    white = "#FFFFFF"
  )
