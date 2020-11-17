library(ggkeyboard)
library(dplyr)
library(prismatic)

plot_layer <- function(g, i = length(g$layers)) {
  gg <- g
  gg$layers <- gg$layers[i]
  gg
}

highlight_keys_base <- function(keys, highlight_keys, title = NULL, palette = "pastel") {
  g <- ggkeyboard(keys, palette = keyboard_palette(palette))

  text_keys <- paste(gsub(" (Left|Right)", "", highlight_keys), collapse = " + ")

  g$layers[[2]]$data <-
    g$layers[[2]]$data %>%
    mutate(
      fill = if_else(!key %in% highlight_keys, clr_lighten(clr_grayscale(fill)), fill),
      colour = if_else(!key %in% highlight_keys, clr_lighten(clr_grayscale(colour)), colour),
      # text_colour = if_else(!key %in% highlight_keys, clr_desaturate(text_colour, 0.9), text_colour)
    )

  g$layers[[6]]$data <- g$layers[[6]]$data %>% mutate_at(c("fill", "colour"), clr_desaturate, 0.9)
  g$layers <- g$layers[-1]

  g +
    ggplot2::theme(
      plot.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, family = "Arial Unicode MS", size = 18),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, family = "Arial Unicode MS", size = 12),
    ) +
    ggplot2::ggtitle(title, text_keys)
}

highlight_keys_mac <- function(highlight_keys, title = NULL, palette = "pastel") {
  mac_keys <- mac %>%
    mutate(
      key_label = gsub("\nCommand", "\nCmd", key_label),
      key_label = gsub("\nOption", "\nOpt", key_label),
      key_label = recode(key_label, Ctrl = "^\nCtrl")
    )

  highlight_keys_base(mac_keys, highlight_keys, title, palette)
}

highlight_keys_win <- function(highlight_keys, title = NULL, palette = "pastel") {
  win_keys <- mac %>%
    mutate(
      key = sub("Command ", "Alt ", key),
      key = recode(key, "Option Left" = "Win Left", "Ctrl" = "Ctrl Left"),
      key_label = case_when(
        grepl("Command", key_label) ~ "Alt",
        key == "Option Right" ~ "Ctrl",
        key == "Win Left" ~ NA_character_,
        TRUE ~ key_label
      )
    )

  highlight_keys_base(win_keys, highlight_keys, title, palette)
}
