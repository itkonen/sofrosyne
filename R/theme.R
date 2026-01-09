
app_theme <- function() {
  bs_theme(
    version = 5,
    primary = "#4ECDC4",   # Calming, fresh teal green for primary elements
    secondary = "#FF6F61", # Warm, inviting coral for secondary elements
    info = "#2980B9",      # Deep blue for information (trustworthy and calming)
    success = "#58D68D",   # Bright green for success (motivating and vibrant)
    warning = "#F4D03F",   # Bright yellow for warnings (clear and attention-grabbing)
    danger = "#E74C3C",    # Slightly darker red for danger (serious and bold)
    base_font = "Arial",   # Keeping Arial as it is clean and widely used
    green = "#4ECDC4",     # Same as primary teal
    orange = "#F39C12",    # A softer orange for complementary usage
    blue = "#2980B9",      # Same as info blue
    red = "#E74C3C",       # Same as danger red
    teal = "#48C9B0"       # A lighter shade of teal for subtle highlights
  )
}

color_palette <- c(
  primary_green = "#4CAF50",
  secondary_orange = "#FF9800",
  accent_blue = "#2196F3",
  warning_yellow = "#FFEB3B",
  success_light_green = "#8BC34A",
  info_teal = "#009688",
  dark_gray_text = "#424242",
  medium_gray_text = "#757575"
)
