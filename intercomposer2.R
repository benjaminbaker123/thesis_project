library(MAPLE.emo)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(shadowtext)
library(ggpubr)
library(patchwork)
library(corrplot)
library(patchwork)
library(dplyr)
library(stringr)
library(ggsignif)

# Function to determine significance stars
get_significance_label <- function(p_value) {
  if (p_value < 0.001) {
    return("p < 0.001 ***")
  } else if (p_value < 0.01) {
    return("p < 0.01 **")
  } else if (p_value < 0.05) {
    return("p < 0.05 *")
  } else {
    return("p > 0.05 NS") # Not Significant
  }
}

process_shortened_pieceID_name <- function(df) {
  minor_rows <- grepl("minor", df$Mode)  # Identify rows where mode is "minor"
  
  df$shortened_pieceID_name[minor_rows] <- paste0(
    tolower(substr(df$shortened_pieceID_name[minor_rows], 1, 1)), 
    substr(df$shortened_pieceID_name[minor_rows], 2, nchar(df$shortened_pieceID_name[minor_rows]))
  )
  
  return(df)
}

### Attack Rate ###

analyze_attack_rate <- function(title, composer1, composer2, file_path, show_legend = "y") {
  # Read CSV
  ar <- read.csv(file_path)
  composer = composer1
  
  ar <- ar |>
    mutate(key_number = str_sub(PieceID, 1, nchar(PieceID) - 6)) |>
    mutate(New_Piece_ID = paste0(ifelse(Mode == "Major", "M", 
                                        ifelse(Mode == "minor", "m", "")),
                                 key_number))
  
  # Add a new column 'setCode' based on the composer input
  ar <- ar |>
    mutate(setCode = case_when(
      composer == "Scriabin" ~ "scriabin-1",
      composer == "Shostakovich" ~ "shostakovich-1",
      composer == "Kabalevsky" ~ "kabalevsky-1",
      TRUE ~ "Unknown"  # Default if none match
    ))
  
  ar |>
    mutate(
      New_Piece_ID = as.character(New_Piece_ID),
      setCode = as.character(setCode),
      pieceID_name = pretty_pieceID(New_Piece_ID, setCode)) -> ar
  
  ar |>
    mutate(shortened_pieceID_name = str_replace(pieceID_name, " minor$", "")) |>  # Replace " minor" at the end with "m"
    mutate(shortened_pieceID_name = str_replace(shortened_pieceID_name, " Major$", ""))  -> ar
  
  ar <- process_shortened_pieceID_name(ar)
  
  # Calculate correlation and p-value
  r <- cor.test(ar$Stimuli, ar$Full.Tracks)$estimate
  p_og <- cor.test(ar$Stimuli, ar$Full.Tracks)$p.value
  adjusted_p <- p.adjust(p_og, "bonferroni", n = 3)
  
  # Calculate R2 and p-value
  model <- lm(Stimuli~Full.Tracks, data=ar)
  
  #view model summary
  r2 <- summary(model)$r.squared
  r2_p_og <- summary(model)$coefficients[2, 4]
  r2_adjusted_p <- p.adjust(r2_p_og, "bonferroni", n = 3)
  sig_label <- get_significance_label(r2_adjusted_p)
  
  # Plot with ggplot
  plot <- ggplot(ar, aes(x = Stimuli, y = Full.Tracks, label = shortened_pieceID_name)) +
    ggtitle(title) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16)
    ) +
    geom_abline(color = "gray", linetype = "dashed") +
    annotate("text", x = 0, y = max(10), 
             label = paste0("R² = ", signif(r2, 3), ", ", sig_label), 
             hjust = 0, vjust = 1, size = 6) +
    geom_smooth(method = "lm", color = "black", alpha = 0.2) + # Lighter error bars (alpha set to 0.2)
    xlim(0, 10) + 
    ylim(0, 10) +
    xlab(paste("Composer 1 Attack Rate")) +
    ylab(paste("Composer 2 Attack Rate")) +
    geom_rug() +
    geom_text_repel(aes(colour = Mode), size = 5) +
    scale_colour_manual(values = c("minor" = "#799afd", "Major" = "#a00202")) +
    theme_maple()
  
  # Hide the legend if show_legend is "n"
  if (show_legend == "n") {
    plot <- plot + theme(legend.position = "none")
  } else {
    # Show the legend inside the plot
    plot <- plot + theme(
      legend.position = c(0.5, 0.85),
      legend.background = element_rect(
        fill = "white",   # Background color
        color = "black",  # Border color
        size = 0.5        # Border thickness
      ),
      legend.direction = "horizontal"
    )
  }
  
  return(plot)
}

### Pitch Height ###

analyze_pitch_height <- function(title, composer1, composer2, file_path, show_legend = "y") {
  # Read CSV
  ph <- read.csv(file_path)
  composer = composer1
  
  ph <- ph |>
    mutate(key_number = str_sub(PieceID, 1, nchar(PieceID) - 6)) |>
    mutate(New_Piece_ID = paste0(ifelse(Mode == "Major", "M", 
                                        ifelse(Mode == "minor", "m", "")),
                                 key_number))
  
  # Add a new column 'setCode' based on the composer input
  ph <- ph |>
    mutate(setCode = case_when(
      composer == "Scriabin" ~ "scriabin-1",
      composer == "Shostakovich" ~ "shostakovich-1",
      composer == "Kabalevsky" ~ "kabalevsky-1",
      TRUE ~ "Unknown"  # Default if none match
    ))
  
  ph |>
    mutate(
      New_Piece_ID = as.character(New_Piece_ID),
      setCode = as.character(setCode),
      pieceID_name = pretty_pieceID(New_Piece_ID, setCode)) -> ph
  
  ph |>
    mutate(shortened_pieceID_name = str_replace(pieceID_name, " minor$", "")) |>  # Replace " minor" at the end with "m"
    mutate(shortened_pieceID_name = str_replace(shortened_pieceID_name, " Major$", ""))  -> ph
  
  ph <- process_shortened_pieceID_name(ph)
  
  # Calculate correlation and p-value
  r <- cor.test(ph$Stimuli, ph$Full.Tracks)$estimate
  p_og <- cor.test(ph$Stimuli, ph$Full.Tracks)$p.value
  adjusted_p <- p.adjust(p_og, "bonferroni", n = 3)
  
  # Calculate R2 and p-value
  model <- lm(Stimuli~Full.Tracks, data=ph)
  
  #view model summary
  r2 <- summary(model)$r.squared
  r2_p_og <- summary(model)$coefficients[2, 4]
  r2_adjusted_p <- p.adjust(r2_p_og, "bonferroni", n = 3)
  sig_label <- get_significance_label(r2_adjusted_p)
  
  # Plot with points
  plot <- ggplot(ph, aes(x = Stimuli, y = Full.Tracks, label = shortened_pieceID_name)) +
    ggtitle(title) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16)
    ) +
    geom_abline(color = "gray", linetype = "dashed") +
    annotate("text", x = 50, y = 73, 
             label = paste0("R² = ", signif(r2, 3), ", ", sig_label), 
             hjust = 0, vjust = 1, size = 6) +
    geom_smooth(method = "lm", color = "black", alpha = 0.2) + # Lighter error bars (alpha set to 0.2)
    xlim(50, 73) + 
    ylim(50, 73) +
    xlab(paste("Composer 1 Average Pitch Height")) +
    ylab(paste("Composer 2 Average Pitch Height")) +
    geom_rug() +
    geom_text_repel(aes(colour = Mode), size = 5) +
    scale_colour_manual(values = c("minor" = "#799afd", "Major" = "#a00202")) +
    theme_maple()
  
  # Hide the legend if show_legend is "n"
  if (show_legend == "n") {
    plot <- plot + theme(legend.position = "none")
  } else {
    # Show the legend inside the plot
    plot <- plot + theme(
      legend.position = c(0.5, 0.1),
      legend.background = element_rect(
        fill = "white",   # Background color
        color = "black",  # Border color
        size = 0.5        # Border thickness
      ),
      legend.direction = "horizontal"
    )
  }
  
  return(plot)
}

### Mode ###

analyze_mode <- function(title, composer1, composer2, file_path, show_legend = "y") {
  # Read CSV
  mode <- read.csv(file_path)
  composer = composer1
  
  mode <- mode |>
    mutate(key_number = str_sub(PieceID, 1, nchar(PieceID) - 6)) |>
    mutate(New_Piece_ID = paste0(ifelse(Mode == "Major", "M", 
                                        ifelse(Mode == "minor", "m", "")),
                                 key_number))
  
  # Add a new column 'setCode' based on the composer input
  mode <- mode |>
    mutate(setCode = case_when(
      composer == "Scriabin" ~ "scriabin-1",
      composer == "Shostakovich" ~ "shostakovich-1",
      composer == "Kabalevsky" ~ "kabalevsky-1",
      TRUE ~ "Unknown"  # Default if none match
    ))
  
  mode |>
    mutate(
      New_Piece_ID = as.character(New_Piece_ID),
      setCode = as.character(setCode),
      pieceID_name = pretty_pieceID(New_Piece_ID, setCode)) -> mode
  
  mode |>
    mutate(shortened_pieceID_name = str_replace(pieceID_name, " minor$", "")) |>  # Replace " minor" at the end with "m"
    mutate(shortened_pieceID_name = str_replace(shortened_pieceID_name, " Major$", ""))  -> mode
  
  mode <- process_shortened_pieceID_name(mode)
  
  # Calculate correlation and p-value
  r <- cor.test(mode$Stimuli, mode$Full.Tracks)$estimate
  p_og <- cor.test(mode$Stimuli, mode$Full.Tracks)$p.value
  adjusted_p <- p.adjust(p_og, "bonferroni", n = 3)
  
  # Calculate R2 and p-value
  model <- lm(Stimuli~Full.Tracks, data=mode)
  
  #view model summary
  r2 <- summary(model)$r.squared
  r2_p_og <- summary(model)$coefficients[2, 4]
  r2_adjusted_p <- p.adjust(r2_p_og, "bonferroni", n = 3)
  sig_label <- get_significance_label(r2_adjusted_p)
  
  # Plot with points
  plot <- ggplot(mode, aes(x = Stimuli, y = Full.Tracks, label = shortened_pieceID_name)) +
    ggtitle(title) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 16),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16)
    ) +
    geom_abline(color = "gray", linetype = "dashed") +
    annotate("text", x = -0.5, y = 0.5, 
             label = paste0("R² = ", signif(r2, 3), ", ", sig_label), 
             hjust = 0, vjust = 1, size = 6) +
    geom_smooth(method = "lm", color = "black", alpha = 0.2) + # Lighter error bars (alpha set to 0.2)
    xlim(-0.5, 0.5) + 
    ylim(-0.5, 0.5) +
    xlab(paste("Composer 1 Mode Correlation")) +
    ylab(paste("Composer 2 Mode Correlation")) +
    geom_rug() +
    geom_text_repel(aes(colour = Mode), size = 5) +
    scale_colour_manual(values = c("minor" = "#799afd", "Major" = "#a00202")) +
    theme_maple()
  
  # Hide the legend if show_legend is "n"
  if (show_legend == "n") {
    plot <- plot + theme(legend.position = "none")
  } else {
    # Show the legend inside the plot
    plot <- plot + theme(
      legend.position = c(0.5, 0.1),
      legend.background = element_rect(
        fill = "white",   # Background color
        color = "black",  # Border color
        size = 0.5        # Border thickness
      ),
      legend.direction = "horizontal"
    )
  }
  
  return(plot)
}

plot1 <- analyze_attack_rate("Scriabin-Shostakovich", "Scriabin", "Shostakovich", "ar_sc_st.csv", show_legend = "n")
plot2 <- analyze_attack_rate("Scriabin-Kabalevsky", "Scriabin", "Kabalevsky", "ar_sc_ka.csv")
plot3 <- analyze_attack_rate("Shostakovich-Kabalevsky", "Shostakovich", "Kabalevsky", "ar_st_ka.csv", show_legend = "n")

plot1 <- plot1 + theme(axis.title.x = element_blank())  # Remove x-axis label
plot2 <- plot2 + theme(axis.title.y = element_blank())  # Remove y-axis label
plot3 <- plot3 + theme(axis.title.y = element_blank()) + theme(axis.title.x = element_blank())  # Remove x and y-axis label

attack_rate_plot <- (plot1 + plot2 + plot3) +
  plot_layout(guides = "keep")  # Avoid "collect" to keep individual plot legends

# Save the plot with the combined legend and labels
ggsave("inter_attack_rate.png", attack_rate_plot, 
       unit = "px", 
       height = 1750, 
       width = 5000)

plot4 <- analyze_pitch_height("Scriabin-Shostakovich", "Scriabin", "Shostakovich", "ph_sc_st.csv", show_legend = "n")
plot5 <- analyze_pitch_height("Scriabin-Kabalevsky", "Scriabin", "Kabalevsky", "ph_sc_ka.csv")
plot6 <- analyze_pitch_height("Shostakovich-Kabalevsky", "Shostakovich", "Kabalevsky", "ph_st_ka.csv", show_legend = "n")

plot4 <- plot4 + theme(axis.title.x = element_blank())  # Remove x-axis label
plot5 <- plot5 + theme(axis.title.y = element_blank())  # Remove y-axis label
plot6 <- plot6 + theme(axis.title.y = element_blank()) + theme(axis.title.x = element_blank())  # Remove x and y-axis label

pitch_height_plot <- (plot4 + plot5 + plot6) +
  plot_layout(guides = "keep")  # Avoid "collect" to keep individual plot legends

# Save the plot with the combined legend and labels
ggsave("inter_pitch_height.png", pitch_height_plot, 
       unit = "px", 
       height = 1750, 
       width = 5000)

plot7 <- analyze_mode("Scriabin-Shostakovich", "Scriabin", "Shostakovich", "mode_sc_st.csv", show_legend = "n")
plot8 <- analyze_mode("Scriabin-Kabalevsky", "Scriabin", "Kabalevsky", "mode_sc_ka.csv")
plot9 <- analyze_mode("Shostakovich-Kabalevsky", "Shostakovich", "Kabalevsky", "mode_st_ka.csv", show_legend = "n")

plot7 <- plot7 + theme(axis.title.x = element_blank())  # Remove x-axis label
plot8 <- plot8 + theme(axis.title.y = element_blank())  # Remove y-axis label
plot9 <- plot9 + theme(axis.title.y = element_blank()) + theme(axis.title.x = element_blank())  # Remove x and y-axis label

mode_plot <- (plot7 + plot8 + plot9) +
  plot_layout(guides = "keep")  # Avoid "collect" to keep individual plot legends

# Save the plot with the combined legend and labels
ggsave("inter_mode.png", mode_plot, 
       unit = "px", 
       height = 1750, 
       width = 5000)
