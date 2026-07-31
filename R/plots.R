# All plot builders.  They take plain data and return ggplot objects, so they
# can be tested without a Shiny session.

# Colour palettes offered in the plot settings.  "Okabe-Ito" and "Viridis" stay
# readable for colour blind readers and in greyscale print.
PALETTES <- list(
  "okabe_ito" = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                  "#0072B2", "#D55E00", "#CC79A7", "#000000"),
  "viridis" = NULL,   # generated on demand, it is a continuous scale
  "ggplot" = NULL,    # the ggplot2 default
  "npg" = NULL        # the Nature Publishing Group palette of ggsci
)

# Add the selected palette to a plot.  discrete_count is the number of levels
# that have to be covered.
apply_palette <- function(plot, palette, discrete_count, type = "colour"){

  if((is.null(palette))||(palette == "ggplot"))
    return(plot)

  if(palette == "npg"){
    return(plot + if(type == "fill") scale_fill_npg() else scale_color_npg())
  }

  values <- if(palette == "viridis"){
    viridisLite::viridis(max(discrete_count, 1), option = "D", end = 0.9)
  } else {
    palette_values(PALETTES[["okabe_ito"]], discrete_count)
  }

  return(plot + if(type == "fill"){
    scale_fill_manual(values = values)
  } else {
    scale_color_manual(values = values)
  })
}

# Repeat a palette until it covers n levels.
palette_values <- function(colours, n){
  if(n <= length(colours)) return(colours[seq_len(max(n, 1))])
  return(rep(colours, length.out = n))
}

# Draw the plate layout.
draw_layout <- function(plate_layout, well_plate, layoutfile){

  glayout <- ggplot(data=plate_layout, aes(x=Column, y=Row)) +
    labs(title= paste("Plate Layout:", layoutfile))

  if(well_plate == 96){
    glayout <- glayout +
      geom_point(size=9) +
      geom_point(size=7, aes(colour = elicitor)) +
      geom_point(size=5, aes(shape = genotype)) +
      coord_fixed(ratio=(13/12)/(9/8), xlim=c(0.8, 12.2), ylim=c(0.6, 8.4)) +
      scale_y_reverse(breaks=seq(1, 8), labels=LETTERS[1:8]) +
      scale_x_continuous(breaks=seq(1, 12))
  } else if (well_plate == 384){
    glayout <- glayout +
      geom_point(size=6) +
      geom_point(size=4, aes(colour = genotype)) +
      geom_point(size=5, aes(shape = elicitor)) +
      coord_fixed(ratio=(25/24)/(17/16), xlim=c(0.8, 24.2), ylim=c(0.6, 16.4)) +
      scale_y_reverse(breaks=seq(1, 16), labels=LETTERS[1:16]) +
      scale_x_continuous(breaks=seq(1, 24))
  }

  glayout <- glayout + theme_bw() +
    guides(colour = guide_legend(title = "Elicitors", ncol = 2, byrow = TRUE),
           shape = guide_legend(title = "Genotypes", ncol = 2, byrow = TRUE)) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      strip.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black"),
      legend.key=element_blank()
    )

  return(glayout)
}

# Draw the well curves of a whole plate.
draw_well_plate <- function(data, empty_wells, excluded_wells, mean_melted,
                            plate_mean, reference_plate_mean,
                            file_name, data_file, xlim, ylim){
  chosen_set <- data$chosen_set

  blanked <- unique(c(empty_wells, excluded_wells))
  if(length(blanked) > 0){
    chosen_set$value[chosen_set$well %in% blanked] <- 0
  }

  g <- ggplot(chosen_set, aes(time, value))

  if(data$well_plate == 384){
    g <- g + facet_wrap(~well, ncol = 24)
  } else {
    g <- g + facet_wrap(~well, ncol = 12)
  }

  if(is.null(mean_melted) == FALSE){
    g <- g + geom_line(data=mean_melted, aes(time, value), color = "red")
  }

  if(is.null(plate_mean) == FALSE){
    g <- g + geom_line(data=plate_mean, aes(time, values), color = "limegreen")
  }

  if(is.null(reference_plate_mean) == FALSE){
    g <- g + geom_line(data=reference_plate_mean, aes(time, values), color = "blue")
  }

  if(file_name){g <- g + labs(title = paste("rawdata file:", data_file))}

  g <- g + geom_line() + theme_bw() +
    theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    strip.text.x = element_blank(),
    strip.background = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black")
  )

  # one label per panel instead of one per measurement point
  labels <- data.frame(well = unique(chosen_set$well),
                       time = xlim[2]*0.85,
                       value = ylim*0.9,
                       stringsAsFactors = FALSE)
  labels$excluded <- labels$well %in% excluded_wells

  g <- g + geom_text(data = labels, aes(x = time, y = value, label = well,
                                        colour = excluded),
                     size = 3, show.legend = FALSE, inherit.aes = FALSE) +
    scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "red"),
                        guide = "none")

  g <- g + ylim(0, ylim) + xlim(xlim[1], xlim[2])

  return(g)
}

# Draw a summary bar plot with error bars.  Shared by the peak maxima and the
# area under the curve; both show one bar per elicitor, facetted by genotype.
draw_summary_bars <- function(bar_data, header, ylabel, arrangement, rotation,
                              palette = "ggplot"){

  bp <- ggplot(bar_data, aes(fill=elicitor, y=values, x=elicitor)) +
    facet_wrap(~genotype, ncol = if(arrangement == 2) 2 else 1) +
    ggtitle(header) + theme(legend.position = "none")

  if(rotation == 2){bp <- bp + coord_flip() + theme(legend.position = "none")}

  bp <- bp + geom_bar(position="dodge", stat="identity")
  bp <- bp + geom_errorbar(aes(ymin=values-sd, ymax=values+sd),
                           width=.2,                    # Width of the error bars
                           position=position_dodge(.9))

  bp <- apply_palette(bp, palette, length(unique(bar_data$elicitor)), type = "fill")

  bp <- bp + labs(x="", y=ylabel) +
    theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.5),
          legend.title=element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey90"),
          panel.grid.major = element_line(color = "grey90"),
          strip.background = element_rect(fill = "grey90", colour = NA),
          panel.grid.minor = element_line(colour = "grey90", linewidth = 0.25))

  return(bp)
}

# Draw the mean kinetics, optionally with the SD band and a reference curve.
draw_mean_kinetics <- function(line_data, header, ylabel, xlabel, sorting,
                               show_sd, reference = NULL, palette = "ggplot"){

  colour_by <- if(sorting == 2) "elicitor" else "genotype"
  facet_by <- if(sorting == 2) "genotype" else "elicitor"

  lpmax2 <- ggplot(line_data, aes(x=time, y=values)) +
    geom_line(aes(color = .data[[colour_by]])) +
    facet_wrap(as.formula(paste("~", facet_by)))

  if(show_sd){
    lpmax2 <- lpmax2 + ggtitle(header) +
      geom_line(aes(x=time, y=values-sd, color = .data[[colour_by]]), alpha=0.2) +
      geom_line(aes(x=time, y=values+sd, color = .data[[colour_by]]), alpha=0.2) +
      geom_errorbar(aes(ymin=values-sd, ymax=values+sd, color = .data[[colour_by]]),
                    alpha=0.2,
                    width=0,                    # Width of the error bars
                    position=position_dodge(.9))
  } else {
    lpmax2 <- lpmax2 + ggtitle("Mean")
  }

  lpmax2 <- apply_palette(lpmax2, palette,
                          length(unique(line_data[[colour_by]])), type = "colour")

  if(is.null(reference) == FALSE){
    # the reference carries no elicitor and no genotype, so ggplot draws it
    # into every facet
    lpmax2 <- lpmax2 + geom_line(data = reference, aes(x = time, y = values),
                                 inherit.aes = FALSE,
                                 colour = "black", linetype = "dashed")
  }

  lpmax2 <- lpmax2 + labs(x=xlabel, y=ylabel) +
    theme(legend.title=element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey90"),
          panel.grid.major = element_line(color = "grey90"),
          strip.background = element_rect(fill = "grey90", colour = NA),
          panel.grid.minor = element_line(colour = "grey90", linewidth = 0.25),
          legend.key=element_rect(fill='white'),
          strip.text.x = element_text(size=10))

  return(lpmax2)
}
