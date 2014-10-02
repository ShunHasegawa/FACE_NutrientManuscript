#########################
# Subset and droplevles #
#########################
subsetD <- function(...) droplevels(subset(...))

#################################
# labells for facet_wrap graphs #
#################################
facet_wrap_labeller <- function(gg.plot,labels=NULL) {
  #works with R 3.0.1 and ggplot2 0.9.3.1
  # copied from http://stackoverflow.com/questions/19282897/
  # how-to-add-expressions-to-labels-in-facet-wrap
  # require(gridExtra)
  
  g <- ggplotGrob(gg.plot)
  gg <- g$grobs      
  strips <- grep("strip_t", names(gg))
  
  for(ii in seq_along(labels))  {
    modgrob <- getGrob(gg[[strips[ii]]], "strip.text", 
                       grep=TRUE, global=TRUE)
    gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
  }
  
  g$grobs <- gg
  class(g) = c("arrange", "ggplot",class(g)) 
  g
}

##############################
# Save ggplot in PDF and PNG #
##############################
ggsavePP <- function(filename, plot, width, height){
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = plot, 
         width = width, 
         height = height)
  
  ggsave(filename = paste(filename, ".png", sep = ""), 
         plot = plot, 
         width = width, 
         height = height, 
         dpi = 600)
}

######################
# Plot nutrient data #
######################
plt_ntr <- function(df, Labels, ylabs) {
  ## Blank data frame ## 
  
  # I woudl like to use the same y range for two layers of lysimeter and also for
  # nitrificaiton and N mineralisation. Create dataframe to define y range for
  # each plot. This data frame actually doesn't plot anything but enable one to
  # define y range (or x range if one likes)
  
  # compute min and max for each metric, then combine with type using expand.grid
  blankDF <- data.frame(
    ddply(df, .(metric), function(x){
      yval <- with(x, c(min(Mean - SE, na.rm = TRUE), max(Mean + SE, na.rm = TRUE)))
      exDF <- expand.grid(yval = yval, type = unique(x$type))
      return(exDF)
    }
    ), 
    co2 = "amb",
    date = df$date[1]
  )
  
  p <- ggplot(df, aes(x = date, y = Mean, group = co2))
  
  p2 <- p + geom_line(aes(linetype = co2)) + 
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                  width = 15, size = .3,
                  position = position_dodge(20)) + 
    geom_point(aes(shape = co2, fill = co2), position = position_dodge(20)) +
    labs(x = "Month", y = ylabs) +
    geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
               linetype = "dashed", col = "black") +
    scale_x_date(breaks= date_breaks("2 month"),
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2012-6-15", "2014-4-2"))) +
    scale_shape_manual(values = c(24, 21), labels = c("Ambient", expression(eCO[2]))) +
    scale_fill_manual(values = c("black", "white"), 
                      labels = c("Ambient", expression(eCO[2]))) +
    scale_linetype_manual(values = c("solid", "dashed"), 
                          labels = c("Ambient", expression(eCO[2]))) +
    facet_wrap(~type, scales = "free_y", ncol = 2) +
    geom_blank(aes(x = date, y = yval), data = blankDF) +
    science_theme
  pl <- facet_wrap_labeller(p2, labels = Labels)
  return(pl)
}