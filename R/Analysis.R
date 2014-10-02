source("R/Pckg.R")

# load files to create figures


## Extractable
load("Data//FACE_Extractable_CO2Mean.RData")
ext <- TrtMean
ext <- within(ext, {
  type <- "extractable"
  metric <- "extractable"
})

## IEM
load("Data//FACE_IEM_CO2Mean.RData")
iem <- TrtMean
iem <- within(iem, {
  type <- "IEM"
  metric <- "IEM"
})

## Lysimter
load("Data//FACE_Lysimeter_CO2Mean.RData")
lys <- TrtMean

# remove unused lysimeter measurement (tc, ic, tn)
lys <- subsetD(lys, !variable %in% c("tc", "ic", "tn"))

# po -> p
# type: soil solution and depth
lys <- within(lys, {
  variable <- factor(variable, labels = c("no", "nh", "p", "toc"))
  type <- paste("lys", depth, sep = "-")
  depth <- NULL
  metric <- "Lysmtr"
})

## Mineralisation
load("Data//FACE_Mineralisation_CO2Mean.RData")
mine <- TrtMean
mine <- within(mine, {
  type <- variable
  variable <- recode(variable, "c('nitrification', 'n.min') = 'no'; 
                                    'p.min' = 'p';
                                    'ammonification' = 'nh'")
  metric <- "Mineralisation"
})

## merge the above data frame
ntrDF <- rbind.fill(list(ext, iem, lys, mine))

#######
# test #
########
df <- subset(ntrDF, variable == "no")
df$type <- factor(df$type)
levels(df$type)

df$type <- factor(df$type, levels = c("extractable", "IEM",
                                      "lys-shallow", "lys-deep", 
                                      "nitrification", "n.min"))


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


pl <- plf()
ggsavePP(filename = "Output//Fig/test", plot = pl, width = 7, height = 7)


theme_set(theme_bw())

science_theme <- theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       legend.position = c(.9, .9),
                       legend.title = element_blank())

plf <- function()
{p <- ggplot(df, aes(x = date, y = Mean, group = co2))

p2 <- p + geom_line(aes(linetype = co2)) + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 15, size = .3,
                position = position_dodge(20)) + 
  geom_point(aes(shape = co2, fill = co2), position = position_dodge(20)) +
  labs(x = "Month", y = "Nitrate") +
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

# modify labels
ylabs <- c(expression(A.~~KCl*-extractable~(mg~kg^"-1")),
           expression(B.~~IEM*-adsorbed~(ng~cm^"-1"~d^"-1")),
           expression(C.~~Soil~solution~(Shallow)~(mg~l^"-1")),
           expression(D.~~Soil~solution~(Deep)~(mg~l^"-1")),
           expression(E.~~Net~nitrification~(mg~kg^"-1"~d^"-1")),
           expression(F.~~Net~N~mineralisation~(mg~kg^"-1"~d^"-1")))

pl <- facet_wrap_labeller(p2, labels = ylabs)
return(pl)
}

ggsavePP(filename = "Output//Fig/test", plot = pl, width = 7, height = 7)
