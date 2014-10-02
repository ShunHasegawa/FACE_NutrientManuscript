source("R/Pckg.R")

# load files to create figures


## Extractable
load("Data//FACE_Extractable_CO2Mean.RData")
ext <- TrtMean
ext$type <- factor(ifelse(ext$variable == "po", "Bray-extractable", "KCl-extractable"))

## IEM
load("Data//FACE_IEM_CO2Mean.RData")
iem <- TrtMean
iem$type <- "IEM-adsorbed"

## Lysimter
load("Data//FACE_Lysimeter_CO2Mean.RData")
lys <- TrtMean

# remove unused lysimeter measurement (tc, ic, tn)
lys <- subsetD(lys, !variable %in% c("tc", "ic", "tn"))

# po -> p
# shallow -> Shallow, deep -> Deep
# type: soil solution and depth
lys <- within(lys, {
  variable <- factor(variable, labels = c("no", "nh", "p", "toc"))
  depth <- factor(depth, labels = c("Shallow", "Deep"))
  type <- paste("Soil solution", depth, sep = "-")
  depth <- NULL
})

## Mineralisation
load("Data//FACE_Mineralisation_CO2Mean.RData")
mine <- TrtMean
mine <- within(mine, {
  type <- factor(variable, levels = c("nitrification", "n.min",
                                      "ammonification", "p.min"),
                 labels = c("Nitrification", "N mineralisation", 
                             "Ammonification", "P mineralisation"))
  
  variable <- recode(variable, "c('nitrification', 'n.min') = 'no'; 
                                    'p.min' = 'p';
                                    'ammonification' = 'nh'")
})

## merge the above data frame
ntrDF <- rbind.fill(list(ext, iem, lys, mine))

#######
# test #
########
df <- subset(ntrDF, variable == "no")
df$type <- factor(df$type, levels = c("KCl-extractable", "Soil solution-Shallow",
                                      "Nitrification", "IEM-adsorbed", 
                                      "Soil solution-Deep", "N mineralisation"))

science_theme <- theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       legend.position = "top",
                       legend.title = element_blank())


theme_set(theme_bw())


p <- ggplot(df, aes(x = date, y = Mean, group = co2))

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
  science_theme

# modify labels
ylabs <- c(expression(textstyle(atop("KCl extractable", paste((mg~kg^"-1"))))),
           expression(textstyle(atop("Soil solution (Shallow)", (mg~l^"-1")))),
           expression(textstyle(atop("Net nitrification rate", (mg~kg^"-1"~d^"-1")))),
           expression(textstyle(atop("IEM adsorbed", (ng~cm^"-1"~d^"-1")))),
           expression(textstyle(atop("Soil solution (Deep)", (mg~l^"-1")))),
           expression(textstyle(atop("Net N mineralisation rate", (mg~kg^"-1"~d^"-1")))))

pl <- facet_wrap_labeller(p2, labels = ylabs)
ggsavePP(filename = "Output//Fig/test", plot = pl, width = 7, height = 7)

  
  
  geom_text(aes(x = xv, y = yv * .95, label = labels),
            fontface = "bold",
            hjust = 1,
            data = subLabDF) +
  facet_grid(variable~., scales= "free_y", labeller= facetLab) +
  figTheme