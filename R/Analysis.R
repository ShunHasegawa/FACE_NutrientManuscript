source("R/Pckg.R")
source("R/Function.R")

###########
# Process #
###########

## Extractable
load("Data//FACE_Extractable_CO2Mean.RData")
ext <- TrtMean

# po -> p
levels(ext$variable)[3] <- "p"

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
ntrDF$type <- factor(ntrDF$type, levels = c("extractable", "IEM", "lys-shallow", 
                                            "lys-deep","nitrification", "n.min",
                                            "ammonification", "p.min"))

# split and make list
ntr_lst <- split(ntrDF, f = ntrDF$variable)
ntr_lst <- lapply(ntr_lst, droplevels)
save(ntr_lst, file = "Output//Data/FACE_Nutrient.RData")

##########
# Figure #
##########
theme_set(theme_bw())

science_theme <- theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1, size = 7),
                       legend.position = c(.9, .93),
                       legend.title = element_blank(),
                       legend.background = element_blank())
pl()

# modify labels
ylabs_lst <- list('no' = c(expression(A.~~KCl*-extractable~(mg~kg^"-1")),
                           expression(B.~~IEM*-adsorbed~(ng~cm^"-1"~d^"-1")),
                           expression(C.~~Soil~solution~(Shallow)~(mg~l^"-1")),
                           expression(D.~~Soil~solution~(Deep)~(mg~l^"-1")),
                           expression(E.~~Net~nitrification~(mg~kg^"-1"~d^"-1")),
                           expression(F.~~Net~N~mineralisation~(mg~kg^"-1"~d^"-1"))),
                  'nh' = c(expression(A.~~KCl*-extractable~(mg~kg^"-1")),
                           expression(B.~~IEM*-adsorbed~(ng~cm^"-1"~d^"-1")),
                           expression(C.~~Soil~solution~(Shallow)~(mg~l^"-1")),
                           expression(D.~~Soil~solution~(Deep)~(mg~l^"-1")),
                           expression(E.~~Net~ammonification~(mg~kg^"-1"~d^"-1"))),
                  'p' =  c(expression(A.~~Bray*-extractable~(mg~kg^"-1")),
                           expression(B.~~IEM*-adsorbed~(ng~cm^"-1"~d^"-1")),
                           expression(C.~~Soil~solution~(Shallow)~(mg~l^"-1")),
                           expression(D.~~Soil~solution~(Deep)~(mg~l^"-1")),
                           expression(E.~~Net~P~mineralisation~(mg~kg^"-1"~d^"-1")))
                  )
  

figtitles <- c("Nitrate", "Ammonium", "Phosphate")


pl <- function(){
l_ply(1:3, function(x) 
  ggsavePP(filename = paste("Output//Fig/FACE_Manuscript_", figtitles[x], sep = ""),
           plot = plt_ntr(df = ntr_lst[[x]], Labels = ylabs_lst[[x]], ylabs = figtitles[x]),
           width = 7, height = 8)
  )
}