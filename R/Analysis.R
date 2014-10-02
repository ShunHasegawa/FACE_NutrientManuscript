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



