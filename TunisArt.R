library(maptools)
library(sp)
library(shapefiles)
library(geodata)
library(dplyr)
library(raster)
# Load geographic data for Tunisia using the geodata library
fdc <- getData(name="GADM", country="TUN", level=1)
summary(fdc)
donnees <- read.csv("repartition-des-visites.csv", header = TRUE, sep = ";",dec = ",", encoding = "latin1")
dim(donnees)
donnees
# Group the data by government region
grouped_data <- group_by(donnees, gov_name_f)

# Summarize the data to calculate the average number of visits
df <- summarize(grouped_data, avg_Visite = mean(Visites))
df
#df <- donnees %>% group_by(gov_name_f) %>% summarize(avg_price = mean(Visites))
pt <- cbind(fdc@data[, "GID_1"], as.data.frame(coordinates(fdc)))
colnames(pt) <- c("GID_1", "x", "y")
i <- merge(pt, donnees, by =c("GID_1"),sort=T)
# View the first few rows of the merged data
head(i)
i$var <- i$Visites
i$Visites
x1 <- bbox(fdc)[1]
y1 <- bbox(fdc)[2]
x2 <- bbox(fdc)[3]
y2 <- bbox(fdc)[4]
sfdc <- (x2 - x1) * (y2 - y1)
sc <- sum(i$var, na.rm = TRUE)
sc
k <- 0.2  
i$size <- sqrt((i$var * k * sfdc/sc)/pi)
plot(fdc, border = "white", col = "grey")
symbols(i[, c("x", "y")], circles = i$size, add = TRUE, bg = "red", inches = FALSE)
## Titre de la légende.
LegTitle <- "repartition des visites des monuments par gouvernorat 2015\n"
## Tracer des cercles dont la taille indique les valeurs de la variable population.
rLeg <- quantile(pt$size, c(1, 0.9, 0.25, 0), type = 1, na.rm = TRUE);
rLeg
rVal <- quantile(pt$var, c(1, 0.9, 0.25, 0), type = 1, na.rm = TRUE);
rVal
l <- data.frame(x = x1, y = y1);
head(l)
xinit <- l$x + rLeg[1];xinit
ypos <- l$y + rLeg;ypos
symbols(x = rep(xinit, 4), y = ypos, circles = rLeg, add = TRUE, bg = "red",inches = FALSE)
text(x = rep(xinit, 4) + rLeg[1] * 1.2, y = (l$y + (2 * rLeg)), rVal, cex = 0.3,srt = 0, adj = 0)
for (i in 1:4) {
  segments(xinit, (l$y + (2 * rLeg[i])), xinit + rLeg[1] * 1.1, (l$y + (2 *rLeg[i])))
}
text(x = xinit - rLeg[1], y = (l$y + (2 * rLeg[1])), LegTitle, adj = c(0, 0),cex = 0.7)
# Titre
title(main = "repartition des visites des monuments par gouvernorat, 2015",  cex.sub = 0.7)
i <- merge(pt, donnees, by =c("GID_1"),sort=T)

i$var <- i$Visites
var <- as.vector(na.omit(i$var))
var
nbclass <- 8
library(classInt)
istr <- classIntervals(var, nbclass, style = "quantile")$brks
library(RColorBrewer)
colours <- brewer.pal(nbclass, "YlOrRd")
colMap <- colours[(findInterval(i$var, istr, all.inside = TRUE))]
plot(fdc, col = colMap, border = "black", lwd = 1)
legend(x = "topright", legend = leglabs(round(istr, 2), over = "plus de", under = "moins de"), fill = colours, bty = "n", pt.cex = 1, cex = 0.7, title = "indice 0-1")
# Titre
title(main = "Indicateur de developpement regional", cex.sub = 0.7)