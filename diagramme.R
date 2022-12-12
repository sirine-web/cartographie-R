library(maptools)
library(sp)
library(shapefiles)
library(geodata)
library(dplyr)
library(raster)
library(ggplot2)
# Load geographic data for Tunisia using the geodata library
fdc <- getData(name="GADM", country="TUN", level=1)
summary(fdc)
donnees <- read.csv("visites-payantes-tunisiens-et-etrangers-2020.csv", header = TRUE, sep = ";",dec = ",", encoding = "latin1")
dim(donnees)

# Group the data by government region
grouped_data <- group_by(donnees, donnees$GID_1)

# Summarize the data to calculate the average number of visits
df <- summarize(grouped_data, avg_Visite = mean(Visites))
df

visits <- data.frame(month = c("Janvier","Fevrier","Mars","Avril","Mai","Juin","Juillet","Aout","Septembre","Octobre","Novembre","Decembre"),
                     nb_visits = c(sum(donnees$Janvier),sum(donnees$Fevrier),sum(donnees$Mars),sum(donnees$Avril),sum(donnees$Mai),sum(donnees$Juin),sum(donnees$Juillet),sum(donnees$Aout),sum(donnees$Septembre),sum(donnees$Octobre),sum(donnees$Novembre),sum(donnees$Decembre)))

ggplot(data = visits, aes(x = month, y = nb_visits)) +
  geom_line()
