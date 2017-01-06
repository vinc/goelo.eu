library(raster)
library(rgdal)
library(RColorBrewer)
library(ncdf4)
library(gfcanalysis)
library(maptools)
library(viridis)
library(scales)
library(rgeos)
library(gdata)

colors.green              <- "#00AD59"
colors.red                <- "#E24D50"
colors.blue               <- "#6FCB9F"
colors.light_brown        <- "#FFFEB3"
colors.dark_brown         <- "#666547"
colors.darker_brown       <- "#454530"

goelo.communes <- sort(c(
  "Binic", "Cohiniac", "Étables-sur-Mer", "Île-de-Bréhat", "Kerfot",
  "La Méaugon", "Lanleff", "Lanloup", "Lannebert", "Lantic", "Lanvollon",
  "Le Fœil", "Paimpol", "Plaine-Haute", "Plerneuf", "Ploubazlanec", "Plouha",
  "Plourhan", "Plourivo", "Plouvara", "Plouézec", "Pludual", "Pléguien",
  "Pléhédel", "Plélo", "Plérin", "Pordic", "Quintin", "Saint-Donan",
  "Saint-Quay-Portrieux", "Tressignaux", "Trégomeur", "Tréguidel", "Trémuson",
  "Tréméloir", "Tréméven", "Tréveneuc", "Yvias"
))

# Load Goelo map from GADM
#france <- getData('GADM', country='FRA', level=5)
#saint_brieuc <- france[france$NAME_3 %in% "Saint-Brieuc", ]
#goelo <- saint_brieuc[saint_brieuc$NAME_5 %in% goelo.communes, ]
#goelo <- crop(goelo, extent(xmin(goelo), xmax(goelo), ymin(goelo), 49))
#goelo <- goelo[order(goelo$NAME_5), ] # Reorder by commune name
#goelo.borders <- gUnionCascaded(goelo)

# Load Goelo map from OpenStreetMap
#france <- readOGR(".", "communes-20160614")
france <- readOGR(".", "communes-20150101-5m")
#france <- readOGR(".", "communes-20150101-50m")
goelo <- france[france$nom %in% goelo.communes, ]
goelo <- goelo[goelo$insee != "29297", ] # Remove Tréméven in Finistere
goelo <- goelo[order(goelo$nom), ] # Reorder by commune name
goelo.communes <- goelo$nom # Tréméloir doesn't exist in OpenStreetMap
goelo.borders <- gUnionCascaded(goelo)

#HACK: to speed up plotting
#france <- getData('GADM', country='FRA', level=2)

goelo.xmin   <- -3.199
goelo.xmax   <- -2.648
goelo.ymin   <- 48.35
goelo.ymax   <- 48.93
goelo.xlim   <- c(goelo.xmin, goelo.xmax)
goelo.ylim   <- c(goelo.ymin, goelo.ymax)
goelo.w      <- goelo.xmax - goelo.xmin
goelo.h      <- goelo.ymax - goelo.ymin
goelo.bg     <- gUnionCascaded(crop(france, extent(goelo.xlim, goelo.ylim)))

ratio <- 1.5
goelo.height <- 500 
goelo.width <- goelo.height * goelo.w / goelo.h / ratio


# Administrative Limits

png("goelo-communes.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo, col=colors.light_brown, border=colors.darker_brown, add=TRUE)
dev.off()


# Rivers

brittany.rivers <- readOGR("53_BRETAGNE", "COURS_D_EAU")
goelo.rivers_names <- c("le Trieux", "le Leff", "le Gouët")
goelo.rivers <- brittany.rivers[brittany.rivers$TOPONYME %in% goelo.rivers_names, ]
goelo.rivers <- spTransform(goelo.rivers, crs(goelo))

png("goelo-rivers.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.rivers, col=colors.blue, lwd=2, add=TRUE)
dev.off()

# length
round(SpatialLinesLengths(goelo.rivers), 2)

# Natura 2000

sic <- readOGR(".", "sic1609")
sic <- spTransform(sic, crs(goelo))

png("goelo-sic.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.borders, col=colors.light_brown, border=colors.darker_brown, add=TRUE)
plot(sic, col=alpha(colors.green, 0.5), border=alpha(colors.green, 0.5), add=TRUE)
dev.off()

cdl <- readOGR("cdl/cdl2016_03", "N_ENP_SCL_S_000")
cdl <- spTransform(cdl, crs(goelo))

png("goelo-cdl.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.borders, col=colors.light_brown, border=colors.darker_brown, add=TRUE)
plot(cdl, col=alpha(colors.green, 0.5), border=alpha(colors.green, 0.5), add=TRUE)
dev.off()

znieff1 <- readOGR(".", "ZNIEFF1_G2")
znieff1 <- spTransform(znieff1, crs(goelo))

png("goelo-znieff1.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.borders, col=colors.light_brown, border=colors.darker_brown, add=TRUE)
plot(znieff1, col=alpha(colors.green, 0.5), border=alpha(colors.green, 0.5), add=TRUE)
dev.off()


znieff2 <- readOGR(".", "ZNIEFF2_G2")
znieff2 <- spTransform(znieff2, crs(goelo))

png("goelo-znieff2.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.borders, col=colors.light_brown, border=colors.darker_brown, add=TRUE)
plot(znieff2, col=alpha(colors.green, 0.5), border=alpha(colors.green, 0.5), add=TRUE)
dev.off()


round(100 * sum(area(crop(sic, goelo))) / sum(area(goelo)), 2)
round(100 * sum(area(crop(cdl, goelo))) / sum(area(goelo)), 2)
round(100 * sum(area(crop(znieff1, goelo))) / sum(area(goelo)), 2)
round(100 * sum(area(crop(znieff2, goelo))) / sum(area(goelo)), 2)


# Population

insee <- read.xls("dep22.xls", sheet=3, skip=6, header=TRUE, fileEncoding="latin1", method="csv")
insee$Nom.de.la.commune <- gsub("Le Foeil", "Le Fœil", insee$Nom.de.la.commune)
insee$Population.municipale <- as.numeric(gsub(",", "", insee$Population.municipale))
goelo.insee <- insee[insee$Nom.de.la.commune %in% goelo.communes,]
goelo.insee <- goelo.insee[order(goelo.insee$Nom.de.la.commune),] # Reorder by commune name
color_ratio <- goelo.insee$Population.municipale / max(goelo.insee$Population.municipale)

sum(goelo.insee$Population.municipale) # Total Population

png("goelo-population.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo, col=colors.light_brown, border=NA, add=TRUE)
plot(goelo, col=heat.colors(256, color_ratio), border=colors.darker_brown, add=TRUE)
dev.off()


# Land Cover

clc <- readOGR(".", "CLC12_D022_RGF")
clc <- spTransform(clc, crs(goelo))
goelo.clc <- crop(clc, goelo)
goelo.clc$CODE_12 <- as.numeric(as.character(goelo.clc$CODE_12))
goelo.clc.artif <- goelo.clc[goelo.clc$CODE_12 %/% 100 == 1,]
goelo.clc.agric <- goelo.clc[goelo.clc$CODE_12 %/% 100 == 2,]
goelo.clc.fores <- goelo.clc[goelo.clc$CODE_12 %/% 100 == 3,]
goelo.clc.wetla <- goelo.clc[goelo.clc$CODE_12 %/% 100 == 4,]
goelo.clc.water <- goelo.clc[goelo.clc$CODE_12 %/% 100 == 5,]
#goelo.clc.artif.col <- rgb(230, 000, 077, maxColorValue=255)
#goelo.clc.agric.col <- rgb(255, 255, 168, maxColorValue=255)
#goelo.clc.fores.col <- rgb(128, 255, 000, maxColorValue=255)
#goelo.clc.wetla.col <- rgb(166, 166, 255, maxColorValue=255)
#goelo.clc.water.col <- rgb(000, 204, 242, maxColorValue=255)

png("goelo-clc.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.borders, col=colors.light_brown, border=NA, add=TRUE)
plot(goelo.clc.fores, col=colors.green, border=alpha(colors.green, 0), add=TRUE)
plot(goelo.clc.artif, col=colors.red, border=alpha(colors.red, 0), add=TRUE)
plot(goelo.borders, col=NA, border=colors.darker_brown, add=TRUE)
dev.off()


# Tree Cover

colors.forest <- gradient_n_pal(c(colors.light_brown, colors.green))(seq(0, 1, length.out=256))
tiles <- calc_gfc_tiles(goelo)
download_tiles(tiles, ".")
gfc <- extract_gfc(goelo, ".", filename='gfc_NAK_extract.tif', overwrite=TRUE)
goelo.gfc <- crop(gfc, extent(goelo))
goelo.gfc <- mask(goelo.gfc, goelo)

goelo.area <- area(goelo)
goelo.forest <- threshold_gfc(goelo.gfc)
stats <- gfc_stats(goelo, goelo.forest)
cover <- stats$loss_table$cover[stats$loss_table$year == 2014]
goelo.forest_area <- 10000 * cover

100 * sum(goelo.forest_area) / sum(goelo.area) # forest cover

png("goelo-forests.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.forest$forest2000, col=colors.forest, legend=FALSE, add=TRUE)
plot(goelo.borders, border=colors.dark_brown, add=TRUE)
dev.off()

# Location in Europe

fra <- getData('GADM', country='FRA', level=2)

brittany.departements <- c(
  "Côtes-d'Armor",
  "Ille-et-Vilaine",
  "Finistère",
  "Morbihan", 
  "Loire-Atlantique"
)
brittany <- fra[fra$NAME_2 %in% brittany.departements, ]


# Goelo in Brittany

brittany.xmin   <- -5.5
brittany.xmax   <- -0.7
brittany.ymin   <- 46.8
brittany.ymax   <- 49.0
brittany.xlim   <- c(brittany.xmin, brittany.xmax)
brittany.ylim   <- c(brittany.ymin, brittany.ymax)
brittany.w      <- brittany.xmax - brittany.xmin
brittany.h      <- brittany.ymax - brittany.ymin
brittany.bg     <- gUnionCascaded(crop(france, extent(brittany.xlim, brittany.ylim)))

ratio <- 1.5
brittany.height <- 500 
brittany.width <- brittany.height * brittany.w / brittany.h / ratio

png("brittany-goelo.png", width=brittany.width, height=brittany.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(brittany.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=brittany.xlim, ylim=brittany.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(brittany, border=colors.darker_brown, add=TRUE)
plot(goelo.borders, col=colors.light_brown, border=colors.darker_brown, add=TRUE)
dev.off()


# Cotes d'Armor in Brittany

cotes_armor <- fra[fra$NAME_2 == "Côtes-d'Armor", ]
cotes_armor.borders <- gUnionCascaded(cotes_armor)

png("brittany-cotes-armor.png", width=brittany.width, height=brittany.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(brittany.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=brittany.xlim, ylim=brittany.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(brittany, border=colors.darker_brown, add=TRUE)
plot(cotes_armor.borders, col=colors.light_brown, border=colors.darker_brown, add=TRUE)
dev.off()


# Brittany in Atlantic Europe

atl <- gUnion(getData('GADM', country='AND', level=1), fra)
atl <- gUnion(getData('GADM', country='AUT', level=2), atl)
atl <- gUnion(getData('GADM', country='BEL', level=2), atl)
atl <- gUnion(getData('GADM', country='CHE', level=2), atl)
atl <- gUnion(getData('GADM', country='DEU', level=2), atl)
atl <- gUnion(getData('GADM', country='ESP', level=2), atl)
atl <- gUnion(getData('GADM', country='GBR', level=2), atl)
atl <- gUnion(getData('GADM', country='GGY', level=1), atl)
atl <- gUnion(getData('GADM', country='IRL', level=1), atl)
atl <- gUnion(getData('GADM', country='ITA', level=2), atl)
atl <- gUnion(getData('GADM', country='JEY', level=1), atl)
atl <- gUnion(getData('GADM', country='LIE', level=1), atl)
atl <- gUnion(getData('GADM', country='LUX', level=2), atl)
atl <- gUnion(getData('GADM', country='NLD', level=2), atl)
atl <- gUnion(getData('GADM', country='PRT', level=2), atl)

atlantic.xmin   <- -11.2
atlantic.xmax   <- 10.0
atlantic.ymin   <- 40.2
atlantic.ymax   <- 52.7
atlantic.xlim   <- c(atlantic.xmin, atlantic.xmax)
atlantic.ylim   <- c(atlantic.ymin, atlantic.ymax)
atlantic.w      <- atlantic.xmax - atlantic.xmin
atlantic.h      <- atlantic.ymax - atlantic.ymin
atlantic.bg <- gUnionCascaded(crop(atl, extent(atlantic.xlim, atlantic.ylim)))

brittany.borders <- gUnionCascaded(brittany)

ratio <- 1.5
atlantic.height <- 500 
atlantic.width <- atlantic.height * atlantic.w / atlantic.h / ratio

png("atlantic-brittany.png", width=atlantic.width, height=atlantic.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(atlantic.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=atlantic.xlim, ylim=atlantic.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(brittany.borders, col=colors.light_brown, border=colors.darker_brown, add=TRUE)
dev.off()


# Climate

colors.temp <- inferno(256)
colors.rain <- rev(viridis(256))

climate <- getData('worldclim', var='bio', res=0.5, lon=-10, lat=45)

names(climate) <- c(
  "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9",
  "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17",
  "bio18", "bio19"
)

goelo.climate <- crop(climate, extent(goelo))
goelo.climate <- mask(goelo.climate, goelo)

png("goelo-climate-annual-mean-temperature.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.climate$bio1 / 10, col=colors.temp, bg=NA, legend=FALSE, axes=FALSE, add=TRUE)
plot(goelo.borders, border=colors.darker_brown, add=TRUE)
#plot(goelo.climate$bio5 / 10, col=colors.temp, bg=NA, legend.only=TRUE, legend.shrink=1, legend.width=1,
#     smallplot=c(0.8, 0.85, 0.5, 0.97), add=TRUE)
plot(goelo.climate$bio1 / 10, col=colors.temp, bg=NA, legend.only=TRUE, legend.shrink=1, legend.width=1,
     smallplot=c(0.05, 0.1, 0.03, 0.5), add=TRUE)
dev.off()

png("goelo-climate-warmest-month.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.climate$bio5 / 10, col=colors.temp, bg=NA, legend=FALSE, axes=FALSE, add=TRUE)
plot(goelo.borders, border=colors.darker_brown, add=TRUE)
#plot(goelo.climate$bio5 / 10, col=colors.temp, bg=NA, legend.only=TRUE, legend.shrink=1, legend.width=1,
#     smallplot=c(0.8, 0.85, 0.5, 0.97), add=TRUE)
plot(goelo.climate$bio5 / 10, col=colors.temp, bg=NA, legend.only=TRUE, legend.shrink=1, legend.width=1,
     smallplot=c(0.05, 0.1, 0.03, 0.5), add=TRUE)
dev.off()

png("goelo-climate-coldest-month.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.climate$bio6 / 10, col=colors.temp, bg=NA, legend=FALSE, axes=FALSE, add=TRUE)
plot(goelo.borders, border=colors.darker_brown, add=TRUE)
#plot(goelo.climate$bio5 / 10, col=colors.temp, bg=NA, legend.only=TRUE, legend.shrink=1, legend.width=1,
#     smallplot=c(0.8, 0.85, 0.5, 0.97), add=TRUE)
plot(goelo.climate$bio6 / 10, col=colors.temp, bg=NA, legend.only=TRUE, legend.shrink=1, legend.width=1,
     smallplot=c(0.05, 0.1, 0.03, 0.5), add=TRUE)
dev.off()

png("goelo-climate-annual-precipitation.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.climate$bio12 / 10, col=colors.rain, bg=NA, legend=FALSE, axes=FALSE, add=TRUE)
plot(goelo.borders, border=colors.darker_brown, add=TRUE)
#plot(goelo.climate$bio5 / 10, col=colors.temp, bg=NA, legend.only=TRUE, legend.shrink=1, legend.width=1,
#     smallplot=c(0.8, 0.85, 0.5, 0.97), add=TRUE)
plot(goelo.climate$bio12, col=colors.rain, bg=NA, legend.only=TRUE, legend.shrink=1, legend.width=1,
     smallplot=c(0.05, 0.1, 0.03, 0.5), add=TRUE)
dev.off()

png("goelo-climate-wettest-month.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.climate$bio13, col=colors.rain, bg=NA, legend=FALSE, axes=FALSE, add=TRUE)
plot(goelo.borders, border=colors.darker_brown, add=TRUE)
#plot(goelo.climate$bio5 / 10, col=colors.temp, bg=NA, legend.only=TRUE, legend.shrink=1, legend.width=1,
#     smallplot=c(0.8, 0.85, 0.5, 0.97), add=TRUE)
plot(goelo.climate$bio13, col=colors.rain, bg=NA, legend.only=TRUE, legend.shrink=1, legend.width=1,
     smallplot=c(0.05, 0.1, 0.03, 0.5), add=TRUE)
dev.off()

png("goelo-climate-driest-month.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.climate$bio14, col=colors.rain, bg=NA, legend=FALSE, axes=FALSE, add=TRUE)
plot(goelo.borders, border=colors.darker_brown, add=TRUE)
#plot(goelo.climate$bio5 / 10, col=colors.temp, bg=NA, legend.only=TRUE, legend.shrink=1, legend.width=1,
#     smallplot=c(0.8, 0.85, 0.5, 0.97), add=TRUE)
plot(goelo.climate$bio14, col=colors.rain, bg=NA, legend.only=TRUE, legend.shrink=1, legend.width=1,
     smallplot=c(0.05, 0.1, 0.03, 0.5), add=TRUE)
dev.off()

# mean data
lapply(extract(goelo.climate$bio1 / 10, goelo.borders), FUN=mean, na.rm=TRUE)
lapply(extract(goelo.climate$bio5 / 10, goelo.borders), FUN=mean, na.rm=TRUE)
lapply(extract(goelo.climate$bio6 / 10, goelo.borders), FUN=mean, na.rm=TRUE)
lapply(extract(goelo.climate$bio12, goelo.borders), FUN=mean, na.rm=TRUE)
lapply(extract(goelo.climate$bio13, goelo.borders), FUN=mean, na.rm=TRUE)
lapply(extract(goelo.climate$bio14, goelo.borders), FUN=mean, na.rm=TRUE)


# Topography

rtm <- raster("srtm_36_03.tif")
goelo.srtm <- crop(srtm, extent(goelo.bg))
colors.terrain <- colorRampPalette(c(
  rgb(0,132,53,maxColorValue=255),
  rgb(51,204,0,maxColorValue=255),
  rgb(244,240,113,maxColorValue=255),
  rgb(244,189,69,maxColorValue=255),
  rgb(153,100,43,maxColorValue=255),
  rgb(255,255,255,maxColorValue=255)
))

png("goelo-srtm.png", width=goelo.width, height=goelo.height)
par(mar=c(0, 0, 0, 0), bg=colors.blue)
plot(goelo.bg, col=NA, border=NA,
     xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
plot(goelo.srtm, col=colors.terrain(64), bg=NA,
     zlim=c(1, cellStats(goelo.srtm, max)), legend=FALSE, axes=FALSE, add=TRUE)
plot(goelo.bg, col=NA, border=colors.darker_brown, add=TRUE)
plot(goelo.borders, border=colors.darker_brown, bg=colors.blue, add=TRUE)
plot(goelo.srtm, col=colors.terrain(64), bg=NA,
     zlim=c(1, cellStats(goelo.srtm, max)),
     legend.only=TRUE, legend.shrink=1, legend.width=1,
     smallplot=c(0.8, 0.85, 0.5, 0.97), add=TRUE)
dev.off()

lapply(extract(goelo.srtm, goelo.borders), FUN=mean, na.rm=TRUE)


# Communes

for (commune in goelo.communes) {
  filename <- sprintf("goelo-communes-%s.png", gsub("[- ]", "_", iconv(tolower(commune), to="ASCII//TRANSLIT")))
  png(filename, width=goelo.width, height=goelo.height)
  par(mar=c(0, 0, 0, 0), bg=colors.blue)
  plot(goelo.bg, col=colors.dark_brown, border=colors.darker_brown, xlim=goelo.xlim, ylim=goelo.ylim, asp=ratio, xaxs="i", yaxs="i")
  plot(goelo, col=colors.light_brown, border=colors.darker_brown, add=TRUE)
  plot(goelo[goelo$nom == commune, ], col=alpha("#E24D50", 0.4), border=alpha("#E24D50", 0.6), add=TRUE)
  dev.off()
}

data <- data.frame(
  goelo.communes,
  round(goelo$surf_m2 / 1e6, 2),
  goelo.insee$Population.municipale,
  round(goelo.insee$Population.municipale / (goelo$surf_m2 / 1e6), 0)
)
colnames(data) <- c("name", "surface", "population", "density")
