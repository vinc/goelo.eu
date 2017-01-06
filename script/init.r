install.packages("raster")
install.packages("rgdal")
install.packages("RColorBrewer")
install.packages("ncdf4")
install.packages("stringi")
install.packages("gfcanalysis")
install.packages("maptools")
install.packages("viridis")
install.packages("scales")
install.packages("gdata")
install.packages("rgeos")
install.packages("downloader")

library(downloader)

download("http://srtm.csi.cgiar.org/SRT-ZIP/SRTM_V41/SRTM_Data_GeoTiff/srtm_36_03.ZIP", "srtm_36_03.ZIP")
unzip("srtm_36_03.ZIP")

download("http://services.sandre.eaufrance.fr/telechargement/geo/ETH/BDCarthage/FXX/2014/arcgis/Regions/53_BRETAGNE/COURS_D_EAU.shp.zip", destfile="rivers_brittany.zip")
unzip("rivers_brittany.zip")

download("https://inpn.mnhn.fr/docs/Shape/zps.zip", destfile="zps.zip")
unzip("zps.zip")

download("https://inpn.mnhn.fr/docs/Shape/sic.zip", destfile="sic.zip")
unzip("sic.zip")

download("http://osm13.openstreetmap.fr/~cquest/openfla/export/communes-20160614-shp.zip", destfile="osm_communes.zip")
unzip("osm_communes.zip")

download("https://www.insee.fr/fr/statistiques/fichier/2387611/dep22.xls", destfile="dep22.xls")

download("http://osm13.openstreetmap.fr/~cquest/openfla/export/communes-20150101-5m-shp.zip", destfile="osm_communes_5m.zip")
unzip("osm_communes_5m.zip")

download("http://osm13.openstreetmap.fr/~cquest/openfla/export/communes-20150101-50m-shp.zip", destfile="osm_communes_50m.zip")
unzip("osm_communes_50m.zip")


download("https://inpn.mnhn.fr/docs/Shape/znieff1.zip", destfile="znieff1.zip")
unzip("znieff1.zip")

download("https://inpn.mnhn.fr/docs/Shape/znieff2.zip", destfile="znieff2.zip")
unzip("znieff2.zip")

download("https://inpn.mnhn.fr/docs/Shape/cdl.zip", destfile="cdl.zip")
unzip("cdl.zip")
