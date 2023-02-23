require(sp)
require(dplyr)
require(ggplot2)
require(sf)
require(raster)
require(rgdal)
require(terra)

source("niche_space_fct.R")

nspace = niche_space(rast_envtopo) # rastenvtopo est un SpatRast de la library terra avec plusieurs layers environnementaux

inertia.dudi(nspace$pca)$tot # permet de voir les contributions

dens_bg = densitePts2(focalPts=nspace$li[,1:2], backgroundPts=nspace$li[,1:2],  R=100, scale = FALSE) # permet d'avoir le background (densité de pixels dans le plan de l'ACP)


# construire le raster d"observations
sites_proj = spTransform(sites_all, crs(rast_envTot)) # verifier les projections
index = terra::extract(rast_envTot, coordinates(sites_proj),  cells=TRUE)

rastSp = rast(rast_envTot, nlyrs = 1, vals = 0)
rastSp[index[,"cell"]] = 1
fac.sp = rastSp[]

dens_obs = densitePts2(focalPts=nspace$li[which(fac.sp!="0"),1:2], backgroundPts=nspace$li[,1:2], R=100, density.bg = dens_bg, scale = TRUE) # fac.sp correspond au vecteur qui dit quels pixels correspondent aux observations ou prédictions d'observations


plot_niche(pca=nspace$pca, density.pts=dens_obs)
# autres options:
# palette de couleur:  colo.pts = c("white", colorRampPalette(c("lightblue", "violet", "darkred"), space="Lab")(5)),
# titre de la légende :  tit.legend = "probability \n density",
# valeurs de break pour les couleurs:  at.pts = c(-0.1, 0.001, 0.2, 0.4, 0.6, 0.8, 1 ),
# valeurs de break pour les couleurs dans la barre de légende: at.scalePts= 0:6,
# taille des flèches des variables de la pca: w.arrows=5,
# taille des labels des variables de la pca:  clab.arrows=2, 
# selection des variables de la pca à afficher : col.select = 1:ncol(pca$tab)
# on peut utiliser un tableau de OMI au lieu de PCA, dans ce cas, omi = FALSE

