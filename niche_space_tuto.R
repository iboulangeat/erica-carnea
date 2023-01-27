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

dens_obs = sampling_density(obs_env, nspace, R= 100) # obs_env est une dataframe des variables environnementales (même noms que les layers du SpatRast) pour les observations ou prédictions de présences

plot_niche(pca=nspace$pca, density.pts=dens_obs)
# autres options:
# palette de couleur:  colo.pts = c("white", colorRampPalette(c("lightblue", "violet", "darkred"), space="Lab")(5)),
# titre de la légende :  tit.legend = "probability \n density",
# valeurs de break pour les couleurs:  at.pts = c(-0.1, 0.001, 0.2, 0.4, 0.6, 0.8, 1 ),
# valeurs de break pour les couleurs dans la barre de légende: at.scalePts= 0:5,
# taille des flèches des variables de la pca: w.arrows=5,
# taille des labels des variables de la pca:  clab.arrows=2, 
# selection des variables de la pca à afficher : col.select = 1:ncol(pca$tab)
