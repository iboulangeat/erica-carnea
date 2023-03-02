---
title: "Distribution spatiale de la bruyère des neiges"
author: "Isabelle Boulangeat"
date: "02/03/23"
output:
  html_document:
    keep_md: yes
    variant: gfm
editor_options:
  chunk_output_type: console
always_allow_html: yes
---




# Données utilisées pour la construction du modèle

## Données de présence de Erica Carnea

En tout, il y a 177 observations réparties dans la zone et dans le Mercantour.

![](rapport_files/figure-html/visudata-1.png)<!-- -->


## Variables explicatives

Nous avons sélectionné un ensemble de variables qui prenant en compte la saisonalité du climat, très importante en zone de montagne (Körner 2003), ainsi que la topographie et la pente, et les informations disponibles les plus pertinentes sur le sol. La proportion de sable permet d'informer sur les affinités calcaire-silice et l'indice de végétation de médiane annuelle du NDVI permet d'intégrer plusieurs propriétés du sol qui sont sous-jacentes au degré de végétalisation et à sa durée dans l'année.

Liste complète des variables explicatives:

- slope
- northing and easting
- Convergence Index
- bio2 : Mean Diurnal Range (Mean of monthly (max temp - min temp))
- bio3 : Isothermality (BIO2/BIO7) (×100)
- bio4 : Temperature Seasonality (standard deviation ×100)
- bio5 : Max Temperature of Warmest Month
- bio6 : Min Temperature of Coldest Month
- bio12 : Annual Precipitation
- bio15 :  Precipitation Seasonality (Coefficient of Variation)
- gdd0 : heat sum of all days above the 0°C temperature accumulated over 1 year
- gsl : Length of the growing season (TREELIM)
- gsp : precipitation sum accumulated on all days during the growing season (TREELIM)
- scd : Number of days with snowcover (TREELIM)
- sand proportion 
- ndvi : yearly median, average between 2000 and 2020

### sources:

> Digital Elevation Model at 30m resolution from NASA Shuttle Radar Topography Mission Global 1 arc second V003

> MODIS product MOD13Q1.061 Terra Vegetation Indices 16-Day Global 250m

> Soil sand from European Soil DataBase

>> Hiederer, R. 2013. Mapping Soil Typologies - Spatial Decision Support Applied to European Soil Database. Luxembourg: Publications Office of the European Union – 2013 – 147pp. – EUR25932EN Scientific and Technical Research series, ISSN 1831-9424, doi:10.2788/87286


# Modèle de distribution

## Calibration

Pour calibrer le modèle nous avons généré environ 10 fois plus de pseudo-absences que de présences observées, aléatoirement dans la zone d'étude au dessus de 300m.

Nous avons ensuite utilisé un algorithme de Random Forest et pour l'évaluation une validation croisée de 5 fois.

Les packages `randomForest` et `mecofun` ont été utilisés pour cela.



## Evaluation

![](rapport_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

A cette échelle de calibration le modèle est très bien évalué (85.6% de déviance expliquée).

La variable BIO15 (precipitations annuelles) ressort comme la plus importante. Ensuite un ensemble de variables contribuent au modèle: le sol (ndvi et sand), les variables de saisonalité (BIO2, BIO3 et BIO12), et les précipitations durant la saison de végétation (gsp).

## Projection cartographique

https://iboulangeat.github.io/erica-carnea/carte.html
