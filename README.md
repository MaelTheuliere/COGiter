
<!-- README.md is generated from README.Rmd. Please edit that file -->

# COGiter <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/MaelTheuliere/COGiter/workflows/R-CMD-check/badge.svg)](https://github.com/MaelTheuliere/COGiter/actions)
[![Codecov test
coverage](https://codecov.io/gh/MaelTheuliere/COGiter/branch/master/graph/badge.svg)](https://codecov.io/gh/MaelTheuliere/COGiter?branch=master)
<!-- badges: end -->

COGiter fournit des fonctions, des données et des fonds de carte pour
permettre des analyses territoriales sur les collectivités françaises.

## Installation

Installer le package depuis [github](https://github.com/)

``` r
remotes::install_github("MaelTheuliere/COGiter")
```

## A propos

Ce package R vise à mettre à disposition :

- les tables du Code Officiel Géographique (COG) au millésime 2025 de
  l’Insee en RData,
- une table de passage des COG historiques vers le COG millésimé 2025,
- les fonds de carte (couches géomatiques) correspondant au COG 2025,
- des fonctions d’aide à au passage de jeux de données vers le millésime
  du COG 2025,
- des fonctions de calculs d’agrégats aux différentes échelles
  territoriales,
- des fonctions de filtrage pour ne conserver que les territoires
  correspondant à une région ou un département (nouveau) dans une table
  ou une carte
- des [tables de passages
  communales](https://maeltheuliere.github.io/COGiter/reference/table_passage_communes_zonages.html)
  vers les zonages d’études de l’Insee.

Le tout avec des règles de nommage identiques pour faciliter les
appariements.

Le package [COGUGAISON](https://github.com/antuki/COGugaison) remplit en
partie la même fonction : il est plus fin en permettant de choisir les
millésimes de départ et d’arrivée du COG par exemple, mais est plutôt
destiné à traiter des fichiers nationaux. Par exemple, il ne comprend
pas les fonctionnalités de filtres aux contours d’une région, d’un
département ou d’un EPCI, et ne propose pas les fonds de cartes
associés.

Le présent package COGiter vise à répondre aux besoins de mise à jour du
COG des services connaissances déconcentrés, besoins initialement
identifiés par le [DREAL datalab Pays de la
Loire](http://www.pays-de-la-loire.developpement-durable.gouv.fr/dreal-centre-de-service-de-la-donnee-r1957.html).
Son maintien, année après année, est réalisé dans le cadre du [pôle
national
connaissance](https://greentechinnovation.fr/mission-connaissance/)
porté par la DREAL Pays de la Loire, intitulé Acter (Appui à la
connaissance territoriale reproductible), afin de mutualiser la solution
apportée.

## Les données sources

### Le COG Insee 2025

- <https://www.insee.fr/fr/information/2560452>

### Admin Express COG Carto 2025 (couche France entière)

- <https://geoservices.ign.fr/adminexpress#telechargementCogCarto>

Les surfaces proviennent d’une mesure opérée sur la couche commune de la
base Admin Express de l’IGN.

## Description du package

Voir la [page de
référence](https://maeltheuliere.github.io/COGiter/reference/index.html)

## Exemple d’usage

Voir la
[vignette](https://maeltheuliere.github.io/COGiter/articles/cogiter.html)
