
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

Installer le package depuis [github](https://github.com/) ou
[gitlab](https://gitlab.com)

``` r
remotes::install_github("MaelTheuliere/COGiter")
remotes::install_gitlab("dreal-datalab/COGiter")
```

## A propos

Ce package R vise à mettre à disposition :

- les tables du COG 2023 de l’Insee en RData,
- une table de passage des COG historiques vers le COG millésimé 2023,
- les couches géomatiques correspondant au COG 2023,
- des fonctions d’aide à au passage de jeux de données vers le millésime
  du COG 2023,  
- des fonctions de calculs d’agrégats aux différentes échelles
  territoriales,  
- des fonctions d’aide à la sélection des différents fond de cartes
  nécessaire à la mise en page de cartes statistiques.

Le tout avec des règles de nommage identiques pour faciliter les
appariements.

Le package [COGUGAISON](https://github.com/antuki/COGugaison) rempli en
partie la même fonction et nous vous invitons à privilégier son
utilisation. Ce présent package visant à répondre à des besoins
spécifiques du [DREAL datalab Pays de la
Loire](http://www.pays-de-la-loire.developpement-durable.gouv.fr/dreal-centre-de-service-de-la-donnee-r1957.html)
et est par ailleurs non stabilisé.

## Les données sources

### Le COG Insee 2023

- <https://www.insee.fr/fr/information/2560452>

### Admin Express COG Carto 2023 (couche France entière)

- <https://geoservices.ign.fr/adminexpress#telechargementCogCarto>

## Description du package

Voir la [page de
référence](https://maeltheuliere.github.io/COGiter/reference/index.html)

## Exemple d’usage

Voir la
[vignette](https://maeltheuliere.github.io/COGiter/articles/cogiter.html)
