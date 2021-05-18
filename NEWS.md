# Version de développement 

- Mise à jour COG 2021
- Ajout logo
- Amélioration documentation des datasets
- Ajout fonctions pour traiter la correspondances des territoire
- Ajout fonctions pour savoir si un code commune/epci/departements/regions est valide
- Ajout fonction ajouter_typologie qui permet de créer des aggrégats par typologie de territoire et type de territoire (epci, départements, région...)
- Ajout table des arrondissements de Paris, Lyon et Marseille

# COGiter 0.0.7

- Correction bug passer_au_cog_a_jour suite à une regression
- Explicitation de la gestion des NA dans cogifier()

## interne

- ajout de tests


# COGiter 0.0.6

- Correction filtrer_cog_geo : rajout d'un sf_buffer à 0 pour nettoyer la géometrie après st_crop()
- Ajout de la variable nature_epci dans liste_zone pour les epci

# COGiter 0.0.5

- correction bug sur `passer_au_cog_a_jour()` avec le passage à la nouvelle table de passage.

# COGiter 0.0.3

- Intégration des données 2020
- Mise en place d'une fonction `filtrer_cog_geo()` qui permet de filtrer les fonds de cartes
- Modification du tri du code epci de la table epci trié maintenant en fonction de la nature de l'epci
- La fonction `passer_au_cog_a_jour()` conserve maintenant les groupes pour les dataframes groupés.

# COGiter 0.0.2

- rajout de deux agrégats possibles pour la cogification : France de province et Départements et régions d'outre-mer

# COGiter 0.0.1.9000

- bug fix sur liste_zone
- correction sur les communes hors epci dans les tables communes et communes_info_supra
- précision millésime COG des tables pinel et abc

# COGiter 0.0.1

## Nouvelles fonctionnalités

- Ajout des données du COG 2019 et admin express 2019
- Ajout d'une fonction ajouter_zonage pour ajouter un regrouppement de communes spécifique après cogification.

- Ajout du site pkgdown

- Ajout d'une vignette d'exemples d'usage

# COGiter 0.0.0.9002

## Nouvelles fonctionnalités

* Nouvelles cartes avec DOM
* Nouvelles fonctions filtrer_cog pour filtrer une sortie de cogifier sur un sous zonage
* Nouvelles fonction cog_df_to_list et cog_list_to_df pour passer d'un format liste à un format dataframe pour la sortie de cogifier
* Nouvelles options dans cogifier pour sortir les totaux sous forme de liste de dataframe par type de territoire

## Bug fix

* Bug fix sur les communes hors epci

# COGiter 0.0.0.9001

* Added a `NEWS.md` file to track changes to the package.
* Initial version
