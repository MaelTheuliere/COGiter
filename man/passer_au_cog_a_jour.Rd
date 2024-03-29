% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/passer_au_cog_a_jour.R
\name{passer_au_cog_a_jour}
\alias{passer_au_cog_a_jour}
\title{Fonction de passage d'une table de données à la commune vers le millésime le plus récent du COG}
\usage{
passer_au_cog_a_jour(
  .data,
  code_commune = DEPCOM,
  aggrege = TRUE,
  garder_info_supra = TRUE,
  na.rm = FALSE
)
}
\arguments{
\item{.data}{la table de données à convertir}

\item{code_commune}{le nom de la variable contenant le code commune sur 5 caractères}

\item{aggrege}{booléen TRUE si on souhaite réaggréger les colonnes numériques sur la nouvelle carte communale}

\item{garder_info_supra}{booléen TRUE si on souhaite garder les informations sur les territoires supra des communes}

\item{na.rm}{argument passé à la fonction d'agrégation (sum), na.rm=FALSE par défaut}
}
\value{
Renvoie la table de données convertie pour les codes communes valide en entrée
}
\description{
Cette fonction vous permet de convertir vos dataframes ayant une variable correspondant au code commune Insee vers le COG le plus récent.
Champ : codes communes de la France métropolitaine et des DROM
}
\examples{
pop2015_a_jour <- passer_au_cog_a_jour(pop2015, code_commune = DEPCOM,
                                       aggrege = TRUE, garder_info_supra = FALSE)

}
