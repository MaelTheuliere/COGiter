% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filtrer_cog.R
\name{filtrer_cog}
\alias{filtrer_cog}
\title{Filtrer une fichier du cog sur un sous ensemble du territoire}
\usage{
filtrer_cog(
  .data,
  depcom = NULL,
  epci = NULL,
  dep = NULL,
  reg = NULL,
  garder_supra = "non"
)
}
\arguments{
\item{.data}{la table de données a filtrer}

\item{depcom}{la commune sur laquelle filtrer les données}

\item{epci}{l'epci sur lequel filtrer les données}

\item{dep}{le departement sur lequel filtrer les données}

\item{reg}{la region sur laquelle filtrer les données}

\item{garder_supra}{">" si on souhaite garder les territoires supra, ">=" si on souhaite garder les territoires suppra et du même niveau que celui sélectionné}
}
\value{
Renvoie une table de donnees filtrées
}
\description{
Filtrer une fichier du cog sur un sous ensemble du territoire
}
