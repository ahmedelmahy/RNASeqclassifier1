% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_selection_by_information.R
\name{filter_step}
\alias{filter_step}
\title{Stepwise feature selection}
\usage{
filter_step(counts, class, direction = "backward")
}
\arguments{
\item{counts}{matrix}

\item{class}{vector}

\item{direction}{of feature selection default is backwards}
}
\value{
a vector of selected genes
}
\description{
Stepwise feature selection
}
