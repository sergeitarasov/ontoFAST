% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ontology_operations_main.R
\name{syn_extract}
\alias{syn_extract}
\title{Link synonyms with ontology terms}
\usage{
syn_extract(ontology, list_id = "synonym")
}
\arguments{
\item{ontology}{ontology_index object.}

\item{list_id}{ID of list where synonyms are stored}
}
\value{
vector of ontology IDs and synonym names.
}
\description{
Extracts and parses synonyms from ontology to make them readable and searchable
}
\examples{
data(HAO)
syn_extract(HAO)
}
