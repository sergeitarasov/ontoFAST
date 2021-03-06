% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ontology_operations_main.R
\docType{data}
\name{Scarab}
\alias{Scarab}
\title{A modified Hymenoptera Anatomy Ontology (HAO) to accommodate anatomy of dung beetles}
\format{List containing various ontological relationships and terms.}
\usage{
Scarab
}
\description{
Anatomy ontology of Hymenoptera + Dung beetles. This ontology was imported into R using get_OBO() function from ontologyIndex packages (see the examples). The added terms for dung beetle anatomy have prefix "SCR".
}
\examples{
data(Scarab)
#you can also parse the original .obo file
get_OBO(system.file("data_onto", "HAO4scarabs.obo", package = "ontoFAST"),
extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))
}
\references{
Yoder MJ, Mikó I, Seltmann KC, Bertone MA, Deans AR. 2010. A Gross Anatomy Ontology for Hymenoptera. PLoS ONE 5 (12): e15991.
(\href{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0015991}{Read})

\href{http://portal.hymao.org/projects/32/public/ontology/}{Hymenoptera Anatomy Ontology Portal}
}
\keyword{datasets}
