% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ontology_operations_main.R
\name{annot_all_chars}
\alias{annot_all_chars}
\title{Annotate all character statements with ontology terms}
\usage{
annot_all_chars(ontology, use.synonyms = TRUE, min_set = TRUE)
}
\arguments{
\item{ontology}{ontology_index object with character names (ontology$name_characters) and ids (ontology$id_characters)}

\item{use.synonyms}{using synonyms list during search. It has to be included in ontology,
see syn_extract()}

\item{min_set}{if TRUE eliminates higher order inferred ontology terms}
}
\value{
The list of matched ontology terms and their character ids.
}
\description{
Matches character statement and returns most similar ontology terms using
grep and distance-based matching
}
\examples{
#getting ontology
data(HAO)
data(Sharkey_2011)
ontology<-HAO
#parsing synonyms
ontology$parsed_synonyms<-syn_extract(HAO)
# reading in characters
char_et_states<-Sharkey_2011
# embedding characters and character ids into ontology
id_characters<-paste("CHAR:",c(1:392), sep="")
name_characters<-char_et_states[,1]
names(name_characters)<-id_characters
ontology$name_characters<-name_characters
ontology$id_characters<-id_characters
# running annotations
\donttest{
auto_annotations<-annot_all_chars(ontology)
}
}
