
# package Dependencies
devtools::use_package("pbapply", type = "Depends")
devtools::use_package("ontologyIndex", type = "Depends")
devtools::use_package("dplyr", type = "Depends")
devtools::use_package("shiny", type = "Depends")
devtools::use_package("shinydashboard", type = "Depends")
devtools::use_package("visNetwork", type = "Depends")
devtools::use_package("stringr", type = "Depends")
devtools::use_package("magrittr", type = "Depends")
devtools::use_package("devtools", type = "Depends")
#############

#' @title Get names for ontology IDs
#' @description Returns names of ontology terms for ontology IDs
#' @param vec ID or IDs
#' @param onto ontology
#' @param names add element names
#' @return vector.
#' @examples
#' get_onto_name("HAO:0002272", HAO)
#' @export

get_onto_name<-function(vec, onto, names=F){
  #name.vec=onto$name[onto$id%in%unlist(vec, use.names = FALSE)]
  name.vec=onto$name[c(na.omit(match(unlist(vec, use.names = FALSE), onto$id)))]
  if (names==F) {name.vec=unname(name.vec)}
  return(name.vec)
}

#sdfscf
#sfvdfv


#' @title Get IDs for ontology names
#' @description Returns IDs of ontology terms given terms' names
#' @param vec_name term names
#' @param onto ontology
#' @param names add element names
#' @return vector.
#' @examples
#' vec_name=c("ventral mesofurco-profurcal muscle", "anatomical entity")
#' get_onto_id(vec_name, HAO)
#' @export

get_onto_id<-function(vec_name, ontology, names=F){
  match_vec<-match(unlist(vec_name, use.names = FALSE), ontology$name)
  ids=names(ontology$name)[match_vec]
  if (names==T) {names(ids)<-ontology$name[match_vec]}
  return(ids)
}



#' @title Link synonyms with ontology terms
#' @description Extracts and parses synonyms from ontology to make them readable
#' @param ontology ontology_index object.
#' @param list_id theu ID of list element where synonyms are stored
#' @return The vector with ontology IDs and synonym names.
#' @examples
#' parsed_synonyms<-syn_extract(HAO)
#' @export

syn_extract<-function(ontology, list_id="synonym"){
  syn.raw=utils::stack(ontology[[list_id]])
  syn.raw=setNames(syn.raw$values, syn.raw$ind)
  syn.extr=stringr::str_match(syn.raw, pattern="\"(.*?)\"")[,2]
  names(syn.extr)<-names(syn.raw)
  return(syn.extr)
}


#' @title Annotate a character statement with ontology terms
#' @description Matches character statement and returns most similar ontology terms
#' @param ontology ontology_index object.
#' @param char.statement character statement
#' @param use.synonyms using synonyms list during search. It has to be included in ontology,
#' see syn_extract()
#' @param min_set if TRUE eliminates higher order inferred ontology terms
#' @return The vector of matches ontology terms.
#' @examples
#' annot_char_grep(HAO, "Mola on right mandible")
#' @export

annot_char_grep<-function(ontology, char.statement, use.synonyms=TRUE, min_set=TRUE){
  if (use.synonyms) search_terms=c(ontology$name, ontology$parsed_synonyms)
  if (!use.synonyms) search_terms=ontology$name
  #onto.names=c(ontology$name, ontology$parsed_syns)
  terms=search_terms[vapply(search_terms,
                          function(x) {grepl(x, char.statement, ignore.case = T)}, logical(1))]
  if (min_set==T){
    terms=ontologyIndex::minimal_set(ontology, names(terms))
  }
  return(terms)
}



#' @title Annotate all character statements with ontology terms
#' @description Matches character statement and returns most similar ontology terms using
#' grep and distance-based matching
#' @param ontology ontology_index object with character names (ontology$name_characters) and ids (ontology$id_characters)
#' @param use.synonyms using synonyms list during search. It has to be included in ontology,
#' see syn_extract()
#' @param min_set if TRUE eliminates higher order inferred ontology terms
#' @return The list of matched ontology terms and their charatcter ids.
#' @examples
#' #getting ontology
#' ontology<-HAO
#' #parsing synonyms
#' ontology$parsed_synonyms<-syn_extract(HAO)
#' # reading in characters
#' char_et_states<-Sharkey_2011
#' # embedding characters and character ids into ontology
#' id_characters<-paste("CHAR:",c(1:392), sep="")
#' name_characters<-char_et_states[,1]
#' names(name_characters)<-id_characters
#' ontology$name_characters<-name_characters
#' ontology$id_characters<-id_characters
#'
#' # running annotations
#' auto_annotations<-annot_all_chars(ontology)
#' @export

annot_all_chars<-function(ontology, use.synonyms=TRUE, min_set=TRUE){
  print("Doing automatic annotation of characters with ontology terms...")
  annot_grepl=pbapply::pblapply(ontology$name_characters,
                       function(x) {annot_char_grep(ontology, x, use.synonyms, min_set)})
  names(annot_grepl)<-ontology$id_characters
  return(annot_grepl)
}



#' Hymenoptera character statements
#'
#' A table of characters and characters states from
#' Hymenoptera character matrix (Sharkey et al., 2011: Cladistics). The table contains 392 character statements.
#'
#' @docType data
#'
#' @format A data table with 392 rows; each row is a character statement with character states:
#' \describe{
#'   \item{CHARACTER STATEMENTS:}{Charater statements}
#'   \item{STATES}{sates of the character}
#'   \item{STATES}{....}
#' }
#'
#' @references Sharkey, M.J., et al. 2011. Phylogenetic relationships among superfamilies of Hymenoptera. Cladistics 28(1), 80-112.
#' (\href{http://onlinelibrary.wiley.com/doi/10.1111/j.1096-0031.2011.00366.x/full}{Read})
#'
#' @examples
#' chars<-Sharkey_2011
"Sharkey_2011"



#' Hymenoptera Anatomy Ontology (HAO)
#'
#' Anatomy ontology of Hymenoptera. This ontology was imported into R using get_OBO() function from ontologyIndex packages (see the examples).
#'
#' @docType data
#'
#' @format List containing various ontological relationships and terms.
#'
#' @references Yoder MJ, Mikó I, Seltmann KC, Bertone MA, Deans AR. 2010. A Gross Anatomy Ontology for Hymenoptera. PLoS ONE 5 (12): e15991.
#' (\href{http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0015991}{Read})
#'
#' \href{http://portal.hymao.org/projects/32/public/ontology/}{Hymenoptera Anatomy Ontology Portal}
#'
#' @examples
#' ontology<-HAO
#' # you can also parse the original .obo file
#' ontology<-get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),
#' extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))
"HAO"

