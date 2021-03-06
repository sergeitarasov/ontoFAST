#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage dashboardSidebar box
#' @importFrom utils head stack
#' @importFrom visNetwork renderVisNetwork visIgraphLayout visLayout visLegend visNetwork visNetworkOutput visNodes visOptions
#' @importFrom ontologyIndex minimal_set get_OBO get_ancestors get_descendants
#' @importFrom pbapply pblapply
#' @importFrom stats setNames na.omit
#' @importFrom stringr str_match
#' @importFrom shiny a actionButton tags runApp stopApp checkboxGroupInput column fluidRow h2 h3 h5 hr HTML icon incProgress isolate observe observeEvent renderText renderUI selectInput selectizeInput shinyApp textInput uiOutput updateCheckboxGroupInput updateSelectizeInput updateTextInput verbatimTextOutput withProgress
#' @importFrom dplyr %>%
#' @importFrom plyr ldply
#' @importFrom sunburstR sunburst
#' @importFrom usethis use_package


#utils::globalVariables("shiny_in", package="ontoFAST")
utils::globalVariables("ontofast", package="ontoFAST")

usethis::use_package("shiny", type = "Depends")
usethis::use_package("ontologyIndex", type = "Depends")


# devtools::document()
# devtools::build()
# devtools::install()
# devtools::check()



#' @title Get names for ontology IDs
#' @description Returns names of ontology terms for ontology IDs
#' @param vec ID or a vector of IDs
#' @param onto ontology
#' @param names use element name
#' @return vector of names.
#' @examples
#' data(HAO)
#' get_onto_name("HAO:0002272", HAO)
#' @export

get_onto_name<-function(vec, onto, names=FALSE){
  #name.vec=onto$name[onto$id%in%unlist(vec, use.names = FALSE)]
  #name.vec=onto$name[c(na.omit(match(unlist(vec, use.names = FALSE), onto$id)))]
  name.vec=onto$name[c(match(unlist(vec, use.names = FALSE), onto$id))]
  if (names==FALSE) {name.vec=unname(name.vec)}
  return(name.vec)
}


#' @title Get IDs for ontology names
#' @description Returns IDs of ontology terms given terms' names
#' @param vec_name names od terms
#' @param ontology ontology
#' @param names use element name
#' @return vector of IDs.
#' @examples
#' data(HAO)
#' vec_name=c("ventral mesofurco-profurcal muscle", "anatomical entity")
#' get_onto_id(vec_name, HAO)
#' @export

get_onto_id<-function(vec_name, ontology, names=FALSE){
  match_vec<-match(unlist(vec_name, use.names = FALSE), ontology$name)
  ids=names(ontology$name)[match_vec]
  if (names==TRUE) {names(ids)<-ontology$name[match_vec]}
  return(ids)
}



#' @title Link synonyms with ontology terms
#' @description Extracts and parses synonyms from ontology to make them readable and searchable
#' @param ontology ontology_index object.
#' @param list_id ID of list where synonyms are stored
#' @return vector of ontology IDs and synonym names.
#' @examples
#' data(HAO)
#' syn_extract(HAO)
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
#' @param ontology ontology.
#' @param char.statement character statement
#' @param use.synonyms if TRUE then the synonyms are used during search. The synonyms have to be included in the ontology using
#' syn_extract() function
#' @param min_set if TRUE eliminates higher order inferred ontology terms
#' @return The vector of matches ontology terms.
#' @examples
#' data(HAO)
#' annot_char_grep(HAO, "Mola on right mandible")
#' @export

annot_char_grep<-function(ontology, char.statement, use.synonyms=TRUE, min_set=TRUE){
  if (use.synonyms) search_terms=c(ontology$name, ontology$parsed_synonyms)
  if (!use.synonyms) search_terms=ontology$name
  #onto.names=c(ontology$name, ontology$parsed_syns)
  terms=search_terms[vapply(search_terms,
                          function(x) {grepl(x, char.statement, ignore.case = TRUE)}, logical(1))]
  if (min_set==TRUE){
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
#' @return The list of matched ontology terms and their character ids.
#' @examples
#' #getting ontology
#' data(HAO)
#' data(Sharkey_2011)
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
#' # running annotations
#' \donttest{
#' auto_annotations<-annot_all_chars(ontology)
#' }
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
#' A table of characters and character states from
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
#' (\href{https://onlinelibrary.wiley.com/doi/full/10.1111/j.1096-0031.2011.00366.x}{Read})
#'
#' @examples
#' data(Sharkey_2011)
#' # read .csv file directly
#' char_et_states<-read.csv(system.file("data_onto", "Sharkey_2011.csv",
#' package = "ontoFAST"), header=TRUE,  stringsAsFactors = FALSE, na.strings = "")
"Sharkey_2011"



#' Hymenoptera Anatomy Ontology (HAO)
#'
#' Anatomy ontology of Hymenoptera. This ontology was imported into R using get_OBO() function from ontologyIndex package (see the examples).
#'
#' @docType data
#'
#' @format List containing various ontological relationships and terms.
#'
#' @references Yoder MJ, Mikó I, Seltmann KC, Bertone MA, Deans AR. 2010. A Gross Anatomy Ontology for Hymenoptera. PLoS ONE 5 (12): e15991.
#' (\href{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0015991}{Read})
#'
#' \href{http://portal.hymao.org/projects/32/public/ontology/}{Hymenoptera Anatomy Ontology Portal}
#'
#' @examples
#' data(HAO)
#' # you can also parse the original .obo file
#' get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),
#' extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))
"HAO"


#' A modified Hymenoptera Anatomy Ontology (HAO) to accommodate anatomy of dung beetles
#'
#' Anatomy ontology of Hymenoptera + Dung beetles. This ontology was imported into R using get_OBO() function from ontologyIndex packages (see the examples). The added terms for dung beetle anatomy have prefix "SCR".
#'
#' @docType data
#'
#' @format List containing various ontological relationships and terms.
#'
#' @references Yoder MJ, Mikó I, Seltmann KC, Bertone MA, Deans AR. 2010. A Gross Anatomy Ontology for Hymenoptera. PLoS ONE 5 (12): e15991.
#' (\href{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0015991}{Read})
#'
#' \href{http://portal.hymao.org/projects/32/public/ontology/}{Hymenoptera Anatomy Ontology Portal}
#'
#' @examples
#' data(Scarab)
#' #you can also parse the original .obo file
#' get_OBO(system.file("data_onto", "HAO4scarabs.obo", package = "ontoFAST"),
#' extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))
"Scarab"

#' Ontology terms to exclude for sunburst plot
#'
#' List of ontology terms from Hymenoptera Anatomy Ontology that can be considered
#' redundant and need to be excluded from some operations.
#'
#' @docType data
#'
#' @format List containing ontological terms.
#'
#'
#' @examples
#' exclude_terms
"exclude_terms"



#' Hymenoptera characters annotated with ontology terms
#'
#' List of character IDs and their ontology annotations. The characters are from Hymenoptera phylogeny (Sharkey et al., 2011: Cladistics).
#' The ontology annotations are from Hymenoptera Anatomy Ontology (HAO).
#'
#' @docType data
#'
#' @format List containing characters and ontology terms.
#'
#' @references Sharkey, M.J., et al. 2011. Phylogenetic relationships among superfamilies of Hymenoptera. Cladistics 28(1), 80-112.
#' (\href{https://onlinelibrary.wiley.com/doi/full/10.1111/j.1096-0031.2011.00366.x}{Read})
#'
#' @examples
#' Sharkey_2011_annot
"Sharkey_2011_annot"



#' Dung beetle characters annotated with ontology terms
#'
#' List of character IDs and their ontology annotations. All 232 characters are from Scarabaeinae phylogeny (Tarasov, 2017: Zootaxa).
#' The ontology annotations are from a modified version Hymenoptera Anatomy Ontology (HAO) that was enriched with additional terms (SCR:) to accommodate anatomy of the dung beetles.
#'
#' @docType data
#'
#' @format List containing characters and ontology terms.
#'
#' @references Tarasov, S., 2017. A cybertaxonomic revision of the new dung beetle tribe Parachoriini (Coleoptera: Scarabaeidae: Scarabaeinae) and its phylogenetic assessment using molecular and morphological data. Zootaxa, 4329(2), pp.101-149.
#' (\href{https://www.mapress.com/j/zt/article/view/zootaxa.4329.2.1}{Read})
#'
#' @examples
#' Tarasov_2017_annot
"Tarasov_2017_annot"

