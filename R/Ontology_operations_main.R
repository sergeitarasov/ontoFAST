
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
#require("ontologyIndex")
#require("pbapply")
#require("stringr")


# list of functions
#gt.name<-function(vec, onto, names=F)
#syn.extract<-function(onto.syn)
#distance.statement.match<-function(statement, onto.terms)
#distance.all.match<-function(statement, onto.obj, thresh=0.7, include_synonyms=F)
#onto.match<-function(onto.names, char.statement, onto.obj, min_set=T)
#
#
#

#' @title Returns names of ontology terms for ontology IDs
#' @description Returns names of ontology terms for ontology IDs
#' @param vec ID or IDs
#' @param onto ontology
#' @param names add element names
#' @return vector.
#' @examples
#' get_onto_name("HAO:0002272", hao.obo)

get_onto_name<-function(vec, onto, names=F){
  #name.vec=onto$name[onto$id%in%unlist(vec, use.names = FALSE)]
  name.vec=onto$name[c(na.omit(match(unlist(vec, use.names = FALSE), onto$id)))]
  if (names==F) {name.vec=unname(name.vec)}
  return(name.vec)
}



#' @title Returns IDs of ontology terms given terms' names
#' @description Returns IDs of ontology terms given terms' names
#' @param vec_name term names
#' @param onto ontology
#' @param names add element names
#' @return vector.
#' @examples
#' vec_name=c("ventral mesofurco-profurcal muscle", "anatomical entity")
#' get_onto_id(vec_name, hao.obo)

get_onto_id<-function(vec_name, ontology, names=F){
  match_vec<-match(unlist(vec_name, use.names = FALSE), ontology$name)
  ids=names(ontology$name)[match_vec]
  if (names==T) {names(ids)<-ontology$name[match_vec]}
  return(ids)
}
#vec_name=c("ventral mesofurco-profurcal muscle", "anatomical entity")
#get_onto_id(vec_name, hao.obo)





#' @title Associates synonym names with ontology terms
#' @description Extracts and parses synonyms from ontology to make them readable
#' @param ontology ontology_index object.
#' @param list_id theu ID of list element where synonyms are stored
#' @return The vector with ontology IDs and synonym names.
#' @examples
#' hao.obo$parsed_synonyms=syn_extract(hao.obo)


syn_extract<-function(ontology, list_id="synonym"){
  syn.raw=utils::stack(ontology[[list_id]])
  syn.raw=setNames(syn.raw$values, syn.raw$ind)
  syn.extr=stringr::str_match(syn.raw, pattern="\"(.*?)\"")[,2]
  names(syn.extr)<-names(syn.raw)
  return(syn.extr)
}
#hao.obo$parsed_synonyms=syn_extract(hao.obo)



#' @title Annotates character statement against all ontology terms useng grep
#' @description Matches character statement and returns most similar ontology terms
#' @param ontology ontology_index object.
#' @param char.statement character statement
#' @param use.synonyms using synonyms list during search. It has to be included in ontology,
#' see syn_extract()
#' @param min_set if TRUE eliminates higher order inferred ontology terms
#' @return The vector of matches ontology terms.
#' @examples
#' annot_char_grep(hao.obo, "Mola on right mandible")
#'

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
#annot_char_grep(hao_obo, "Mola on right mandible")





#' @title Annotates all character statement against all ontology terms
#' @description Matches character statement and returns most similar ontology terms using
#' grep and distance-based matching
#' @param ontology ontology_index object with character names (ontology$name_characters) and ids (ontology$id_characters)
#' @param use.synonyms using synonyms list during search. It has to be included in ontology,
#' see syn_extract()
#' @param min_set if TRUE eliminates higher order inferred ontology terms
#' @return The list of matched ontology terms and their charatcter ids.
#' @examples
#' grep_all_chars=annot_all_chars(hao.obo)
#'


annot_all_chars<-function(ontology, use.synonyms=TRUE, min_set=TRUE){
  print("Doing automatic annotation of characters with ontology terms...")
  annot_grepl=pbapply::pblapply(ontology$name_characters,
                       function(x) {annot_char_grep(ontology, x, use.synonyms, min_set)})
  names(annot_grepl)<-ontology$id_characters
  return(annot_grepl)
}
#grep_all_chars=annot_all_chars(hao.obo)


#' @title Compares  orignal annotationd vs. auto_annotations
#' @description Gives set relatinships
#' @param set_org manula annotations
#' @param set_annot auto-annotations
#' @return List.
#' @examples
#' comp.sets(set_org, set_annot)
#'

comp.sets<-function(set_org, set_annot){
  iden_org_found=list()
  if (length(intersect(set_org, set_annot))==0) {
    iden_org_found$iden="none"
    iden_org_found$org="na"
    iden_org_found$found="na"
    if (any(set_annot %in% get_ancestors(hao.obo, set_org))==T){iden_org_found$iden="original is finer" }
  } else if (setequal(set_org, set_annot)==T) {
    iden_org_found$iden="identical"
    iden_org_found$org="na"
    iden_org_found$found="na"
  } else if ((setequal(set_org, set_annot)==F)){
    iden_org_found$org=setdiff(set_org, set_annot)
    iden_org_found$found=setdiff(set_annot, set_org)
    iden_org_found$iden="partial"
    if (all(set_org %in% set_annot)==T) {iden_org_found$iden="original is subset"}
    if (all(set_annot %in% set_org)==T) {iden_org_found$iden="auto_annot is subset"}
  }
  return(iden_org_found)
}



#' @title Compares  orignal annotationd vs. auto_annotations and returns a vector of relatinships summarized by comp.sets()
#' @description Compares  orignal annotationd vs. auto_annotations and returns a vector of relatinships summarized by comp.sets().
#' @param ontology ontology with ontology$annot_characters
#' @param annot.auto annot.auto=grep_all_chars; from grep search
#' @return Vector with relatinships; each name is a character ID.
#' @examples
#' com_lists=compare_annot(ontology, grep_all_chars)

compare_annot<-function(ontology, annot.auto){
  out.list=c()
  for (i in seq_along(annot.auto)){
    char_id=ontology$id_characters[i]
    #out.list$ids[i]=i
    if (length(ontology$annot_characters[[char_id]])>0){
      comp.list=comp.sets(ontology$annot_characters[[char_id]], annot.auto[[char_id]])
      out.list=c(out.list, comp.list$iden)
      #out.list$identical[i]=comp.list$iden
      #out.list$original_missing[[i]]=comp.list$org
      #out.list$found_missing[[i]]=comp.list$found
    }
    else out.list=c(out.list, "character abscent")
  }
  #names(out.list)<-names(ontology$id_characters)
  return(out.list)
}
#length(which(com_lists=="none"))
#cbind(com_lists)

#' @title Compares  orignal annotationd vs. auto_annotations and returns a table of comparisons
#' @description Compares  orignal annotationd vs. auto_annotations and returns a table of comparisons. depends on compare_annot()
#' @param ontology ontology with ontology$annot_characters
#' @param grep_all_chars auto annotated chars from grep search
#' @return Table.
#' @examples
#' tb_compar=get_comp_table(hao.obo, grep_all_chars)

get_comp_table<-function(ontology, grep_all_chars){
  com_lists=compare_annot(ontology, grep_all_chars)
  dt_out=c()

  for (i in seq_along(ontology$id_characters)){
    c1=char_id=ontology$id_characters[i]
    c2=ontology$name_characters[i]
    c3=ontology$annot_characters[[char_id]]
    c4=com_lists[i]
    c5=grep_all_chars[[char_id]]

    c3=paste(paste(c3, get_onto_name(c3, ontology), sep="-(", collapse="); "), ")", sep="")
    c5=paste(paste(c5, get_onto_name(c5, ontology), sep="-(", collapse="); "), ")", sep="")

    dt_out=rbind(dt_out, c(c1,c2,c3,c4,c5))
  }
  colnames(dt_out)<-c("id", "statemet", "original_annot", "compar_results", "suggested_annot")
  return(dt_out)
}

# tb_compar=get_comp_table(hao.obo, grep_all_chars)
# write.csv(tb_compar, file="auto_annatotated4.csv", row.names = F)






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

