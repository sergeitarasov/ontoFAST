#' @title Associates synonym names with ontology terms
#' @description Extracts and parses synonyms from ontology to make them readable
#' @param ontology ontology_index object.
#' @param list_id theu ID of list element where synonyms are stored
#' @return The vector with ontology IDs and synonym names.
#' @examples
#' hao.obo$parsed_synonyms=syn_extract(hao.obo)
#'

syn_extract<-function(ontology, list_id="synonym"){
  syn.raw=utils::stack(ontology[[list_id]])
  syn.raw=setNames(syn.raw$values, syn.raw$ind)
  syn.extr=str_match(syn.raw, pattern="\"(.*?)\"")[,2]
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
# former onto.match()

annot_char_grep<-function(ontology, char.statement, use.synonyms=TRUE, min_set=TRUE){
  if (use.synonyms) search_terms=c(ontology$name, ontology$parsed_synonyms)
  if (!use.synonyms) search_terms=ontology$name
  terms=onto.names[vapply(search_terms,
                          function(x) {grepl(x, char.statement, ignore.case = T)}, logical(1))]
  if (min_set==T){
    terms=ontologyIndex::minimal_set(ontology, names(terms))
  }
  return(terms)
}
#annot_char_grep(hao.obo, "Mola on right mandible")



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
#hao.obo$name_characters=char.list
#ontology=hao.obo
#annot.list=lapply(char.list, function(x) {onto.match(c(hao.obo$name, hao.syns), x, hao.obo, min_set = T)})
#!!! INCLUDE DISTANCE METHODs

annot_all_chars<-function(ontology, use.synonyms=TRUE, min_set=TRUE){
  print("Doing grep search...")
  annot_grepl=pblapply(ontology$name_characters,
                       function(x) {annot_char_grep(ontology, x, use.synonyms, min_set)})
  names(annot_grepl)<-ontology$id_characters
  return(annot_grepl)
}
#grep_all_chars=annot_all_chars(hao.obo)


#' @title Compares  orignal annotationd vs. auto_annotations
#' @description Gives set relatinships
#' @param set_org
#' @param set_annot
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







