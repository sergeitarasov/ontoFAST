#list to table
#plyr::ldply(annotated.char.list, rbind) #list to table
#################################################### BASIC LIST FUNCTIOINS
#' @title Converts table of charater annotations to list
#' @description Takes table where each row consists of charcter number and ontology annotations and returns a list.
#' Each character is assigned its own ID CHAR:XXXX
#' @param table A character table with annotations.
#' @param id_col A column ID corresponding to character
#' @param descendants_cols IDs of columns corresponding to character annotations
#' @return The list.
#' @examples
#' annotated.char.list=table2list(char.dt, 1, 3:9)

table2list<-function(table, id_col=c(1), descendants_cols=c(3:9)){
annotated.char.list=list()
for (i in 1:nrow(table)) {
  annotated.char.list[[i]]=c(na.omit(as.character(table[i,descendants_cols])))
}
names(annotated.char.list)<-paste("CHAR:", table[,id_col], sep="")
return(annotated.char.list)
}
#annotated.char.list=table2list(char.dt, 1, 3:9)



#' @title Converts list of charater annotations to two-column edge matrix
#' @description Takes list of charater annotations amd creates an edge matrix comprising two columns: from and to.
#' @param annotated.char.list Character list with ontology annotations.
#' @param col_order_inverse The default creates the first columns consisting if character IDs and the second columns consisting of ontology annatotaions.
#' The inverse order changes the columns order.
#' @return Two-columns matrix.
#' @examples
#' list2edges(annotated.char.list, col_order_inverse=F)

list2edges<-function(annotated.char.list, col_order_inverse=F){
annotated.vec=setNames(unlist(annotated.char.list, use.names=F),rep(names(annotated.char.list), lengths(annotated.char.list)))
if (col_order_inverse==T){
  edge.matrix=cbind(unname(annotated.vec), names(annotated.vec))
} else
edge.matrix=cbind(names(annotated.vec), unname(annotated.vec))
return(edge.matrix)
}
#list2edges(annotated.char.list, col_order_inverse=F)




#' @title Converts edge matrix of charater annotations to list
#' @description Takes two columns edge matrix (columns from and two) and produces list
#' @param edge.matrix Two-columns edge matrix.
#' @return The list.
#' @examples
#' #edges2list(edge.matrix)

edges2list<-function(edge.matrix){
uniq_ids=unique(edge.matrix[,1])
list.from.edge=list()
for (i in 1:length(uniq_ids)){
  char.ids=which(edge.matrix[,1]==uniq_ids[i])
  list.from.edge[[i]]=edge.matrix[char.ids,2]
}
names(list.from.edge)<-uniq_ids
return(list.from.edge)
}
#edges2list(edge.matrix)



#' @title Get all character IDs descending from specified term
#' @description Returns all characters located (associated) with a given ontology terms
#' @param ontology ontology_index object.
#' @param terms IDs of ontology terms for which descendants are queried.
#' @param ... other parameters for ontologyIndex::get_descendants() function
#' @return The vector of character IDs.
#' @examples
#' get_descendants_chars(hao.obo, "HAO:0000653")

get_descendants_chars<-function(ontology, terms, ...){
  onto_chars_list=list2edges(ontology$annot_characters, col_order_inverse=T)
  descen<-unique(onto_chars_list[,2][onto_chars_list[,1]%in%
                                ontologyIndex::get_descendants(ontology, terms, ...)])
  return(descen)
}
#get_descendants_chars(hao.obo, "HAO:0000653")



#' @title Get number of chracters per each ontology term
#' @description Returns matrix summarizing  number of characters per each ontology terms in descending order
#' @param ontology ontology_index object with character annatotions included (ontology$annot_characters).
#' @return The matrix of ontology terms IDs, their names and character number.
#' @examples
#' chars_per_term(hao.obo)
#'
chars_per_term<-function(ontology){
  all_des=pblapply(ontology$id, function(x) {get_descendants_chars(ontology, x)})
  char_per_term=unlist(lapply(all_des, length))
  term_tb=char_per_term[order(char_per_term, decreasing=T)]
  term_tb=cbind(names(term_tb), get_name(names(term_tb), hao.obo, names=F), unname(term_tb))
  colnames(term_tb)<-c("ID", "names", "N_chars")
  return(term_tb)
}
#chars_per_term(hao.obo)



