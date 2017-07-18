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

table2list<-function(table, id_col=c(1), descendants_cols=c(2:ncol(table))){
annotated.char.list=list()
for (i in 1:nrow(table)) {
  annotated.char.list[[i]]=c(na.omit(as.character(table[i,descendants_cols])))
}
#names(annotated.char.list)<-paste("CHAR:", table[,id_col], sep="")
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



#' @title Get all ancestal ontology terms in a given set of characters
#' @description Returns all ontology terms which are ancestors of a given character set
#' @param ontology ontology_index object with character annatotions included (ontology$annot_characters).
#' @param char_id IDs of character.
#' @return The vector of ontology terms IDs.
#' @examples
#'get_ancestors_chars(hao.obo, c("CHAR:381","CHAR:382"))

get_ancestors_chars<-function(ontology, char_id){
  ontologyIndex::get_ancestors(ontology, unlist(ontology$annot_characters[char_id], use.names = FALSE))
}
#get_ancestors_chars(hao.obo, c("CHAR:381","CHAR:382"))




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
  term_tb=cbind(names(term_tb), get_onto_name(names(term_tb), hao.obo, names=F), unname(term_tb))
  colnames(term_tb)<-c("ID", "names", "N_chars")
  return(term_tb)
}
terms_tb=chars_per_term(hao.obo)
write.csv(terms_tb, file="n_characters_per_term.csv")


#' @title Summarizes path of all ancestors (names) for every annotate character
#' @description Returns matrix with character ids and their ancestor paths
#' @param ontology ontology_index object with character annatotions included (ontology$annot_characters).
#' @param sep separator use to delimit ontology terms
#' @return The matrix.
#' @examples
#' chr_paths=all_char_paths(ontology)
#'write.csv(chr_paths, file="full_char_annots_ontology.csv")


all_char_paths<-function(ontology, sep=" | "){
  f<-function(char) rev(get_onto_name(get_ancestors_chars(ontology, char), ontology))
  list_paths=lapply(names(ontology$annot_characters), function(x) paste(f(x), collapse=sep))
  slec_names=ontology$name_characters[ontology$id_characters%in%names(ontology$annot_characters)]
  out<-cbind(names(ontology$annot_characters), slec_names, unlist(list_paths))
  colnames(out)<-c("ID", "Statement", "Full_ontology_anotation")
  return(out)

}






#' @title Makes dataframe of descndanrts to plot using visNetwork
#' @description Returns a list of two dataframes: nodes and edges
#' @param ontology ontology_index object.
#' @param terms temr id for which descendants to be displayed
#' @param is_a id of how is_a relationships are coded in ontology.
#' To no included in output use NA.
#' @param part_of same as previous
#' @param color color for is_a and part_of relationships
#' @param all_links whether all links (is_a and part_of) which link descendants with other nodes must be included in the output.
#' Better not use as it makes the output messy.
#' @param incl.top.anc include the parents of terms
#' @param highliht_focus whether terms mus be highlited
#' @return The list of dataframes.
#' @examples
#' dt=get_part_descen(hao.obo, get_onto_id("mouthparts", ontology) , is_a=c("is_a"), part_of=c("BFO:0000050"))
#' visNetwork(dt$nodes, dt$edges, width = "100%", height = "100%") %>%
#' visNodes(borderWidthSelected=4)%>%
#' visOptions(highlightNearest = TRUE)%>%
#' visLayout(randomSeed = 12)
#'

get_part_descen<-function(ontology, terms, is_a=c("is_a"), part_of=c("BFO:0000050"), color=c("red", "blue"),
                          all_links=F, incl.top.anc=T, highliht_focus=T){
  des=get_descendants(ontology, terms)
  all_edges=c()
  if (all_links){ k=1
  }else k=2

  if (length(is_a)>0){
    edges=list2edges(ontology[[is_a]])
    rows=which(edges[,k]%in%des==T)
    if (incl.top.anc){ rows=c(rows, which(edges[,1]%in%terms==T))}
    all_edges=cbind(edges[rows,], color[1])
  }

  if (length(part_of)>0){
    edges=list2edges(ontology[[part_of]])
    rows=which(edges[,1]%in%des==T)
    if (incl.top.anc){ rows=c(rows, which(edges[,1]%in%terms==T))}
    all_edges=rbind(all_edges, cbind(edges[rows,], color[2]))
  }

  #nodes=unique(des)
  nodes=unique(c(all_edges[,1], all_edges[,2]))
  nodes_color<-rep(NA, length(nodes))
  if (highliht_focus){nodes_color[which(nodes==terms)]<-"orange"}

  dt_nodes=data.frame(id=nodes,
                      label=get_onto_name(nodes, ontology),
                      title=nodes,
                      color.background =nodes_color,
                      color.highlight.background=nodes_color)


  dt_edges=data.frame(from=all_edges[,1],
                      to=all_edges[,2],
                      arrows="to",
                      color=all_edges[,3])
  dt=list(nodes=dt_nodes, edges=dt_edges)
  return(dt)
}

dt=get_part_descen(hao.obo, get_onto_id("mouthparts", ontology) , is_a=c("is_a"), part_of=c("BFO:0000050"))
dt=get_part_descen(hao.obo, get_onto_id("thorax", ontology) , is_a=c("is_a"), part_of=c("BFO:0000050"))

visNetwork(dt$nodes, dt$edges, width = "100%", height = "100%") %>%
  visNodes(borderWidthSelected=4)%>%
  visOptions(highlightNearest = TRUE)%>%
  visLayout(randomSeed = 12)

visNetwork(dt$nodes, dt$edges, width = "100%", height = "100%") %>%
  visNodes(borderWidthSelected=4)%>%
  visOptions(highlightNearest = TRUE)%>%
  visLayout(randomSeed = 12) %>%
  visIgraphLayout(layout="layout_with_gem")



#_________________________________________________________________________
colnames(dt_out)<-c("id", "statemet", "selected_annot", "grep_id", "grep_id_name")
i=17
char_id=
#Make input for shiny app
data_interctive<-function(ontology){
  c1=char_id=ontology$id_characters
  c2=ontology$name_characters
  c3=ontology$annot_characters
  c4=grep_all_chars
  c5=lapply(grep_all_chars, function(x) {paste(get_onto_name(x, ontology), paste("(", x, ")", sep=""))})
  shiny_in=list(c1, c2, c3, c4, c5)
  return(shiny_in)
}



####Map checkbox with Text objects to retrive info in Shiny
nchar=3
map_obj<-function(nchar){
  map_f=paste("checkbox", c(1:nchar), sep="")
  names(map_f)<-paste("ids_selec", c(1:nchar), sep="")
  return(map_f)
}

#map_f=c("checkbox1", "checkbox2", "checkbox3")
#names(map_f)<-c("ids_selec1", "ids_selec2", "ids_selec3")



