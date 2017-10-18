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
#' # converting Sharkey_2011 data set to list of characater states
#' list_data<-table2list(Sharkey_2011)

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
#' annot_list<-list(`CHAR:1`=c("HAO:0000933", "HAO:0000958"), `CHAR:2`=c("HAO:0000833", "HAO:0000258"))
#' list2edges(annot_list)

list2edges<-function(annotated.char.list, col_order_inverse=F){
annotated.vec=setNames(unlist(annotated.char.list, use.names=F),rep(names(annotated.char.list), lengths(annotated.char.list)))
if (col_order_inverse==T){
  edge.matrix=cbind(unname(annotated.vec), names(annotated.vec))
} else
edge.matrix=cbind(names(annotated.vec), unname(annotated.vec))
return(edge.matrix)
}




#' @title Converts edge matrix of charater annotations to list
#' @description Takes two columns edge matrix (columns from and two) and produces list
#' @param edge.matrix Two-columns edge matrix.
#' @return The list.
#' @examples
#' annot_list<-list(`CHAR:1`=c("HAO:0000933", "HAO:0000958"), `CHAR:2`=c("HAO:0000833", "HAO:0000258"))
#' edge.matrix<-list2edges(annot_list)
#' edges2list(edge.matrix)

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



#' @title Get all character IDs descending from specified term
#' @description Returns all characters located (associated) with a given ontology terms
#' @param ontology ontology_index object.
#' @param terms IDs of ontology terms for which descendants are queried.
#' @param ... other parameters for ontologyIndex::get_descendants() function
#' @return The vector of character IDs.
#' @examples
#' ontology<-HAO
#' ontology$annot_characters<-list(`CHAR:1`=c("HAO:0000653"), `CHAR:2`=c("HAO:0000653"))
#' get_descendants_chars(ontology, "HAO:0000653")

get_descendants_chars<-function(ontology, terms, ...){
  onto_chars_list=list2edges(ontology$annot_characters, col_order_inverse=T)
  descen<-unique(onto_chars_list[,2][onto_chars_list[,1] %in%
                                       ontologyIndex::get_descendants(ontology=ontology, roots=terms, ...)])
  return(descen)
}




#' @title Get all ancestal ontology terms in a given set of characters
#' @description Returns all ontology terms which are ancestors of a given character set
#' @param ontology ontology_index object with character annatotions included (ontology$annot_characters).
#' @param char_id IDs of character.
#' @return The vector of ontology terms IDs.
#' @examples
#' ontology<-HAO
#' ontology$annot_characters<-list(`CHAR:1`=c("HAO:0000653"), `CHAR:2`=c("HAO:0000653"))
#' get_ancestors_chars(ontology, c("CHAR:1","CHAR:2"))

get_ancestors_chars<-function(ontology, char_id){
  ontologyIndex::get_ancestors(ontology, unlist(ontology$annot_characters[char_id], use.names = FALSE))
}



#' @title Get number of chracters per each ontology term
#' @description Returns matrix summarizing  number of characters per each ontology terms in descending order
#' @param ontology ontology_index object with character annatotions included (ontology$annot_characters).
#' @return The matrix of ontology terms IDs, their names and character number.
#' @examples
#' ontology<-HAO
#' ontology$annot_characters<-list(`CHAR:1`=c("HAO:0000653"), `CHAR:2`=c("HAO:0000653"))
#' chars_per_term(ontology)

chars_per_term<-function(ontology){
  all_des=pblapply(ontology$id, function(x) {get_descendants_chars(ontology, x)})
  char_per_term=unlist(lapply(all_des, length))
  term_tb=char_per_term[order(char_per_term, decreasing=T)]
  term_tb=cbind(names(term_tb), get_onto_name(names(term_tb), ontology, names=F), unname(term_tb))
  colnames(term_tb)<-c("ID", "names", "N_chars")
  return(term_tb)
}
#terms_tb=chars_per_term(hao.obo)
#write.csv(terms_tb, file="n_characters_per_term.csv")


#' @title Summarizes path of all ancestors (names) for every annotate character
#' @description Returns matrix with character ids and their ancestor paths
#' @param ontology ontology_index object with character annatotions included (ontology$annot_characters).
#' @param sep separator use to delimit ontology terms
#' @return The matrix.
#' @examples
#' #getting ontology
#' ontology<-HAO
#' # reading in characters
#' char_et_states<-Sharkey_2011
#' # embedding characters and character ids into ontology
#' id_characters<-paste("CHAR:",c(1:392), sep="")
#' name_characters<-char_et_states[,1]
#' names(name_characters)<-id_characters
#' ontology$name_characters<-name_characters
#' ontology$id_characters<-id_characters
#' ontology$annot_characters<-list(`CHAR:1`=c("HAO:0000653"), `CHAR:2`=c("HAO:0000653"))
#' #' all_char_paths(ontology)

all_char_paths<-function(ontology, sep=" | "){
  f<-function(char) rev(get_onto_name(get_ancestors_chars(ontology, char), ontology))
  list_paths=lapply(names(ontology$annot_characters), function(x) paste(f(x), collapse=sep))
  slec_names=ontology$name_characters[ontology$id_characters%in%names(ontology$annot_characters)]
  out<-cbind(names(ontology$annot_characters), slec_names, unlist(list_paths))
  colnames(out)<-c("ID", "Statement", "Full_ontology_anotation")
  return(out)

}


#
# #' @title Makes dataframe of descndanrts to plot using visNetwork
# #' @description Returns a list of two dataframes: nodes and edges
# #' @param ontology ontology_index object.
# #' @param terms temr id for which descendants to be displayed
# #' @param is_a id of how is_a relationships are coded in ontology.
# #' To no included in output use NA.
# #' @param part_of same as previous
# #' @param color color for is_a and part_of relationships
# #' @param all_links whether all links (is_a and part_of) which link descendants with other nodes must be included in the output.
# #' Better not use as it makes the output messy.
# #' @param incl.top.anc include the parents of terms
# #' @param highliht_focus whether terms mus be highlited
# #' @return The list of dataframes.
# #' @examples
# #' dt=get_part_descen(hao.obo, get_onto_id("mouthparts", ontology) , is_a=c("is_a"), part_of=c("BFO:0000050"))
# #' visNetwork(dt$nodes, dt$edges, width = "100%", height = "100%") %>%
# #' visNodes(borderWidthSelected=4)%>%
# #' visOptions(highlightNearest = TRUE)%>%
# #' visLayout(randomSeed = 12)
#

get_part_descen<-function(ontology, terms, is_a=c("is_a"), part_of=c("BFO:0000050"), color=c("red", "blue"),
                          all_links=F, incl.top.anc=T, highliht_focus=T){
  des=get_descendants(ontology, terms)
  all_edges=c()
  if (all_links){ k=1
  }else k=2

  if (length(is_a)>0){
    edges=list2edges(ontology[[is_a]])
    rows=which(edges[,k]%in%des==T)
    if (length(rows)>0){
      if (incl.top.anc){ rows=c(rows, which(edges[,1]%in%terms==T))}
      all_edges=cbind(matrix(edges[rows,], ncol = 2), color[1])
      #all_edges=cbind(edges[rows,], color[1])
    }
  }

  if (length(part_of)>0){
    edges=list2edges(ontology[[part_of]])
    rows=which(edges[,1]%in%des==T)
    if (length(rows)>0){
      if (incl.top.anc){ rows=c(rows, which(edges[,1]%in%terms==T))}
      all_edges=rbind(all_edges, cbind(matrix(edges[rows,], ncol = 2), color[2]))
      #all_edges=rbind(all_edges, cbind(edges[rows,], color[2]))
    }
  }

  if(is.null(all_edges)==T) return(dt=list(nodes=NULL, edges=NULL))

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

# dt=get_part_descen(hao.obo, get_onto_id("mouthparts", ontology) , is_a=c("is_a"), part_of=c("BFO:0000050"))
#
# get_part_descen(hao.obo, "HAO:0000011", is_a=c("is_a"), part_of=c("BFO:0000050"))
#
# dt=get_part_descen(hao.obo, get_onto_id("thorax", ontology) , is_a=c("is_a"), part_of=c("BFO:0000050"))
#
# visNetwork(dt$nodes, dt$edges, width = "100%", height = "100%") %>%
#   visNodes(borderWidthSelected=4)%>%
#   visOptions(highlightNearest = TRUE)%>%
#   visLayout(randomSeed = 12)
#
# visNetwork(dt$nodes, dt$edges, width = "100%", height = "100%") %>%
#   visNodes(borderWidthSelected=4)%>%
#   visOptions(highlightNearest = TRUE)%>%
#   visLayout(randomSeed = 12) %>%
#   visIgraphLayout(layout="layout_with_gem")



###############################################
#### Similar to above but for ancestors
#
# #' @title Makes dataframe of ancestors to plot using visNetwork
# #' @description Returns a list of two dataframes: nodes and edges
# #' @param ontology ontology_index object.
# #' @param terms temr id for which descendants to be displayed
# #' @param is_a id of how is_a relationships are coded in ontology.
# #' To no included in output use NA.
# #' @param part_of same as previous
# #' @param color color for is_a and part_of relationships
# #' @param all_links whether all links (is_a and part_of) which link descendants with other nodes must be included in the output.
# #' Better not use as it makes the output messy.
# #' @param incl.top.anc include the parents of terms
# #' @param highliht_focus whether terms mus be highlited
# #' @return The list of dataframes.
# #' @examples
# #' dt=get_part_anc(hao.obo, get_onto_id("mouthparts", ontology) , is_a=c("is_a"), part_of=c("BFO:0000050"))
# #' visNetwork(dt$nodes, dt$edges, width = "100%", height = "100%") %>%
# #' visNodes(borderWidthSelected=4)%>%
# #' visOptions(highlightNearest = TRUE)%>%
# #' visLayout(randomSeed = 12)


get_part_anc<-function(ontology, terms, is_a=c("is_a"), part_of=c("BFO:0000050"), color=c("red", "blue"),
                          all_links=F, incl.top.anc=T, highliht_focus=T){
  des=get_ancestors(ontology, terms)
  all_edges=c()
  if (all_links){ k=2
  }else k=1

  if (length(is_a)>0){
    edges=list2edges(ontology[[is_a]])
    rows=which(edges[,k]%in%des==T)
    if (length(rows)>0){
      if (incl.top.anc){ rows=c(rows, which(edges[,1]%in%terms==T))}
      all_edges=cbind(matrix(edges[rows,], ncol = 2), color[1])
      #all_edges=cbind(edges[rows,], color[1])
    }
  }

  if (length(part_of)>0){
    edges=list2edges(ontology[[part_of]])
    rows=which(edges[,2]%in%des==T)
    if (length(rows)>0){
      if (incl.top.anc){ rows=c(rows, which(edges[,1]%in%terms==T))}
      all_edges=rbind(all_edges, cbind(matrix(edges[rows,], ncol = 2), color[2]))
      #all_edges=rbind(all_edges, cbind(edges[rows,], color[2]))
    }
  }

  if(is.null(all_edges)==T) return(dt=list(nodes=NULL, edges=NULL))

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



#' @title Make ontology object for vizualization
#' @description Make ontology object for vizualization
#' @param ontology Ontology
#' @return Ontology index object named as shiny_in.
#' @examples
#' make_shiny_in(HAO)

make_shiny_in<-function(ontology){

shiny_in<- ontology
shiny_in$terms_selected<-list()
shiny_in$terms_selected_id<-list()

#map between term_id_name and term_id
shiny_in$terms_map<-paste(unname(ontology$name), paste(", ", names(ontology$name), sep=""), sep="")
names(shiny_in$terms_map)<-names(ontology$name)

# making serch terms for Selectize using synonyms
shiny_in$srch_items<-c(names(ontology$name), names(ontology$parsed_synonyms))
names(shiny_in$srch_items)<-c(
  paste(unname(ontology$name), names(ontology$name), sep=", "),
                              paste(unname(ontology$parsed_synonyms), get_onto_name(names(ontology$parsed_synonyms), ontology),
                                    sep=" <synonym of> ")
                              )


# rendering auto annotations with IDs and names
shiny_in$auto_annot_characters_id_name <-lapply(
  ontology$auto_annot_characters, function(x) {
    paste(get_onto_name(x, ontology), paste(", ", x, sep=""), sep="")
    })
shiny_in$auto_annot_characters_id_name[shiny_in$auto_annot_characters_id_name ==", "]<-NULL #making empty annot as na


return(shiny_in)
}




####Function to create mapping for Shiny
map_obj<-function(obj, nchar){
  map_f=paste(obj, c(1:nchar), sep="")
  names(map_f)<-paste(c(1:nchar), sep="")
  return(map_f)
}

#map_f=c("checkbox1", "checkbox2", "checkbox3")
#names(map_f)<-c("ids_selec1", "ids_selec2", "ids_selec3")





