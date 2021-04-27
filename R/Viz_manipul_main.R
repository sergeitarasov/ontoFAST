#' @title Converts a table to list
#' @description Takes a table where each row consists of character ID + an ontology annotation and returns a list.
#' Each character is assigned its own ID CHAR:XXXX
#' @param table A character table with annotations.
#' @param id_col A column ID corresponding to character
#' @param descendants_cols IDs of columns corresponding to character annotations
#' @return The list.
#' @examples
#' # converting Sharkey_2011 dataset to list of character states
#' table2list(Sharkey_2011)
#' @export

table2list<-function(table, id_col=c(1), descendants_cols=c(2:ncol(table))){
annotated.char.list=list()
for (i in 1:nrow(table)) {
  annotated.char.list[[i]]=c(na.omit(as.character(table[i,descendants_cols])))
}
#names(annotated.char.list)<-paste("CHAR:", table[,id_col], sep="")
return(annotated.char.list)
}



#' @title Convert list to edge matrix
#' @description Takes a list of character annotations and creates an edge matrix comprising two columns: from and to.
#' The list to table conversion can be done using ldply function from plyr package: plyr::ldply(list, rbind).
#' @param annotated.char.list Character list with ontology annotations.
#' @param col_order_inverse The default creates the first columns consisting if character IDs and the second columns consisting of ontology annatotaions.
#' The inverse order changes the columns order.
#' @return Two-column matrix.
#' @examples
#' annot_list<-list(`CHAR:1`=c("HAO:0000933", "HAO:0000958"), `CHAR:2`=c("HAO:0000833", "HAO:0000258"))
#' list2edges(annot_list)
#' # use plyr package and run
#' plyr::ldply(annot_list, rbind)
#' @export

list2edges<-function(annotated.char.list, col_order_inverse=FALSE){
annotated.vec=setNames(unlist(annotated.char.list, use.names=FALSE),rep(names(annotated.char.list), lengths(annotated.char.list)))
if (col_order_inverse==TRUE){
  edge.matrix=cbind(unname(annotated.vec), names(annotated.vec))
} else
edge.matrix=cbind(names(annotated.vec), unname(annotated.vec))
return(edge.matrix)
}




#' @title Convert edge matrix to list
#' @description Takes two-column edge matrix (columns from and two) and produces a list
#' @param edge.matrix Two-column edge matrix.
#' @return The list.
#' @examples
#' annot_list<-list(`CHAR:1`=c("HAO:0000933", "HAO:0000958"), `CHAR:2`=c("HAO:0000833", "HAO:0000258"))
#' edge.matrix<-list2edges(annot_list)
#' edges2list(edge.matrix)
#' @export

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



#' @title Get characters that descendants of selected ontology term
#' @description Returns all characters located (associated) with given ontology term(s)
#' @param ontology ontology_index object.
#' @param annotations which annotations to use: "auto" means automatic annotations, "manual" means manual ones.
#' Alternatively, any othe list element containing annotations can be specified.
#' @param terms IDs of ontology terms for which descendants are queried.
#' @param ... other parameters for ontologyIndex::get_descendants() function
#' @return The vector of character IDs.
#' @examples
#' data(HAO)
#' ontology<-HAO
#' ontology$terms_selected_id<-list(`CHAR:1`=c("HAO:0000653"), `CHAR:2`=c("HAO:0000653"))
#' get_descendants_chars(ontology, annotations="manual", "HAO:0000653")
#' @export

get_descendants_chars<-function(ontology, annotations="auto", terms, ...){

  if (is.list(annotations)){
    annot_list<-annotations # specify your annotation list
  } else {

    if (annotations=="auto"){
      annot_list<-ontology$auto_annot_characters
    }
    if (annotations=="manual"){
      annot_list<-ontology$terms_selected_id
    }
  }


  onto_chars_list=list2edges(annot_list, col_order_inverse=TRUE)
  descen<-unique(onto_chars_list[,2][onto_chars_list[,1] %in%
                                       ontologyIndex::get_descendants(ontology=ontology, roots=terms, ...)])
  return(descen)
}




#' @title Get ancestal ontology terms for a set of characters
#' @description Returns all ontology terms which are ancestors of a given character set
#' @param ontology ontology_index object with character annatotions included (ontology$annot_characters).
#' @param char_id IDs of character.
#' @param annotations which annotations to use: "auto" means automatic annotations, "manual" means manual ones.
#' Alternatively, any othe list element containing annotations can be specified.
#' @return The vector of ontology terms IDs.
#' @examples
#' data(HAO)
#' ontology<-HAO
#' ontology$terms_selected_id<-list(`CHAR:1`=c("HAO:0000653"), `CHAR:2`=c("HAO:0000653"))
#' get_ancestors_chars(ontology, c("CHAR:1","CHAR:2"), annotations="manual")
#' @export

get_ancestors_chars<-function(ontology, char_id, annotations="auto" ){

  if (is.list(annotations)){
    annot_list<-annotations # specify your annotation list
  } else {

    if (annotations=="auto"){
      annot_list<-ontology$auto_annot_characters
    }
    if (annotations=="manual"){
      annot_list<-ontology$terms_selected_id
    }
  }


  ontologyIndex::get_ancestors(ontology, unlist(annot_list[char_id], use.names = FALSE))
}





#' @title Get number of chracters per each ontology term
#' @description Returns matrix summarizing  number of characters per each ontology terms in descending order
#' @param ontology ontology_index object with character annatotions included (ontology$annot_characters).
#' @param annotations which annotations to use: "auto" means automatic annotations, "manual" means manual ones.
#' Alternatively, any othe list element containing annotations can be specified.
#' @return The matrix of ontology terms IDs, their names and character number.
#' @examples
#' data(HAO)
#' ontology<-HAO
#' ontology$terms_selected_id<-list(`CHAR:1`=c("HAO:0000653"), `CHAR:2`=c("HAO:0000653"))
#' chars_per_term(ontology, annotations="manual")
#' @export


chars_per_term<-function(ontology, annotations="auto"){

  all_des=pblapply(ontology$id, function(x) {get_descendants_chars(ontology, annotations=annotations, x)})
  char_per_term=unlist(lapply(all_des, length))
  term_tb=char_per_term[order(char_per_term, decreasing=TRUE)]
  term_tb=cbind(names(term_tb), get_onto_name(names(term_tb), ontology, names=FALSE), unname(term_tb))
  colnames(term_tb)<-c("ID", "names", "N_chars")
  return(term_tb)
}



#' @title Return ontology paths for characters
#' @description Returns ontology paths for all characters. These paths can be used to create a sunburst plot of
#' ontological dependencies.
#' @param ontology ontology_index object with character annatotions included.
#' @param annotations which annotations to use: "auto" means automatic annotations, "manual" means manual ones.
#' Alternatively, any othe list element containing annotations can be specified.
#' @param exclude.terms list of terms to exclude
#' @param include.terms list of terms to include
#' @param use.chars indicate whether character ids should be included in output
#' @param sep separator used to delimit ontology terms
#' @return Table.
#' @examples
#' \donttest{
#' # reading in ontology and part_of relatinships only
#' ontology_partof=ontologyIndex::get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),
#'                         extract_tags="everything", propagate_relationships = c("BFO:0000050"))
#' # atomatically annotating ontology
#' ontology_partof<-onto_process(ontology_partof, Sharkey_2011[,1])
#' # creating character paths; exluding redundant terms
#' tb<-paths_sunburst(ontology_partof, annotations =
#' ontology_partof$auto_annot_characters, exclude.terms=exclude_terms)
#' # use sunburstR package if you lack it
#' # library(sunburstR)
#' # create sunburst plot
#' sunburstR::sunburst(tb)
#' }
#' @export

paths_sunburst<-function(ontology, annotations="auto", exclude.terms=NULL, include.terms=NULL, use.chars=TRUE,
                         sep="-"){

  if (is.list(annotations)){
    annot_list<-annotations # specify your annotation list
  } else {

    if (annotations=="auto"){
      annot_list<-ontology$auto_annot_characters
    }
    if (annotations=="manual"){
      annot_list<-ontology$terms_selected_id
    }
  }

  annot_list<-list2edges(annot_list)

  f<-function(char_id) {
    anc<-get_onto_name(ontologyIndex::get_ancestors(ontology, char_id), ontology)
    # if (length(exclude.terms)>1){
    anc<-anc[!anc%in%exclude.terms]
    if (is.null(include.terms)==FALSE){
      anc<-anc[anc%in%include.terms]
    }

    return(anc)
  }


  list_paths=c()
  for (i in 1:nrow(annot_list)){
    tmp=f(annot_list[i,2])

    if (use.chars){
      ps=paste(c( gsub("-", " ", tmp ),
                  annot_list[i,1]), collapse=sep)
    }
    if (use.chars==FALSE){
      ps=paste(c( gsub("-", " ", tmp )),
               collapse=sep)
    }
    names(ps)=length(tmp)
    list_paths=c(list_paths,  ps )
  }


  tb<-data.frame(paths=unname(list_paths), size=rep(1, length(list_paths)), length=names(list_paths), stringsAsFactors =FALSE)

  return(tb)

}



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
                          all_links=FALSE, incl.top.anc=TRUE, highliht_focus=TRUE){
  des=get_descendants(ontology, terms)
  all_edges=c()
  if (all_links){ k=1
  }else k=2

  if (length(is_a)>0){
    edges=list2edges(ontology[[is_a]])
    rows=which(edges[,k]%in%des==TRUE)
    if (length(rows)>0){
      if (incl.top.anc){ rows=c(rows, which(edges[,1]%in%terms==TRUE))}
      all_edges=cbind(matrix(edges[rows,], ncol = 2), color[1])
      #all_edges=cbind(edges[rows,], color[1])
    }
  }

  if (length(part_of)>0){
    edges=list2edges(ontology[[part_of]])
    rows=which(edges[,1]%in%des==TRUE)
    if (length(rows)>0){
      if (incl.top.anc){ rows=c(rows, which(edges[,1]%in%terms==TRUE))}
      all_edges=rbind(all_edges, cbind(matrix(edges[rows,], ncol = 2), color[2]))
      #all_edges=rbind(all_edges, cbind(edges[rows,], color[2]))
    }
  }

  if(is.null(all_edges)==TRUE) return(dt=list(nodes=NULL, edges=NULL))

  #nodes=unique(des)
  nodes=unique(c(all_edges[,1], all_edges[,2]))
  nodes_color<-rep(NA, length(nodes))
  if (highliht_focus){nodes_color[which(nodes==terms)]<-"orange"}

  #length(nodes)
  #length(nodes_color)

  dt_nodes=data.frame(id=nodes,
                      label=get_onto_name(nodes, ontology),
                      title=nodes,
                      color.background =nodes_color,
                      color.highlight.background=nodes_color) #check.rows = TRUE)


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
                          all_links=FALSE, incl.top.anc=TRUE, highliht_focus=TRUE){
  des=get_ancestors(ontology, terms)
  all_edges=c()
  if (all_links){ k=2
  }else k=1

  if (length(is_a)>0){
    edges=list2edges(ontology[[is_a]])
    rows=which(edges[,k]%in%des==TRUE)
    if (length(rows)>0){
      if (incl.top.anc){ rows=c(rows, which(edges[,1]%in%terms==TRUE))}
      all_edges=cbind(matrix(edges[rows,], ncol = 2), color[1])
      #all_edges=cbind(edges[rows,], color[1])
    }
  }

  if (length(part_of)>0){
    edges=list2edges(ontology[[part_of]])
    rows=which(edges[,2]%in%des==TRUE)
    if (length(rows)>0){
      if (incl.top.anc){ rows=c(rows, which(edges[,1]%in%terms==TRUE))}
      all_edges=rbind(all_edges, cbind(matrix(edges[rows,], ncol = 2), color[2]))
      #all_edges=rbind(all_edges, cbind(edges[rows,], color[2]))
    }
  }

  if(is.null(all_edges)==TRUE) return(dt=list(nodes=NULL, edges=NULL))

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



#' @title Make an ontology object for visualization
#' @description Make an ontology object for visualization in a separate environment "ontofast"
#' @param ontology Ontology
#' @return Ontology index object named as shiny_in.
#' @examples
#' data(Sharkey_2011)
#' data(HAO)
#' hao_obo<-onto_process(HAO, Sharkey_2011[,1], do.annot = FALSE)
#' ontofast <- new.env(parent = emptyenv())
#' ontofast$shiny_in <- make_shiny_in(hao_obo)
#' runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="shiny_in" )
#' @export

make_shiny_in<-function(ontology){

  if(exists("ontofast", mode="environment")==FALSE){
    msg <- message("Create and assign objects in ontofast enviroment:\nontofast <- new.env(parent = emptyenv())\nontofast$shiny_in <- make_shiny_in(hao_obo)\nSee the tutorial for details.\n")
    return(msg)
  }

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

#---- working with environment ontofast
# if(exists("ontofast", mode="environment")==FALSE){
#   message("Making envriroment ontofast to store the ontology. Use ls(ontofast) to see the objects")
#   env <- new.env(parent = emptyenv())
#   env <- new.env(parent = emptyenv())
#   env$shiny_in <- shiny_in
#   return(env)
# }
# ontofast[[output.name]] <- shiny_in
#-----------

return(shiny_in)
}



#' @title Export annotation data
#' @description This function converts character annotations stored in shiny_in object to table format.
#' @param ontology Ontology
#' @param annotations which annotations to use: "auto" means automatic annotations, "manual" means manual ones.
#' Alternatively, any othe list element containing annotations can be specified.
#' @param incl.names if TRUE includes terms' names and IDs, otherwise includes just IDs.
#' @param sep.head if incl.names=TRUE, this is a separator attached to the begining of term's ID
#' @param sep.tail if incl.names=TRUE, this is a separator attached to the end of term's ID
#' @param collapse if NULL all annotations of a term placed in separate columns, if a value is specified (e.g., "; ") then all anotations
#' are collapsed in one line given that values
#' @return Returns a table
#' @examples
#' data(Sharkey_2011)
#' data(HAO)
#' hao_obo<-onto_process(HAO, Sharkey_2011[,1], do.annot = FALSE)
#' ontofast <- new.env(parent = emptyenv())
#' ontofast$shiny_in <- make_shiny_in(hao_obo)
#' # runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="shiny_in" )
#' tb<-export_annotations(ontofast$shiny_in, annotations="manual", incl.names=TRUE,collapse="; ")
#' tb<-export_annotations(ontofast$shiny_in, annotations="auto", incl.names=TRUE,collapse="; ")
#' tb<-export_annotations(ontofast$shiny_in, annotations="auto", incl.names=TRUE,collapse=NULL)
#' # save annotations in csv
#' # write.csv(tb, "annotated_characters.csv")
#' @export


export_annotations<-function(ontology, annotations="auto", incl.names=FALSE, sep.head=", ", sep.tail=NULL, collapse=NULL){

  if (is.list(annotations)){
    annot_list<-annotations # specify your annotation list
  } else {

    if (annotations=="auto"){
      annot_list<-ontology$auto_annot_characters
    }
    if (annotations=="manual"){
      annot_list<-ontology$terms_selected_id
    }
  }

  annot.export<-annot_list

  if (incl.names==TRUE){
    annot.export<- lapply(
    annot_list, function(x) {
      paste(get_onto_name(x, ontology), paste(sep.head, x, sep.tail, sep=""), sep="")
    })
  }

 #annot.export[["CHAR:391"]]<-NULL
  annot.export[annot.export ==", "]<-NA

  #which are absent
 absent<-ontology$id_characters[!(ontology$id_characters %in% names(annot.export))]

 # insert absent
 annot.export<-c(annot.export, as.list(setNames(rep(NA, length(absent)), absent)))

 #sort list
 annot.export<-annot.export[ontology$id_characters]

 annot.export<-lapply(annot.export, function(x){paste(x, collapse=collapse)})


 table_annot<-plyr::ldply(annot.export, rbind)
 table_annot<-as.matrix(table_annot)
 table_annot<-cbind(table_annot[,1], unname(ontology$name_characters), table_annot[,2:ncol(table_annot)])
 colnames(table_annot)<-c("ID", "Character statement", rep("Annotation", ncol(table_annot)-2))

 return(table_annot)


}


#' @title Export to Cytoscape format
#' @description This function converts character annotations to Cytoscape format. It returns a table that can be saved as in csv format
#' and imported in Cytoscape. In Cytoscape choose File -> Import -> Network -> File. Then assign columns to nodes and edges. Do not select
#' columns that enumerate the tables' rows!
#' @param ontology Ontology
#' @param annotations which annotations to use: "auto" means automatic annotations, "manual" means manual ones.
#' Alternatively, any other list containing annotations can be specified.
#' @param is_a is_a
#' @param part_of part_of
#' @return Returns a table
#' @examples
#' \donttest{
#' data(Sharkey_2011)
#' data(HAO)
#' # do.annot = T takes a while
#' hao_obo<-onto_process(HAO, Sharkey_2011[,1], do.annot = T)
#' ontofast <- new.env(parent = emptyenv())
#' ontofast$shiny_in <- make_shiny_in(hao_obo)
#' # runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="shiny_in" )
#' cyto<-export_cytoscape(ontofast$shiny_in)
#' write.csv(cyto, "cyto_exp.csv")
#' }
#' @export

export_cytoscape<-function(ontology, annotations="auto", is_a=c("is_a"), part_of=c("BFO:0000050")   ){

  if (is.null(names(ontology$name_characters)) ) stop("Error: ontology object must contain character names")


  if (is.list(annotations)){
    annot_list<-annotations # specify your annotation list
  } else {

    if (annotations=="auto"){
      annot_list<-ontology$auto_annot_characters
    }
    if (annotations=="manual"){
      annot_list<-ontology$terms_selected_id
    }
  }


  # get Part_of relatinshisp
  partof<-list2edges(ontology[[part_of]])
  partof<-cbind(partof, rep("part_of", nrow(partof)))
  partof<-cbind(get_onto_name(partof[,1], ontology), partof)

  # get is_a
  isa<-list2edges(ontology[[is_a]])
  isa<-cbind(isa, rep("is_a", nrow(isa)))
  isa<-cbind(get_onto_name(isa[,1], ontology), isa)

  # get char_of
  charof<-list2edges(annot_list)
  charof<-cbind(charof, rep("char_of", nrow(charof)))
  charof<-cbind(unname(
    ontology$name_characters[c(na.omit(match(unlist(charof[,1], use.names = FALSE), names(ontology$name_characters)  )))]
  ), charof)

  # bind all
  cyto<-rbind(charof, partof, isa)
  colnames(cyto)<-c("character/term_name", "id_from", "id_to", "relationship")


  return(cyto)

}





####Function to create mapping for Shiny objects
map_obj<-function(obj, nchar){
  map_f=paste(obj, c(1:nchar), sep="")
  names(map_f)<-paste(c(1:nchar), sep="")
  return(map_f)
}
#############




#' @title Shortcut to process characters and ontology
#' @description This is a shortcut function to make characters and ontology suitable for visualization using ontoFAST interactive tools.
#' @param ontology Ontology
#' @param name_characters a vector of character names
#' @param do.annot specifiees if you need to run automatic annotations or not
#' @param ... other arguments for annot_all_chars() function
#' @return Ontology index object named
#' @examples
#' data(Sharkey_2011)
#' data(HAO)
#' hao_obo<-onto_process(HAO, Sharkey_2011[,1], do.annot = FALSE)
#' ontofast <- new.env(parent = emptyenv())
#' ontofast$shiny_in <- make_shiny_in(hao_obo)
#' runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="shiny_in" )
#' @export

onto_process<-function(ontology, name_characters, do.annot=TRUE, ...){
  id_characters<-paste("CHAR:", c(1:length(name_characters)), sep="")
  ontology$id_characters<-id_characters
  names(name_characters)<-id_characters
  ontology$name_characters<-name_characters

  if (do.annot){
    ontology$parsed_synonyms<-syn_extract(ontology)
    ontology$auto_annot_characters<-annot_all_chars(ontology, ...)
  }

  return(ontology)

}





