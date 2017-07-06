# list of functions
#gt.name<-function(vec, onto, names=F)
#syn.extract<-function(onto.syn)
#distance.statement.match<-function(statement, onto.terms)
#distance.all.match<-function(statement, onto.obj, thresh=0.7, include_synonyms=F)
#onto.match<-function(onto.names, char.statement, onto.obj, min_set=T)
#
#
#

gt.name<-function(vec, onto, names=F){
  name.vec=onto$name[onto$id%in%unlist(vec, use.names = FALSE)]
  if (names==F) {name.vec=unname(name.vec)}
  return(name.vec)
}

syn.extract<-function(onto.syn){
  syn.raw=stack(onto.syn)
  syn.raw=setNames(syn.raw$values, syn.raw$ind)
  syn.extr=str_match(syn.raw, pattern="\"(.*?)\"")[,2]
  names(syn.extr)<-names(syn.raw)
  return(syn.extr)
}
hao.syns=syn.extract(hao.obo$synonym)
hao.obo$parsed_syns=hao.syns #make ontology object to contains parsed synonyms
class(hao.obo)

distance.statement.match<-function(statement, onto.terms){
  statement=tolower(statement); onto.terms=tolower(onto.terms)
  statement_split=strsplit(statement, " ")[[1]]
  statement_split=statement_split[!statement_split==""]
  onto.terms_split=strsplit(onto.terms, " ")[[1]]
  onto.terms_split=onto.terms_split[!onto.terms_split==""]

  max.vec=c()
  for (i in seq_along(statement_split)){
    score=stringsim(statement_split[i],onto.terms_split, method="osa") #quering one statemetn element against all onto.terms
    max.vec=c(max.vec, max(score))
  }
  return(max.vec) #maximum score inferred for every word in stetement
}
distance.statement.match("Ocellar corona", "ocellus cell df")



distance.all.match<-function(statement, onto.obj, thresh=0.7, include_synonyms=F){
  if (include_synonyms==T){
    onto.names=c(onto.obj$name, onto.obj$parsed_syns)
  } else onto.names=onto.obj$name

  max.all=sapply(onto.names, function(x) {distance.statement.match(statement, x)}) #run over all HAO terms
  #selec.terms=apply(max.all, 1, order, decreasing=T)[1:,] #selection based on ordering
  if (max(max.all)<thresh){
    selec.terms=character(0)
    return(selec.terms)
  } else
    selec.terms=apply(max.all, 1, function(x) {x[which(x>=thresh)]}) #selection based on distance threshhold

  #names(selec.terms[[1]])
  selec.terms=lapply(selec.terms, function(x) {minimal_set(onto.obj, names(x))})

  selec.terms=unlist(selec.terms)
  selec.terms=selec.terms[duplicated(selec.terms)]
  return(selec.terms)
}

tt=distance.all.match("galea f", hao.obo, include_synonyms=T)


onto.match<-function(onto.names, char.statement, onto.obj, min_set=T){
  terms=onto.names[vapply(onto.names, function(x) {grepl(x, char.statement, ignore.case = T)}, logical(1))]
  if (min_set==T){
    terms=ontologyIndex::minimal_set(onto.obj, names(terms))
    terms=gt.name(terms, onto.obj, names=T)}
  return(terms)
}

om=onto.match(c(hao.obo$name, hao.syns), "mesoventrite", hao.obo)
onto.match(c(hao.obo$name, hao.syns), "Metacoxal plates", hao.obo)
gt.name(distance.all.match("mesoventrite", hao.obo), hao.obo)

dm=distance.all.match("Metacoxal plates", hao.obo)

intersect(names(om), dm)
minimal_set( hao.obo, c(names(om), dm))
##____________________________________________________



