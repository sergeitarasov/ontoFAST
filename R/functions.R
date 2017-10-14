# package Dependencies
devtools::use_package("pbapply")
devtools::use_package("ontologyIndex")
devtools::use_package("dplyr")
devtools::use_package("shiny")
devtools::use_package("shydashboard")
devtools::use_package("visNetwork")

#############


# list of functions
#gt.name<-function(vec, onto, names=F)
#syn.extract<-function(onto.syn)
#distance.statement.match<-function(statement, onto.terms)
#distance.all.match<-function(statement, onto.obj, thresh=0.7, include_synonyms=F)
#onto.match<-function(onto.names, char.statement, onto.obj, min_set=T)
#
#
#


get_onto_name<-function(vec, onto, names=F){
  #name.vec=onto$name[onto$id%in%unlist(vec, use.names = FALSE)]
  name.vec=onto$name[c(na.omit(match(unlist(vec, use.names = FALSE), onto$id)))]
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

get_onto_id<-function(vec_name, ontology, names=F){
  match_vec<-match(unlist(vec_name, use.names = FALSE), ontology$name)
  ids=names(ontology$name)[match_vec]
  if (names==T) {names(ids)<-ontology$name[match_vec]}
  return(ids)
}
#vec_name=c("ventral mesofurco-profurcal muscle", "anatomical entity")
#get_onto_id(vec_name, hao.obo)






#########################
#
# UNSORTED
#
######################
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

(strsplit("ab", " ")[[1]])

statement=char.list[[341]]
statement="Propleura  dorsal part"

distance.all.match<-function(statement, onto.obj, thresh=0.7, include_synonyms=T){
  statement_split=strsplit(statement, " ")[[1]]
  statement_split=statement_split[!statement_split==""]
   if (length(statement_split)==1){
     selec.terms=character(0)
     return(selec.terms)
   } else

  if (include_synonyms==T){
    onto.names=c(onto.obj$name, onto.obj$parsed_syns)
  } else onto.names=onto.obj$name

  max.all=sapply(onto.names, function(x) {distance.statement.match(statement, x)}) #run over all HAO terms

  #max.all[,1:10]
  #selec.terms=apply(max.all, 1, order, decreasing=T)[1:,] #selection based on ordering
  if (max(max.all)<thresh){
    selec.terms=character(0)
    return(selec.terms)
  } else
    selec.terms=apply(max.all, 1, function(x) {x[which(x>=thresh)]}) #selection based on distance threshhold

  #names(selec.terms[[1]])
  selec.terms=lapply(selec.terms, function(x) {minimal_set(onto.obj, names(x))})

  selec.terms=unlist(selec.terms)

  selec.tb=table(selec.terms)
  selec.tb=selec.tb[selec.tb>1]
  selec.dp=selec.tb[order(selec.tb, decreasing=T)]
  selec.terms=c(na.omit(names(selec.dp)[1:10]))


  if (length(selec.terms)==0){selec.terms=character(0)}
  return(selec.terms)
}

tt=distance.all.match("galea f", hao.obo, include_synonyms=T)


onto.match<-function(onto.names, char.statement, onto.obj, min_set=T){
  terms=onto.names[vapply(onto.names, function(x) {grepl(x, char.statement, ignore.case = T)}, logical(1))]
  if (min_set==T){
    terms=ontologyIndex::minimal_set(onto.obj, names(terms))
   #terms=gt.name(terms, onto.obj, names=T)}
  }
  return(terms)
}

om=onto.match(c(hao.obo$name, hao.syns), "Mandibular peg on ventral margin", hao.obo)
onto.match(c(hao.obo$name, hao.syns), "Metacoxal plates", hao.obo)
gt.name(distance.all.match("Propleura  dorsal part", hao.obo), hao.obo)

dm=distance.all.match(char.list[[341]], hao.obo)

intersect(names(om), dm)
minimal_set( hao.obo, c(names(om), dm))

n<-1000
mat<-matrix(rnorm(n),ncol=10,nrow=100)

###### apply() vs pbapply()
x<-pbapply(mat,1,sum)
y<-apply(mat,1,sum)

x-y



##____________________________________________________
onto.obj=hao.obo
### annotate characters for Sharkey's dt
#charsSh=read.csv("chars-Sharkey.csv", header=F,  stringsAsFactors = F)
char.list=unlist(charsSh[,1])

annot.list=lapply(char.list, function(x) {onto.match(c(hao.obo$name, hao.syns), x, hao.obo, min_set = T)}) ##annotating using grepl
#annot.list=setNames(annot.list, char.list)
#annot=list(ids=c(1:392), names=char.list, annotations=annot.list)

annot.dist=lapply(char.list, function(x) {distance.all.match(x, onto.obj)}) #annotating using distsnce

annot.dist=pblapply(char.list, function(x) {distance.all.match(x, onto.obj)}) #annotating using distsnce

gt.name(annot.dist[[341]], hao.obo)
####______________
char.list[[341]]
