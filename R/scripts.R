source("https://bioconductor.org/biocLite.R")
biocLite("ontoCAT")

library("ontologyIndex")
library("stringdist", lib.loc="~/.local/R/site-library")
library("stringr", lib.loc="/usr/lib/R/site-library")
library("RNeXML", lib.loc="~/.local/R/site-library")
library("pbapply")
.libPaths()

#setwd("/home/tarasov/my-papers-2017/AO-BN-ED-morpho/Ontology-play/HYMAO-Ontho-phylo")
#charsSh=read.csv("chars-Sharkey.csv", header=F,  stringsAsFactors = F)
#char.dt=read.csv("HAO-chars.csv", header=F, stringsAsFactors = F, na.strings = c("","NA"))

#hao.obo=get_OBO("hao_new.obo", extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))
#propagate_relations(hao.obo, "HAO:0000345", "is_a")
######
#nexml<-nexml_read("Sharkey.nexml")
#get_characters(geiger_nex)
#get_characters_list(geiger_nex)
#get_metadata(nexml, "otus")
########
### make the list of synonyms suitable for rearch__________
onto.syn=hao.obo$synonym[50:60]
###

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
#______________________________________________

##return ontology match(es) for a character statement_______________
#if min_set is T then only minimal set is returned
###args
onto.obj=hao.obo
onto.names=hao.obo$name
char.statement="Ocellar"

grepl("ocellus", "Ocellar", ignore.case = T)

which(hao.obo$name=="ocellus")

method = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw")

for (i in seq_along(method)){
  #scores=stringdist(c("Ocellar corona"),hao.obo$name, method=method[i])
  scores=stringsim(c("Ocellar"),hao.obo$name, method=method[i])
  print(method[i])
  #print(hao.obo$name[order(scores, decreasing=T)][1:10])}
  print(which(hao.obo$name[order(scores, decreasing=T)]=="ocellus"))}




for (i in seq_along(method)){
  scores=stringsim(c("ocellar"),hao.obo$name, method=method[i])
  print(method[i])
  print(which(hao.obo$name[order(scores, decreasing=T)]=="ocellus"))}

for (i in seq_along(method)){
  scores=stringdist(c("Ocellar"),hao.obo$name, method=method[i])
  print(method[i])
  print(which(hao.obo$name[order(scores, decreasing=F)]=="ocellus"))}

for (i in seq_along(method)){
  scores=stringdist(c("propleura"),hao.obo$name, method=method[i])
  print(method[i])
  print(which(hao.obo$name[order(scores, decreasing=F)]=="dorsal propleural area"))}

for (i in seq_along(method)){
  scores=stringsim(c("cororna"),"carina",  method=method[i])
  print(method[i])
  print(scores)}

statement="Ocellar corona"
onto.obj=hao.obo
onto.terms="ocellus cell df"

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
#############___________________________
#arg
statement="Propleura  dorsal part"
statement="galea"
thresh=.7 #threshhold for cutting array of ordered terms
onto.obj=hao.obo
##dont  use for one word
#does not work with mesoventrite
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

tt=distance.all.match("Propleura  dorsal part", hao.obo, include_synonyms=T)

gt.name(tt, hao.obo)
##______________________________________




i=1
id.set=list()
for (i in seq_along(str_split)){
  scores=stringdist(str_split[i],hao.obo$name, method="jaccard")
  id.set[i]=list(hao.obo$id[order(scores, decreasing=F)][1:20])
}


gt.name(unlist(id.set)[duplicated(unlist(id.set))], hao.obo)

max(table(unlist(id.set)))



#__________________
c("a","b", "c")[c(3,2,1)]

gt.name(order(scores, decreasing=T), hao.obo, names=T)

seq_sim(a, b, method = c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                         "cosine", "jaccard", "jw")

        onto.names[vapply(onto.names, function(x) {grepl(x, char.statement, ignore.case = T)}, logical(1))]
        ###

        onto.match<-function(onto.names, char.statement, onto.obj, min_set=T){
          terms=onto.names[vapply(onto.names, function(x) {grepl(x, char.statement, ignore.case = T)}, logical(1))]
          if (min_set==T){
            terms=ontologyIndex::minimal_set(onto.obj, names(terms))
            terms=gt.name(terms, onto.obj, names=T)}
          return(terms)
        }

        om=onto.match(c(hao.obo$name, hao.syns), "mesoventrite", hao.obo)
        gt.name(distance.all.match("Propleura  prothoracic katepisterna", hao.obo), hao.obo)

        dm=distance.all.match("Metacoxal plates", hao.obo)

        intersect(names(om), dm)
        minimal_set( hao.obo, c(names(om), dm))
        ##____________________________________________________

        matches=onto.match(c(hao.obo$name, hao.syns), "protibial spur")

        #onto.match(c(hao.obo$name, hao.syns), "protibial spur", hao.obo, min_set = F)

        #onto.match(c(hao.obo$name, hao.syns), "Anterior edge of sternite VIII in male", hao.obo, min_set = T)

        ### annotate characters for Sharkey's dt
        #charsSh=read.csv("chars-Sharkey.csv", header=F,  stringsAsFactors = F)
        char.list=unlist(charsSh[,1])

        annot.list=pblapply(char.list, function(x) {onto.match(c(hao.obo$name, hao.syns), x, hao.obo, min_set = T)})
        #annot.list=setNames(annot.list, char.list)
        annot_gr=list(ids=c(1:392), names=char.list, annotations=annot.list)
        str(annot)

        annot.dist_ex=annot.dist
        #annot.dist_ex=setNames(annot.dist_ex, char.list)
        annot_ds=list(ids=c(1:392), names=char.list, annotations=annot.dist_ex)
        ####______________

        #____compare two sets
        set_org=c("a","b")
        set_annot=c("c", "d")

        match(set_org, set_annot, nomatch = -1)

        comp.sets(set_org, set_annot)

        #all(c("a","b","c") %in% c("c", "b"))
        all(c("c", "b")%in% c("a","b","c"))
        comp.sets(c("c", "f"), c("a","b","c"))
        setdiff(c("c", "b"), c("a","b","c"))

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
        #_________________

        "HAO:0000345"%in%get_ancestors(hao.obo, c("HAO:0000670"))



        i=1

        out.list=list()
        out.list$ids=c()
        out.list$original_missing=list()
        out.list$found_missing=list()
        out.list$identical=c()

        for (i in seq_along(annot$ids)){
          out.list$ids[i]=i
          char_id=which(char.dt[,1]==annot$ids[i])

          if (length(char_id)>0){
            terms.org=c(na.omit(as.character(char.dt[char_id,3:9])))
            comp.list=comp.sets(terms.org, annot$annotations[[i]])


            out.list$identical[i]=comp.list$iden
            out.list$original_missing[[i]]=comp.list$org
            out.list$found_missing[[i]]=comp.list$found
          }
          else out.list$identical[i]="character abscent"
        }
        #____________________
        #grepl dist comparions
        out.list=list()
        out.list$ids=c()
        out.list$original_missing=list()
        out.list$found_missing=list()
        out.list$identical=c()
  i=1
        for (i in seq_along(annot_gr$ids)){

           comp.list=comp.sets(annot_gr$annotations[[i]], annot_ds$annotations[[i]])


            out.list$identical[i]=comp.list$iden
            out.list$original_missing[[i]]=comp.list$org
            out.list$found_missing[[i]]=comp.list$found

          #else out.list$identical[i]="character abscent"
        }




        #___________________
         length(which(out.list$identical=="none"))

        ##
        #out.dist=out.list
        #out.grepl=out.list
        smmr=cbind(out.dist$identical, out.grepl$identical, out.list$identical, dist_count)
        colnames(smmr)<-c("dist", "grepl", "grepl_vs_dist", "dist-count")
        write.csv(smmr, file="dist-grepl.csv", row.names = F)

        dist_count=unlist(lapply(annot.dist_ex, length))
        str(annot.dist_ex)
        ##

        str(out.list)
        str(char.dt
            str(annot)

            char.orig.vs.annot=c()
            #names(char.orig.vs.annot)<-c("id", "statemet", "Istvan's annot", "compar_result", "suggested_annot")
            #i=2
            for (i in seq_along(annot$ids)){
              char_id=which(char.dt[,1]==annot$ids[i])
              vec.orig.stat=c(na.omit(as.character(char.dt[char_id,3:9])))
              orig_name=gt.name(vec.orig.stat, hao.obo)
              vec.dt=c(annot$ids[i], annot$names[i], paste(paste(vec.orig.stat, orig_name, sep="(", collapse="); "), ")", sep=""),
                       out.list$identical[i],
                       paste(paste(names(annot$annotations[[i]]), annot$annotations[[i]], sep="(", collapse="); "), ")", sep=""))

              char.orig.vs.annot=rbind(char.orig.vs.annot, vec.dt)
            }

            colnames(char.orig.vs.annot)<-c("id", "statemet", "Istvan's annot", "compar_result", "suggested_annot")

            write.csv(char.orig.vs.annot, file="auto_annatotated3.csv", row.names = F)

            ##______________




            #########
            #minimal_set(hao.obo, c("HAO:0000670", "HAO:0000345"))

            "HAO:0000345"%in%get_ancestors(hao.obo, c("HAO:0000670"))

            minimal_set(hao.obo, c("HAO:0000301", "HAO:0001765"))

            minimal_set(hao.obo, c("HAO:0000508", "HAO:0000506"))


            paste(paste(names(annot$annotations[[i]]), annot$annotations[[i]], sep="(", collapse="); "), ")", sep="")

            paste(annot$annotations[[i]], c("1","2"), collapse="L")

            ?paste

            paste0(1:12, c("st", "nd", "rd", rep("th", 9)))

            length(which(out.list$identical=="identical"))
            length(which(out.list$identical=="partial"))

            setequal(c(1,2), c(2,1))
            length(intersect(c(1,2), c(3,4)))


            nrow(char.dt)
            length(annot.list)

            annot.list$`Propodeum  lateral longitudinal carina`

            dt=ldply(annot.list, rbind)


            c(na.omit(as.character(char.dt[17,3:9])))

            annot$annotations[17]
            annot$names[17]

            gt.name(c(na.omit(as.character(char.dt[17,3:9]))), hao.obo)

            na.omit(char.dt[,5])

            na.omit((char.dt[1,3:9]))

            ?ldply

            i=1
            for(i in 1:(nrow(char.dt)-1)){
              if ((char.dt[i,1]+1)!=char.dt[i+1,1]) print(i)
            }
            #char 155 is missing in hao annotation
            ##########
            minimal_set(hao.obo, names(matches))

            gt.name(get_ancestors(hao.obo, "HAO:0000875"), hao.obo)
            gt.name("HAO:0000875", hao.obo, names=T)
            gt.name(names(matches), hao.obo)


