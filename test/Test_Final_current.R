#------------------
#
# Testing OntoFAST
#
#------------------


#   ____________________________________________________________________________
#   HAO full version                                                        ####


#install.packages("ontoFAST")
library(ontoFAST)
setwd("~/Documents/My_papers/OntoFast_ms/Test_OntoFast")

data(Sharkey_2011)
data(HAO)
HAO
hao_obo=get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

Sharkey_2011
Sharkey_characters<-read.csv(system.file("data_onto", "Sharkey_2011.csv", package = "ontoFAST"), header=T,  stringsAsFactors = F, na.strings = "")


#---- Make object for annotation
hao_obo<-onto_process(hao_obo, Sharkey_characters[,1], do.annot = F)
# new envr
ontofast <- new.env(parent = emptyenv())
ontofast$shiny_in <- make_shiny_in(hao_obo)
runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="shiny_in", file2save = "OntoFAST_shiny_in.RData")

ontofast$shiny_in[['terms_selected']]
ontofast$shiny_in[['terms_selected_id']][1:10]

# Unique list of shiny_in
#nm <- names(shiny_in)[!(names(shiny_in) %in% names(HAO))]
#paste0('shiny_in', "[[\'", nm, "\']]", collapse = '\n') %>% cat()
ontofast$shiny_in[['id_characters']]
ontofast$shiny_in[['name_characters']]
ontofast$shiny_in[['parsed_synonyms']]
ontofast$shiny_in[['auto_annot_characters']]
ontofast$shiny_in[['terms_selected']]
ontofast$shiny_in[['terms_selected_id']]
ontofast$shiny_in[['terms_map']]
ontofast$shiny_in[['srch_items']]
ontofast$shiny_in[['auto_annot_characters_id_name']]

tb <- export_annotations(ontofast$shiny_in, annotations="manual", incl.names=TRUE,collapse="; ")

annot <- ontofast$shiny_in[['terms_selected_id']]
tb <- list2edges(annot)
#tb <- list2edges(Sharkey_2011_annot, col_order_inverse = F)
#write.csv(tb, 'annots.csv')

# read in saved file
rm(ontofast)
open.shiny_in <- readRDS('OntoFAST_shiny_in.RData')
open.shiny_in[['terms_selected']]


#   ____________________________________________________________________________
#   HAO (viz sunburst)                                                      ####

library("sunburstR")

ontology_partof=get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"), extract_tags="everything", propagate_relationships = c("BFO:0000050"))
ontology_partof<-onto_process(ontology_partof, Sharkey_2011[,1], do.annot = F)
ontology_partof$annot_characters<-Sharkey_2011_annot
tb<-paths_sunburst(ontology_partof, annotations = ontology_partof$annot_characters, exclude.terms=exclude_terms)
sunburst(tb)



#   ____________________________________________________________________________
#   HAO export Cytoscape                                                    ####

ontology<-HAO
# processing ontology to incorporate character statements
ontology<-onto_process(ontology, Sharkey_2011[,1], do.annot = F)
# embedding manual annotations
ontology$annot_characters<-Sharkey_2011_annot

# exporting
cyto<-export_cytoscape(ontology, annotations = ontology$annot_characters, is_a = c("is_a"), part_of = c("BFO:0000050"))
#write.csv(cyto, file="cyto.csv")





#   ____________________________________________________________________________
#   HAO Query characters                                                    ####

chars_per_term(ontology, annotations = ontology$annot_characters) %>% head()
get_ancestors_chars(ontology, c("CHAR:1", "CHAR:2", "CHAR:3"),  annotations = ontology$annot_characters)
get_descendants_chars(ontology, annotations = ontology$annot_characters, terms="HAO:0000653")



#   ____________________________________________________________________________
#   HAO (no annotation,just for viewing)                                    ####


hao_obo=get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

# Make object for annotation
hao_obo<-onto_process(hao_obo, name_characters=F, do.annot = F)
# new envr
ontofast <- new.env(parent = emptyenv())
ontofast$shiny_in <- make_shiny_in(hao_obo)
runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="shiny_in", file2save = "OntoFAST_shiny_in.RData")


ontofast$shiny_in[['terms_selected']]
ontofast$shiny_in[['terms_selected_id']]


#   ____________________________________________________________________________
#   Spider ontology                                                         ####

onto.folder <- '/Users/taravser/Documents/My_papers/OntoFast_ms/ontoFAST/test/ontologies'
spdr <- get_OBO(file.path(onto.folder, 'spider_comparative_biology_1.1.obo') , extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))
spdr<-onto_process(spdr, name_characters=F, do.annot = F)

ontofast <- new.env(parent = emptyenv())
ontofast$spdr <- make_shiny_in(spdr)
runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="spdr", file2save = "OntoFAST_shiny_in_spdr.RData")

ontofast$spdr[['terms_selected']]
ontofast$spdr[['terms_selected_id']]



#   ____________________________________________________________________________
#   Spider (viz sunburst)                                                   ####

library("sunburstR")

onto.folder <- '/Users/taravser/Documents/My_papers/OntoFast_ms/ontoFAST/test/ontologies'
ontology_partof=get_OBO(file.path(onto.folder, 'spider_comparative_biology_1.1.obo') , extract_tags="everything")

ontology_partof<-onto_process(ontology_partof, name_characters=F, do.annot = F)
ontology_partof$annot_characters<-Sharkey_2011_annot
tb<-paths_sunburst(ontology_partof, annotations = ontology_partof$annot_characters, exclude.terms=NULL)
sunburst(tb)



#   ____________________________________________________________________________
#   Uberon                                                                  ####


onto.folder <- '/Users/taravser/Documents/My_papers/OntoFast_ms/ontoFAST/test/ontologies'
uberon=get_OBO(file.path(onto.folder, 'uberon_obo.owl') , extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

uberon<-onto_process(uberon, name_characters=c("ch1"), do.annot = F)
ontofast <- new.env(parent = emptyenv())
ls(ontofast)
ontofast$uberon <- make_shiny_in(uberon)
runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="uberon", file2save = "OntoFAST_shiny_in.RData")


ontofast$uberon[['parsed_synonyms']]
ontofast$uberon[['terms_selected']]
ontofast$uberon[['terms_selected_id']]


#   ____________________________________________________________________________
#   Drosophila Gross Anatomy Ontology                                       ####


onto.folder <- '/Users/taravser/Documents/My_papers/OntoFast_ms/ontoFAST/test/ontologies'
# could not parse the latest fbbt version (2020-12-02) but managed 2019-03-28
fbbt=get_OBO(file.path(onto.folder, 'fbbt.obo'), extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a") )

fbbt<-onto_process(fbbt, name_characters=c("ch1"), do.annot = F)
ontofast <- new.env(parent = emptyenv())
ls(ontofast)
ontofast$fbbt <- make_shiny_in(fbbt)
runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="fbbt", file2save = "OntoFAST_shiny_in.RData")


ontofast$fbbt [['parsed_synonyms']]
ontofast$fbbt [['terms_selected']]
ontofast$fbbt [['terms_selected_id']]




#   ____________________________________________________________________________
#   list all functions                                                      ####


library(NCmisc)
setwd("~/Documents/My_papers/OntoFast_ms/ontoFAST/R")


ff <- list.functions.in.file('app.R')
ff <-list.functions.in.file('Ontology_operations_main.R')
ff <-list.functions.in.file('app.R')

lapply(ff, function(x) paste(x, collapse = ' ') )
ff[[1]]


