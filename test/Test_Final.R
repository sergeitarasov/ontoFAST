#------------------
#
# Testing OntoFAST
#
#------------------

#------------------
#
# HAO (full version)
#
#------------------
setwd("~/Documents/My_papers/OntoFast_ms/Test_OntoFast")
#library(ontoFAST)

# Ontology and chars
HAO
hao_obo=get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

Sharkey_2011
Sharkey_characters<-read.csv(system.file("data_onto", "Sharkey_2011.csv", package = "ontoFAST"), header=T,  stringsAsFactors = F, na.strings = "")

# Make object for annotation
hao_obo<-onto_process(hao_obo, Sharkey_characters[,1], do.annot = T)
shiny_in<-make_shiny_in(hao_obo)
shiny_in<-runOntoFast(shiny_in=shiny_in, is_a = c("is_a"), part_of = c("BFO:0000050") )

# Unique list of shiny_in
#nm <- names(shiny_in)[!(names(shiny_in) %in% names(HAO))]
#paste0('shiny_in', "[[\'", nm, "\']]", collapse = '\n') %>% cat()
shiny_in[['id_characters']]
shiny_in[['name_characters']]
shiny_in[['parsed_synonyms']]
shiny_in[['auto_annot_characters']]
shiny_in[['terms_selected']]
shiny_in[['terms_selected_id']]
shiny_in[['terms_map']]
shiny_in[['srch_items']]
shiny_in[['auto_annot_characters_id_name']]

# read in saved file
load('OntoFAST_annotation_shiny_in.RData')

#------------------
#
# HAO (viz sunburst)
#
#------------------
#library("sunburstR")
ontology_partof=get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"), extract_tags="everything", propagate_relationships = c("BFO:0000050"))
ontology_partof<-onto_process(ontology_partof, Sharkey_2011[,1], do.annot = F)
ontology_partof$annot_characters<-Sharkey_2011_annot

tb<-paths_sunburst(ontology_partof, annotations = ontology_partof$annot_characters, exclude.terms=NULL)
sunburst(tb)


#------------------
#
# HAO export Cytoscape
#
#------------------
ontology<-HAO
# processing ontology to incorporate character statements
ontology<-onto_process(ontology, Sharkey_2011[,1], do.annot = F)
# embedding manual annotations
ontology$annot_characters<-Sharkey_2011_annot

# exporting
cyto<-export_cytoscape(ontology, annotations = ontology$annot_characters, is_a = c("is_a"), part_of = c("BFO:0000050"))
write.csv(cyto, file="HAO_chars.csv")

#------------------
#
# HAO Query characters
#
#------------------
chars_per_term(ontology, annotations = ontology$annot_characters) %>% head()
get_ancestors_chars(ontology, c("CHAR:1", "CHAR:2", "CHAR:3"),  annotations = ontology$annot_characters)
get_descendants_chars(ontology, annotations = ontology$annot_characters, terms="HAO:0000653")


#------------------
#
# HAO (no annotation,just for viewing)
#
#------------------

hao_obo=get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

# Make object for annotation
hao_obo<-onto_process(hao_obo, name_characters=F, do.annot = F)
shiny_in<-make_shiny_in(hao_obo)
shiny_in<-runOntoFast(shiny_in=shiny_in, is_a = c("is_a"), part_of = c("BFO:0000050") )

shiny_in[['terms_selected']]
shiny_in[['terms_selected_id']]

#------------------
#
# Spider ontology
#
#------------------
onto.folder <- '/Users/taravser/Documents/My_papers/OntoFast_ms/ontoFAST/test/ontologies'
spdr <- get_OBO(file.path(onto.folder, 'spider_comparative_biology_1.1.obo') , extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))
spdr<-onto_process(spdr, name_characters=F, do.annot = F)

spdr<-onto_process(spdr, name_characters=F, do.annot = F)
shiny_in<-make_shiny_in(spdr)
shiny_in<-runOntoFast(shiny_in=shiny_in, is_a = c("is_a"), part_of=c("part_of"))

shiny_in[['terms_selected']]
shiny_in[['terms_selected_id']]


#------------------
#
# Spider (viz sunburst)
#
#------------------
#library("sunburstR")
onto.folder <- '/Users/taravser/Documents/My_papers/OntoFast_ms/ontoFAST/test/ontologies'
ontology_partof=get_OBO(file.path(onto.folder, 'spider_comparative_biology_1.1.obo') , extract_tags="everything")

ontology_partof<-onto_process(ontology_partof, name_characters=F, do.annot = F)
ontology_partof$annot_characters<-Sharkey_2011_annot
tb<-paths_sunburst(ontology_partof, annotations = ontology_partof$annot_characters, exclude.terms=NULL)
sunburst(tb)


#------------------
#
# Uberon
#
#------------------

onto.folder <- '/Users/taravser/Documents/My_papers/OntoFast_ms/ontoFAST/test/ontologies'
uberon=get_OBO(file.path(onto.folder, 'uberon_obo.owl') , extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

uberon<-onto_process(uberon, name_characters=F, do.annot = F)
annot_all_chars()
shiny_in<-make_shiny_in(uberon)
shiny_in<-runOntoFast(shiny_in=shiny_in, is_a = c("is_a"), part_of=c("part_of"))

shiny_in[['parsed_synonyms']]
shiny_in[['terms_selected']]
shiny_in[['terms_selected_id']]

#------------------
#
# Drosophila Gross Anatomy Ontology
#
#------------------

onto.folder <- '/Users/taravser/Documents/My_papers/OntoFast_ms/ontoFAST/test/ontologies'
# could not parse the latest fbbt version (2020-12-02) but managed 2019-03-28
fbbt=get_OBO(file.path(onto.folder, 'fbbt.obo'), extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a") )

fbbt<-onto_process(fbbt, name_characters=F, do.annot = F)
shiny_in<-make_shiny_in(fbbt)
shiny_in<-runOntoFast(shiny_in=shiny_in, is_a = c("is_a"), part_of=c("part_of"))

shiny_in[['parsed_synonyms']]
shiny_in[['terms_selected']]
shiny_in[['terms_selected_id']]

#------------------
#
# list all functions
#
#------------------

library(NCmisc)
setwd("~/Documents/My_papers/OntoFast_ms/ontoFAST/R")


ff <- list.functions.in.file('app.R')
ff <-list.functions.in.file('Ontology_operations_main.R')
ff <-list.functions.in.file('app.R')

lapply(ff, function(x) paste(x, collapse = ' ') )
ff[[1]]

