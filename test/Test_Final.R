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


#------------------
#
# HAO (no annotation, kust for viewing)
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

