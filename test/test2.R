hao_obo=get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),
                extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))


setwd("~/Documents/My_papers/OntoFast_ms/ontoFAST/R")
source('app.R')
source('Ontology_operations_main.R')
source('Viz_manipul_main.R')

setwd("~/Documents/My_papers/OntoFast_ms/ontoFAST/inst/data_onto")

hao_obo=get_OBO( "HAO.obo",
                extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

setwd("~/Documents/My_papers/OntoFast_ms/ontoFAST/data")
# R embedded data set
load('Sharkey_2011.rda')
Sharkey_characters<-Sharkey_2011

hao_obo<-onto_process(hao_obo, Sharkey_characters[,1], do.annot = T)

shiny_in<<-make_shiny_in(hao_obo)
runOntoFast()


