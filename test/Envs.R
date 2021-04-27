library(rhub)
validate_email()
check()

#------------
data(Sharkey_2011)
data(HAO)

hao_obo<-onto_process(HAO, Sharkey_2011[,1], do.annot = F)
ontofast <- new.env(parent = emptyenv())
ontofast$shiny_in <- make_shiny_in(hao_obo)
runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="shiny_in" )

#---------
library(ontoFAST)
data(Sharkey_2011)
data(HAO)


setwd("~/Documents/My_papers/OntoFast_ms/Test_OntoFast")
#library(ontoFAST)

# Ontology and chars
HAO
hao_obo=get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

Sharkey_2011
Sharkey_characters<-read.csv(system.file("data_onto", "Sharkey_2011.csv", package = "ontoFAST"), header=T,  stringsAsFactors = F, na.strings = "")


# Make object for annotation
hao_obo<-onto_process(hao_obo, Sharkey_characters[,1], do.annot = F)
#tt <- make_shiny_in(hao_obo)
ontofast <- new.env(parent = emptyenv())
ontofast$shiny_in <- make_shiny_in(hao_obo)
ls(ontofast)
runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="shiny_in" )

#--------
exists("ontofast", mode="environment")
ontofast <- new.env(parent = emptyenv())
ontofast$shiny_in<-make_shiny_in(hao_obo)

name <- 'sadd'
ontofast[[name]] <- 11
ls("ontofast")

runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="shiny_in" )

ontofast$shiny_in<-runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050") )
ontofast$shiny_in[['terms_selected']]
ontofast$shiny_in[['terms_selected_id']][1:10]

#


#--------
search()
as.environment('package:ontoFAST')
as.environment('ontofast')
