#------------------
#
# HAO rda
#
#------------------

setwd("~/Documents/My_papers/OntoFast_ms/ontoFAST/inst/data_onto")

HAO=get_OBO('HAO.obo' , extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

setwd("~/Documents/My_papers/OntoFast_ms/ontoFAST/data")
save(HAO, file='HAO.rda')

rm(HAO)


#------------------
#
# HAO
#
#------------------

hao_obo<-HAO

hao_obo=get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),
                extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

# R embedded data set
Sharkey_characters<-Sharkey_2011

# csv file
Sharkey_characters<-read.csv(system.file("data_onto", "Sharkey_2011.csv",
                                         package = "ontoFAST"), header=T,  stringsAsFactors = F, na.strings = "")

hao_obo<-onto_process(hao_obo, Sharkey_characters[,1], do.annot = F)

shiny_in<<-make_shiny_in(hao_obo)

runOntoFast()

hao_obo$`BFO:0000050`

#---
names(shiny_in)[!names(shiny_in)%in%names(hao_obo)]

shiny_in$part_of
shiny_in$id_characters
shiny_in$name_characters
shiny_in$terms_selected
shiny_in$terms_selected_id
str(shiny_in$terms_map)
shiny_in$srch_items
shiny_in$auto_annot_characters_id_name

dt=get_part_descen(shiny_in, term2show, is_a=links_chk_map[[input$links_chk]][2], #### HAO.obo to ontology index!!!!
                   part_of=links_chk_map[[input$links_chk]][1],
                   all_links=F, incl.top.anc=T, highliht_focus=T)


dt=ontoFAST:::get_part_descen(hao_obo, get_onto_id("mouthparts", hao_obo) , is_a=c("is_a"), part_of=c("BFO:0000050"))



get_part_descen(ontology, terms, is_a=c("is_a"), part_of=c("BFO:0000050"), color=c("red", "blue"),
                          all_links=F, incl.top.anc=T, highliht_focus=T)

#------------------
#
# Spiders
#
#------------------
onto.folder <- '/Users/taravser/Documents/My_papers/OntoFast_ms/ontologies'

spdr=get_OBO(file.path(onto.folder, 'spider_comparative_biology_1.1.obo') , extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

spdr<-onto_process(spdr, Sharkey_characters[,1], do.annot = F)

spdr$id_characters
spdr$name_characters

shiny_in<<-make_shiny_in(spdr)
runOntoFast(part_of=c("part_of"))


#------------------
#
# Uberon
#
#------------------
onto.folder <- '/Users/taravser/Documents/My_papers/OntoFast_ms/ontologies'

uberon=get_OBO(file.path(onto.folder, 'uberon_obo.owl') , extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))

uberon<-onto_process(uberon, Sharkey_characters[,1], do.annot = F)

dt=ontoFAST:::get_part_descen(shiny_in, get_onto_id("head", shiny_in), is_a=c("is_a"), part_of=c("part_of"))

uberon$part_of
shiny_in$part_of
shiny_in$is_a

shiny_in<<-make_shiny_in(uberon)
runOntoFast(part_of=c("part_of"))


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

