## ------------------------------------------------------------------------
# !!! uncomment to run
#library(devtools)
#install_github("sergeitarasov/ontoFAST@main")
library(ontoFAST)

## ------------------------------------------------------------------------
hao_obo<-HAO

## ------------------------------------------------------------------------
hao_obo=get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),
                extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))


## ------------------------------------------------------------------------
char_et_states<-Sharkey_2011

## ------------------------------------------------------------------------
id_characters<-paste("CHAR:",c(1:392), sep="")
hao_obo$id_characters<-id_characters


## ------------------------------------------------------------------------
name_characters<-char_et_states[,1]
names(name_characters)<-id_characters
hao_obo$name_characters<-name_characters

## ------------------------------------------------------------------------
hao_obo$parsed_synonyms<-syn_extract(hao_obo)

## ------------------------------------------------------------------------
hao_obo$auto_annot_characters<-annot_all_chars(hao_obo, use.synonyms=TRUE, min_set=TRUE)

## ------------------------------------------------------------------------
shiny_in<<-make_shiny_in(hao_obo)

## ------------------------------------------------------------------------
# !!! uncomment to run
#runOntoFast()

