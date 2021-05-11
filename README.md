[![CRAN status](https://www.r-pkg.org/badges/version/ontoFAST)](https://CRAN.R-project.org/package=ontoFAST)

# ontoFAST: an R package for interactive and semi-automatic annotation of characters with biological ontologies with biological ontologies
 <p align="left">
  <img src="https://github.com/sergeitarasov/ontoFAST/blob/master/test/Icon_ontofast_new.png" width="150" title="hover text">
</p> 

```{r}
# Install necessary packages
install.packages("ontoFAST")
install.packages("igraph")
library("ontoFAST")

# Read in data
hao_obo<-get_OBO(system.file("data_onto", "HAO.obo", package = "ontoFAST"),
extract_tags="everything", propagate_relationships = c("BFO:0000050", "is_a"))
data(Sharkey_2011)

# Process data and run interactive session
hao_obo<-onto_process(hao_obo, Sharkey_characters[,1], do.annot = F)
ontofast <- new.env(parent = emptyenv())
ontofast$shiny_in <- make_shiny_in(hao_obo)
runOntoFast(is_a = c("is_a"), part_of = c("BFO:0000050"), shiny_in="shiny_in", file2save = "OntoFAST_shiny_in.RData")

# Save data
out <- list2edges(ontofast$shiny_in$terms_selected_id)
write.csv(out, "annotations.csv")

```


[Installation and Manual](https://github.com/sergeitarasov/ontoFAST/wiki)

The package provides convenient and efficient tools that allow linking character statements used in phylogenetics with anatomy and phenotype ontologies. It also provides functions to query linked characters using ontological relationships. The prime aim of this package is to enhance integration of comparative phylogenetic methods with ontological approaches.

#### The workflow:
1. Get your character statements and ontology which you want to use to link the characters with.
2. Run automatic annotation of characters with ontology in R.
3. Automatic annotations are not perfect, so post-process them manually using interactive environment straight from R. The interactive mode visualizes the ontology as a network thus providing a convenient way to navigate through it.
4. The annotations are done! Save them for future use or query them to get new insight into you characters.

*The interactive environment for ontoFAST is created using R package [Shiny](https://shiny.rstudio.com/). Shiny builds interactive web apps right within R. The interactive tools of ontoFAST can be run in Rstudio or a web-browser.*

![ontoFAST in interactive mode](https://raw.githubusercontent.com/sergeitarasov/ontoFAST/master/test/ontoFAST.png)
