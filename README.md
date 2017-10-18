![Build Status](https://travis-ci.org/sergeitarasov/ontoFAST.svg?branch=main)

# ontoFAST
### This is the R package for interactive, semi-automatic annotation of character matrices (character statements) with biological ontologies. 

The package provides convenient and efficient tools that allow linking character statements used in phylogenetics, with anatomy and phenotype ontologies. It also provides functions to query linked characters using ontological relationships. The prime aim of this package is to stimulate integration of comparative phylogenetic methods with ontological approaches.

#### The workflow:
1. Get your character statements and ontology which you want to use to link the characters with.
2. Run automatic annotation of characters and ontology terms in R.
3. Automatic annotations are not perfect, so post-process them manually using interactive environment straight from R.
4. The annotations are done! Save them for future use or query them to get new insight into you characters.

*The interactive environment for ontoFAST is created using R package [Shiny](https://shiny.rstudio.com/). Shiny builds interactive web apps right within R. The interactive tools of ontoFAST can be run in Rstudio or a web-browser.*

![ontoFAST in interactive mode](https://raw.githubusercontent.com/sergeitarasov/ontoFAST/main/test/ontoFAST.jpg)
