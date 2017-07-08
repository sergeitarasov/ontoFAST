setwd("~/my-papers-2017/phyloBayesHMM/ontoFast/ontoFast/data")
nex.data=read.nexus.data("sharkey_fused.nex")

str(hao.obo)

names(hao.obo)
print(hao.obo)
class(hao.obo)

hao.obo$is_a[1:10]

annotated.char.list=table2list(char.dt, 1, 3:9)

char_of=cbind(list2edges(annotated.char.list, col_order_inverse=F), "char_of")
is_a=cbind(list2edges(hao.obo$is_a, col_order_inverse=F), "is_a")
part_of=cbind(list2edges(hao.obo$"BFO:0000050", col_order_inverse=F), "part_of")

conct=rbind(char_of, is_a, part_of)
write.csv(conct, file="onto-chars.csv")
gt.name("HAO:0002482", hao.obo)



onto_chars=list2edges(annotated.char.list, col_order_inverse=T)

onto_chars[,2][onto_chars[,1]%in%get_descendants(hao.obo, c("HAO:0000494", "HAO:0002482"), exclude_roots = F)]

hao.obo$annot_characters=annotated.char.list
ontology=hao.obo
