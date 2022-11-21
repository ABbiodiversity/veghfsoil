## code to prepare `hflt` dataset goes here

hftypes <- read.csv("lookup-hf-type-v2014.csv")
hftypes <- droplevels(hftypes[!is.na(hftypes$HF_GROUP_COMB) & !duplicated(hftypes$FEATURE_TY),])
hfgroups <- read.csv("lookup-hf-class-v2014.csv")
hflt <- hfgroups[match(hftypes$HF_GROUP_COMB, hfgroups$HF_GROUP_COMB),]
rownames(hflt) <- hftypes$FEATURE_TY

usethis::use_data(hflt, overwrite = TRUE)
