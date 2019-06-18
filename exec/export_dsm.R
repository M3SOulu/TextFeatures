library(TextFeatures)
library(wordVectors)

GetDSM <- function(senti4sd.env) {
  dsm <- rJava::.jfield(senti4sd.env$vector.reader,
                        "Ljava/util/Map;", name="memory")
  keys <- sapply(dsm$keySet()$toArray(), rJava::.jstrVal)
  values <- t(sapply(as.list(dsm$values()$toArray()), TextFeatures::GetVector))
  rownames(values) <- keys
  as.VectorSpaceModel(values)
}

InitSenti4SD()

system.time(dsm <- GetDSM(senti4sd.env))
gc()

system.time(write.binary.word2vec(dsm, "dsm.bin"))
## system.time(saveRDS(dsm, "dsm.rds"))
