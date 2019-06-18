library(data.table)

senti4sd.gold <- fread("Senti4SD_GoldStandard_EmotionPolarity.csv")
senti4sd.gold[, final := factor(final, levels=c("positive", "neutral",
                                                "negative"))]
Encoding(senti4sd.gold$text) <- "UTF-8"
setkey(senti4sd.gold, label)
usethis::use_data(senti4sd.gold, overwrite=TRUE)
