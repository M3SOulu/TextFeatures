[![Build Status](https://travis-ci.org/M3SOulu/TextFeatures.svg?branch=master)](https://travis-ci.org/M3SOulu/TextFeatures)

# TextFeatures

This package includes functions to compute text features to be used
for NLP and sentiment analysis with machine learning.

## Installation

<!-- From CRAN: -->

<!--     install.packages("TextFeatures") -->

With devtools:

    devtools::install_github("M3SOulu/TextFeatures")


## Example Usage

So far, the package contains mainly functions to compute different
sets of ngrams, and semantic, lexicon-based and keyword-based metrics
inspired by Senti4SD. These produces sparse matrices and data.table objects
that can be used for training a prediction model for NLP tasks such as
sentiment analysis.

    library(TextFeatures)

    # Tokenize some text with cleanNLP package using Stanford CoreNLP
    cleanNLP::cnlp_init_corenlp("en", 0)
    tokens <- CleanNLPTokens(text, label)

    # Loading DSM and SentiStrength lexicons
    dsm <- LoadDSM(file.path(datadir, "dsm.bin"))
    lexicons <- ReadLexicons(datadir, FALSE)
    negations <- fread(file.path(datadir, "NegatingWordList"), header=FALSE)$V1

    # Computing vectors for positive, negative and objective lexicons.
    vectors <- with(lexicons, PolarityVectors(positive$word,
                                              negative$word,
                                              objective$word, dsm))

    # Senti4SD inspired features
    semantic <- SemanticBasedFeatures(tokens, vectors, dsm)
    lexicon <- LexiconBasedFeatures(tokens, lexicons)
    keyword <- KeywordBasedFeatures(tokens, negations)

    # Generating sets of ngrams
    GenerateNgrams(sample.noemos$tokens, sample.noemos$label)

<!-- ## Paper and Citation -->
<!-- If you use our tool please cite our paper: -->
