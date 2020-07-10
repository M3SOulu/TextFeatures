senti4sd.repo <- "https://github.com/collab-uniba/Senti4SD"

senti4sd.path.default <- function() {
  system.file("extdata", package="TextFeatures")
}

#' Download Senti4SD data
#'
#' Downloads Senti4SD jar and DSM data files.
#'
#' @param senti4sd.path Directory where to store the downloaded file.
#' @param update If FALSE, will only download files if they don't
#'   already exists.
DownloadSenti4SDData <- function(senti4sd.path=senti4sd.path.default(),
                                 update=FALSE) {
  logging::loginfo("Downloading Senti4SD data in %s", senti4sd.path)
  url <- "blob/master/ClassificationTask/Senti4SD-fast.jar?raw=true"
  filename <- file.path(senti4sd.path, "Senti4SD-fast.jar")
  if (update || !file.exists(filename)) {
    download.file(file.path(senti4sd.repo, url), filename)
  }
  url <- "blob/master/ClassificationTask/dsm.bin?raw=true"
  filename <- file.path(senti4sd.path, "dsm.bin")
  if (update || !file.exists(filename)) {
    download.file(file.path(senti4sd.repo, url), filename)
  }
}

#' Init JVM
#'
#' Initialize Java Virtual Machine for Senti4SD.
#'
#' @param senti4sd.path Directory where Senti4SD data is stored.
#' @param heap.size Maximum heap size for Java.
#' @seealso DownloadSenti4SDData
InitJVM <- function(senti4sd.path=senti4sd.path.default(), heap.size="2048m") {
  logging::loginfo("Initializing JVM with %s heap size.", heap.size)
  rJava::.jinit(file.path(path.expand(senti4sd.path), "Senti4SD-fast.jar"),
                parameters=sprintf("-Xmx%s", heap.size))
}

#' Load Senti4SD DSM
#'
#' Load Senti4SD's Distributional Semantic Model.
#'
#' @param senti4sd.path Directory where Senti4SD data is stored.
#' @return VectorReader Java object resulting from loading the DSM.
#' @seealso DownloadSenti4SDData
LoadDSMSenti4SD <- function(senti4sd.path=senti4sd.path.default()) {
  logging::loginfo("Loading Senti4SD DSM.")
  vector.reader <- rJava::new(rJava::J("di/uniba/it/tdsm/vectors/MemoryVectorReader"),
                              rJava::new(rJava::J("java/io/File"),
                                         file.path(senti4sd.path, "dsm.bin")))
  vector.reader$init()
  vector.reader
}

#' Senti4SD Polarity Vector
#'
#' Returns the PolarityVector object for Senti4SD.
#'
#' @param vector.reader VectorReader Java object.
#' @param dimension Vector dimension to use.
#' @return PolarityVector Java object.
#' @seealso LoadDSMSenti4SD
Senti4SDPolarityVector <- function(vector.reader, dimension=600L) {
  rJava::new(rJava::J("feature/semantic/PolarityVector"),
             vector.reader, dimension)
}

#' Senti4SD environment
#'
#' Environment containing Java objects needed by Senti4SD. By default
#' this is empty to NULL and needs to be initialized with
#' \code{InitSenti4SD}.
#'
#' @seealso InitSenti4SD
#' @export
senti4sd.env <- new.env(parent=emptyenv())

#' Init Senti4SD
#'
#' Initializes Senti4SD by downloading data, initializing the JVM,
#' loading the DSM and creating the PolarityVector object.
#'
#' @param senti4sd.path Directory where Senti4SD data is stored.
#' @param heap.size Maximum heap size for Java.
#' @param update If FALSE, will only download files if they don't
#'   already exists.
#' @seealso InitJVM
#' @seealso LoadDSMSenti4SD
#' @seealso Senti4SDPolarityVector
#' @seealso DownloadSenti4SDData
#' @export
InitSenti4SD <- function(senti4sd.path=senti4sd.path.default(),
                         heap.size="2048m", update=FALSE) {
  DownloadSenti4SDData(senti4sd.path, update)
  InitJVM(senti4sd.path, heap.size)
  senti4sd.env$vector.reader <- LoadDSMSenti4SD(senti4sd.path)
  senti4sd.env$polarity.vector <- Senti4SDPolarityVector(senti4sd.env$vector.reader, 600L)
}

#' Senti4SD Polarity Vectors
#'
#' Computes polarity vectors for positive, negative, subjective
#' (positive or negative) and objective (neutral) words.
#'
#' @return A list with a positive, negative, subjective, and objective
#'   vector.
#' @export
Senti4SDPolarityVectors <- function() {
  list(positive=senti4sd.env$polarity.vector$getPositiveVector(),
       negative=senti4sd.env$polarity.vector$getNegativeVector(),
       subjective=senti4sd.env$polarity.vector$getSubjectiveVector(),
       objective=senti4sd.env$polarity.vector$getObjectiveVector())
}

#' Senti4SD Get Vector
#'
#' Converts a Java vector to R vector.
#'
#' @param vector Java vector object.
#' @return Vector as an R object.
#' @export
Senti4SDGetVector <- function(vector) {
  rJava::.jcall(vector, "[F", "getCoordinates")
}

#' Senti4SD String Representation
#'
#' Computes the vector representation of a string.
#'
#' @param string String to vectorize.
#' @return Vector Java object representation of the string.
#' @seealso Senti4SDGetVector
#' @export
Senti4SDStringRepresentation <- function(string) {
  string <- paste(string, collapse=" ")
  senti4sd.env$polarity.vector$stringRepresentation(string)
  ## rJava::.jcall(senti4sd.env$polarity.vector,
  ##               "Ldi/uniba/it/tdsm/vectors/Vector",
  ##               "stringRepresentation", string)
}

#' Senti4SD Semantic based features
#'
#' Computes semantic based features.
#'
#' @param tokens Token data.table containing ids for document (id),
#'   and token word.
#' @param vectors Polarity vectors for positive, negative, objective
#'   and subjective words.
#' @return A data.table with features computed for each document.
#' @seealso InitSenti4SD
#' @seealso Senti4SDPolarityVectors
#' @export
Senti4SDSemanticBasedFeatures <- function(tokens,
                                          vectors=Senti4SDPolarityVectors()) {
  res <- tokens[, {
    v <- Senti4SDStringRepresentation(word)
    list(sim.pos=v$measureOverlap(vectors$positive),
         sim.neg=v$measureOverlap(vectors$negative),
         sim.obj=v$measureOverlap(vectors$objective),
         sim.subj=v$measureOverlap(vectors$subjective))
  }, by=id]
  setkey(res, id)
}
