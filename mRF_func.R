myRF_func <- function (object, training.genes = NULL, training.classes = NULL, 
                  verbose = TRUE, ...) 
{
  require(Seurat)
  require(ranger)
  training.classes <- as.vector(x = training.classes)
  training.genes <- SetIfNull(x = training.genes, default = rownames(x = object@data))
  training.data <- as.data.frame(x = as.matrix(x = t(x = object@data[training.genes, 
                                                                     ])))
  training.data$class <- factor(x = training.classes)
  if (verbose) {
    print("Training Classifier ...")
  }
  classifier <- ranger(data = training.data, dependent.variable.name = "class", 
                       classification = TRUE, write.forest = TRUE, ..., probability = TRUE)
  return(classifier)
}