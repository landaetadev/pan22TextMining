# Including needed libraries
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(tidytext)
library(dplyr)
library(ggplot2)

start.time <- Sys.time()

# Preparing parameters
n <- 5000      # Número de palabras en el vocabulario. Usually used 1000 or 10000
k <- 10        # Número de pliegues en cross-validation. Usually used 10
r <- 3        # Número de repeticiones en cross-validation. Usually used 3
# YOUR TRAINING PATH
path_training <- "/Volumes/MBP2021Slave/VirtualDrives/pCloud/UPV/BigData/17TextMiningenSocialMedia/pan22Training/en"
# YOUR TEST PATH
path_test <- "/Volumes/MBP2021Slave/VirtualDrives/pCloud/UPV/BigData/17TextMiningenSocialMedia/pan22TestWithoutTruth/en"
lang <- "en"

# Auxiliar functions
# * freq_terms: Given a text, it extracts the n most frequent terms
# * Plot: Given a set of pairs term, frequency, it plots the distribution
# * GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
# * GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation

freq_terms <- function(corpus.preprocessed, n = 1000) {
  #https://dk81.github.io/dkmathstats_site/rtext-freq-words.html
  corpus.text <- data_frame(Text = corpus.preprocessed)
  corpus.words <- corpus.text %>% unnest_tokens(output = word, input = Text)
  corpus.wordcounts <- corpus.words %>% count(word, sort = TRUE)
  corpus.frequentterms <- corpus.wordcounts[1:n,]
  names(corpus.frequentterms) <- c("WORD", "FREQ")
  
  return (corpus.frequentterms)
}


Plot <- function(wordcounts) {
  wordcounts %>% 
    filter(FREQ > 70) %>% 
    mutate(WORD = reorder(WORD, FREQ)) %>% 
    ggplot(aes(WORD, FREQ)) + 
    geom_col() +
    coord_flip() +
    labs(x = "Word \n", y = "\n Count ", title = "Frequent Words \n") +
    geom_text(aes(label = FREQ), hjust = 1.2, colour = "white", fontface = "bold") +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
          axis.title.y = element_text(face="bold", colour="darkblue", size = 12))
}

# GenerateVocabulary: Given a corpus (training set), obtiene las n palabras más frecuentes
GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
  
  #Reading the truth file #####
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4)]
  colnames(truth) <- c("author","class")
  
  # Lectura de la lista de corpus de archivos
  files = list.files(pattern="*.xml")
  
  
  #Lectura del contenido de los archivos y concatenación en la variable corpus.raw
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    i <- i + 1
    if (verbose) print(paste(i, " ", file))
  }
  
  # Preprocesamiento del corpus
  corpus.preprocessed <- corpus.raw
  
  if (lowcase) {
    if (verbose) print("Reducir...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }
  
  if (punctuations) {
    if (verbose) print("Eliminando puntuaciones...")
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Eliminando números...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Desnudando los espacios en blanco...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{ #palabras vacias
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (swlist!="") { stopwords() #stopwords()
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  #####


  # Generando el vocabulario como los n términos más frecuentes
  if (verbose) print("Generating frequency terms")
  
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  
  if (verbose) Plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}

# GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation
# generar bolsa de palabras #generatebow debe tener las mismas condiciones que el procesado de palabras
GenerateBoW <- function(path, vocabulary, n = 10000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
  
  # Reading the truth file
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4)]
  colnames(truth) <- c("author", "class")
  
  i <- 0
  bow <- NULL
  # Lectura de la lista de archivos en el corpus
  files = list.files(pattern="*.xml")
  for (file in files) {
    # Obtención de información veraz para el autor actual
    author <- gsub(".xml", "", file)
    class <- truth[truth$author==author,"class"]
    
    if (class=="I") {
      class = "ironic"
    } else {
      class = "normal"
    }
    
    # Reading contents for the current author
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
    # Preprocessing the text
    if (lowcase) {
      txtdata <- tolower(txtdata)
    }
    
    if (punctuations) {
      txtdata <- removePunctuation(txtdata)
    }
    
    if (numbers) {
      txtdata <- removeNumbers(txtdata)
    }
    
    if (whitespaces) {
      txtdata <- stripWhitespace(txtdata)
    }
    
    # Construcción del modelo de espacio vectorial.
    # Para cada palabra del vocabulario, obtiene la frecuencia de aparición en el autor actual.
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      #if (length(freq[freq$WORD==word,"FREQ"])>0) {
      if (is.na(freq[freq$WORD==word, "FREQ"]$FREQ[1])) {
        0
      } else {
        thefreq <- freq[freq$WORD==word,"FREQ"]$FREQ[1]
      }
      
      line <- paste(line, ",", thefreq, sep="")
    }
    
    line <- paste(class, ",", line, sep="")
    
    # Nueva fila en la matriz del modelo de espacio vectorial
    bow <- rbind(bow, line)
    i <- i + 1
    
    if (verbose) {
      print(paste(i, author, class))
    }
  }
  
  return (bow)
}

# GENERAR VOCABULARIO


SW = c("amp","d","k","m","r","s","u","w","x","gt","rt","st","th","vs","youtube", "video", "jajaja") #PALABRAS A ELIMINAR
#vocabulary <- GenerateVocabulary(path_training, n, swlang=lang)
vocabulary <- GenerateVocabulary(path_training, n, swlang=lang , swlist=SW )
print(vocabulary)


# GENERANDO BOW PARA EL CONJUNTO DE ENTRENAMIENTO
bow_training <- GenerateBoW(path_training, vocabulary)

# PREPARACIÓN DEL MODELO DE ESPACIO VECTORIAL PARA EL CONJUNTO DE ENTRENAMIENTO
training <- concat.split(bow_training, "V1", ",")
training <- cbind(training[,2], training[,4:ncol(training)])
names(training)[1] <- "theclass"


# Learning a SVM and evaluating it with k-fold cross-validation
print("SVM") ###1
train_control <- trainControl(method="repeatedcv", number = k , repeats = r)
model_SVM <- train( theclass~., data= training, trControl = train_control, method ="svmLinear")
print(model_SVM)
vocabulary$WORD
#Plot(vocabulary)

print("boot632") ###2
train_control <- trainControl(method="boot632", number = k , repeats = r)
model_boot632 <- train( theclass~., data= training, trControl = train_control, method ="svmLinear")
print(model_boot632) ###OK

# Aprendiendo con Naive Bayes
print("Naive Bayes") ###3
train_control <- trainControl(method="cv", number = k , repeats = r)
NaiveBayes <- train( theclass~., data= training, trControl = train_control, method = "nb")
print(NaiveBayes) ###OK

# Aprendiendo con randomForest
print("Random Forest") ###4
train_control <- trainControl(method="" , number = k , repeats = r)
randomForest <- train( theclass~., data= training, trControl = train_control, method = "rf")
print(randomForest) ###OK




#print("boot_all")
#train_control <- trainControl(method ="boot_all", number = k , repeats = r)
#boot_all <- train( theclass~., data= training, trControl = train_control, method ="svmLinear")
#print(boot_all) ###OK

#print("oob")
#train_control <- trainControl(method ="oob", number = k , repeats = r)
#oob <- train( theclass~., data= training, trControl = train_control, method = "oob")
#print(oob) ###OK

#print("boot")
#train_control <- trainControl(method="boot", number = k , repeats = r)
#boot <- train( theclass~., data= training, trControl = train_control, method = "svmLinear")
#print(boot) ###OK


####################################################

# Learning a SVM with the whole training set and without evaluating it
# Aprendizaje SVM con todo el conjunto de entrenamiento y sin evaluarlo
#train_control <- trainControl(method="none")
#model_SVM <- train( theclass~., data= training, trControl = train_control, method = "svmLinear")

# GENERATING THE BOW FOR THE TEST SET
#bow_test <- GenerateBoW(path_test, vocabulary)

# Preparación del modelo de espacio vectorial y la verdad para el conjunto de prueba
#test <- concat.split(bow_test, "V1", ",")
#truth <- as.factor(unlist(test[,2]))
#test <- test[,4:ncol(test)]

# Predecir y evaluar la predicción
#pred_SVM <- predict(model_SVM, test)
#confusionMatrix(pred_SVM, truth)


#end.time <- Sys.time()
#time.taken <- end.time - start.time

#print(time.taken)


