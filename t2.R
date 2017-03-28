library(stringr)
df$GenreList <- strsplit(tolower(str_replace_all(df$Genre , pattern = "[[:punct:]]", "")), split = " ")
genreDictionary <- unique(unlist(df$GenreList, recursive=FALSE)) # 26 unique genres
df$GenreBinary <- c(seq(1:nrow(df)))

for(i in 1:nrow(df))
{
  g <-  c(df[i, "GenreList"][[1]])
  length(g) <- 26
  df[i,]$GenreBinary <- g
}
by(df, 1:nrow(df), function(x) x$GenreList[[1]] <- extend(x$GenreList[[1]], 26))

cps = VCorpus(VectorSource(df$Genre))
mapped = tm_map(cps, content_transformer(tolower))
mapped = tm_map(mapped, removeWords, c("N/A"))
mapped = tm_map(mapped, removePunctuation)
tdm = DocumentTermMatrix(mapped)
genreMatrix = as.data.frame(as.matrix(tdm))
mrgd = merge(genreMatrix, movies_data, by=0, all=TRUE)

############################################################################################
genreCorpus <- Corpus(VectorSource(df$Genre))
genreCorpus <- tm_map(genreCorpus, content_transformer(tolower))
genreCorpus <- tm_map(genreCorpus, removeWords, c("na","n/a"))
genreCorpus <- tm_map(genreCorpus, removePunctuation )
genreList <- lapply(genreCorpus, as.character)
genreList <- paste(genreList, collapse = " ")
genreList <- strsplit(genreList, split = " ")
genreList <- unlist(genreList)
genreList <- Filter(function(x) !identical("",x), genreList)
genreList <- unique(genreList)

genreDictionary <- strsplit(as.character(genreCorpus[1:4520]$content) , " ")
genreDictionary <- unlist(genreDictionary, recursive=FALSE)
genreDictionary <- unique(genreDictionary)
# Cross match to string lists and return true/false based on the existence of elements from list1 in list2
# Usage Example:
# getGenreBinary(list("documentary", "biography", "romance"), list("documentary", "biography", "romance", "short"))
# [1] TRUE TRUE TRUE FALSE
crossMatchStringLists <- function(list1, list2){
  sapply(list1, function(x) any(sapply(strsplit(x, ' '), function(y)
    any(sapply(list2, function(x1) any(sapply(strsplit(x1, ' '),
                                              function(y1) any(y1 %in% y))))))))
}
# Do some preprocessing for the arguments before calling crossMatchStringLists function
# Usage Example:
# getGenreBinary("Documentary, Biography, Romance", list("documentary", "biography", "romance", "short", "thriller"))
# [1] TRUE TRUE TRUE FALSE FALSE
getGenreBinary <- function(x, genreDictionary){
  genreString <- x['Genre']
  print(genreString)
  genreString <- tolower(genreString)
  genreString <- stri_replace_all_charclass(genreString, "\\p{WHITE_SPACE}", "")
  genreString <- strsplit(genreString, ",")
  print(genreString)
  print(genreDictionary)
  result <- crossMatchStringLists(genreDictionary, genreString)
  print(result)
  result <- as.vector(result)
  print(result)
  x['Genre'] <- result
  print(x['Genre'])
  return(result)
}
# This line crashes the R session in RStudio, so I stopped here on this question
# apply(movies_merged, 1, getGenreBinary, genreDictionary = genreDictionary)






# Q4 - Random Scatterplots

mainDir <- getwd()
dir.create(file.path(mainDir, "GTID_903060599"), showWarnings = FALSE)
setwd(file.path(mainDir, "GTID_903060599"))
results <- data.frame()

matrixSizes = c(1:10 %o% 10^(1:4))
fileExtensions = c("ps", "pdf", "jpeg", "png")
for(matrixSize in matrixSizes)
{
  df = data.frame(matrix(runif(2*matrixSize), nrow = matrixSize, ncol = 2))
  names(df) = c("X", "Y")
  chart = ggplot(df, aes(X, Y)) + geom_point()
  tmp = c()
  for(fileExtension in fileExtensions)
  {
    fileName = paste(matrixSize, ".", fileExtension, sep = "")
    ggsave(file=fileName)
    tmp <- c(tmp, as.numeric(file.info(fileName)$size))
  }
  tmp <- c(matrixSize, tmp)
  results <- rbind(results, data.frame(matrix(tmp, ncol = length(fileExtensions)+1, byrow = TRUE)))
}                         
names(results) = c("n",fileExtensions)

unlink(file.path(mainDir, "GTID_903060599"), recursive = TRUE)
setwd(mainDir)

ggplot(results, aes(n)) +
  geom_point(data=results, aes(y=ps, color="ps")) +
  geom_line(data=results, aes(y=ps, color="ps")) +
  geom_point(data=results, aes(y=pdf, color="pdf")) +
  geom_line(data=results, aes(y=pdf, color="pdf")) +
  geom_point(data=results, aes(y=jpeg, color="jpeg")) +
  geom_line(data=results, aes(y=jpeg, color="jpeg")) +
  geom_point(data=results, aes(y=png, color="png")) +
  geom_line(data=results, aes(y=png, color="png")) +
  labs(title="Number of scatter plot total number of points vs. Saved plot file size",
       x="Number of random points",
       y="File size in bytes")


########################################################################
IsIntlRoute = function(route){
  if((route$Stope == 0) &&
     (as.character(airports[airports$IATO == route$SourceAirport, "Country"]) != as.character(airports[airports$IATO == route$DestinationAirport, "Country"])))
    return(TRUE)
  else
    return(FALSE)
}
