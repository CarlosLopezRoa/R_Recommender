## R_Recommender
# Recommender system based on vectorial representation of wordfrequencies of documents

# Loading libraries
library(tm)
library(SnowballC)   

# Please specify the Directory in which this file is.
fp = file.path("~","Dropbox","Github","R_Recommender")

#Import texts
setwd(fp)
dir(fp)
books <- Corpus(DirSource(file.path(fp,"texts")))

# The book names
booksnames = c("Alice's Adventures in Wonderland, by Lewis Carroll", "Metamorphosis, by Franz Kafka
Translated by David Wyllie", "Picture of Dorian Gray, by Oscar Wilde", "The Adventures of Tom Sawyer, Complete by
               Mark Twain", "Gutenberg EBook of Dracula, by Bram Stoker", "Frankenstein, by Mary Wollstonecraft","Brothers Karamazov by Fyodor
               Dostoyevsky", "Siddhartha, by Herman Hesse", "Don Quixote, by Miguel de Cervantes", "The jungle book, by Rudyard Kipling" )
# And emotions
emotions <- c("Anger","Fear", "Hapiness","Sadness","Shame")

## Preprocessing steps
#Remove Punctuation signs, numbers, lowecase and stopwords in english
books <- tm_map(books,removePunctuation)
books <- tm_map(books, removeNumbers)   
books <- tm_map(books, tolower)   
books <- tm_map(books, removeWords, stopwords("english"))
#Stemize the words and remove unnecesary whitespaces 
books <- tm_map(books, stemDocument)   
books <- tm_map(books, stripWhitespace)   
#Change datatype
books <- tm_map(books, PlainTextDocument)   

## Processing steps
#Compute the Term frquecuency matrix
matrix <- DocumentTermMatrix(books)   
# Cast is from sparse matrix to a normal matrix
wordMatrix =as.matrix(  matrix )

#Function to find the emotion of document vec 
emotioner <- function (vec) 
{ r = {}
  for(j in 1:5)   
  { #Compute the dot product of the book with each emotion (emotions are the last five documents)  
    r[j] = vec %*% wordMatrix[10+j,]
  }   
# Normalize the result
r = r/sum(r) 
# Return the proportion of each emotion in the document
return(r)
}

#Compute the emotion in each document
p = matrix(, nrow = 10, ncol = 5)
for(j in 1:10)
{
p[j,] = emotioner(wordMatrix[j,])
}
#Change the names of rows and columns accordinglu
colnames(p) <- emotions
rownames(p) <- booksnames
#See the result of the proportion of each emotion in every document
print (p)

# Anger      Fear  Hapiness    Sadness      Shame
# Alice's Adventures in Wonderland, by Lewis Carroll                   0.2543353 0.3352601 0.2196532 0.16763006 0.02312139
# Metamorphosis, by Franz Kafka\nTranslated by David Wyllie            0.1897810 0.2700730 0.4817518 0.05839416 0.00000000
# Picture of Dorian Gray, by Oscar Wilde                               0.2703777 0.3558648 0.2345924 0.10139165 0.03777336
# The Adventures of Tom Sawyer, Complete by\n               Mark Twain 0.2179837 0.2452316 0.2970027 0.18528610 0.05449591
# Gutenberg EBook of Dracula, by Bram Stoker                           0.2485137 0.2639715 0.3543401 0.11058264 0.02259215
# Frankenstein, by Mary Wollstonecraft                                 0.2349138 0.1767241 0.3836207 0.16594828 0.03879310
# Brothers Karamazov by Fyodor\n               Dostoyevsky             0.2900097 0.2332687 0.2929195 0.07953443 0.10426770
# Siddhartha, by Herman Hesse                                          0.1831683 0.2326733 0.4752475 0.08415842 0.02475248
# Don Quixote, by Miguel de Cervantes                                  0.2641129 0.1895161 0.3501344 0.11895161 0.07728495
# The jungle book, by Rudyard Kipling                                  0.3152709 0.3497537 0.1970443 0.11822660 0.01970443


## Testing
# Create two users, one with random profile and one that reads more Happy books
u1 = runif(5, 0, 1)
u2 = c(.1,.1,1,.1,.1)

#Normalize them
u1 = u1/sum(u1)
u2 = u2/sum(u2)

#Produce the recommendation
recommend <- function(u)
{ r = p %*% u
  return (r)
}

#Preferences of the user 1
recommend(u1)
#Preferences of the user 2
recommend(u2)

# [,1]
# Alice's Adventures in Wonderland, by Lewis Carroll                   0.2196532
# Metamorphosis, by Franz Kafka\nTranslated by David Wyllie            0.4817518
# Picture of Dorian Gray, by Oscar Wilde                               0.2345924
# The Adventures of Tom Sawyer, Complete by\n               Mark Twain 0.2970027
# Gutenberg EBook of Dracula, by Bram Stoker                           0.3543401
# Frankenstein, by Mary Wollstonecraft                                 0.3836207
# Brothers Karamazov by Fyodor\n               Dostoyevsky             0.2929195
# Siddhartha, by Herman Hesse                                          0.4752475
# Don Quixote, by Miguel de Cervantes                                  0.3501344
# The jungle book, by Rudyard Kipling                                  0.1970443


#We will recommend the n top elements in this vector, if n=2

##Recommendation to user 2

#Metamorphosis, by Franz Kafka\nTranslated by David Wyllie            0.4817518
#Siddhartha, by Herman Hesse                                          0.4752475

## Automatic evaluation of user profile. 
# Let's say that user 3 reads books 1,2 & 3, what profiles this gives him. Well it's just

my_books = p[1:3,]

# A function to renurmalize the empirical frequencies of the books
profiler <- function(matrix)
{
  r = colSums(matrix)
  return(r/sum(r))
}
# He surely likes Fear over Shame
profiler(my_books)  

# Anger       Fear   Hapiness    Sadness      Shame 
# 0.23816467 0.32039931 0.31199915 0.10913862 0.02029825 
