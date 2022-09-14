# Assignment 8
# Description: stringr and regex text processing in R using Ulysses Chapter 02 dataset.
# Niall Guerin
# Student ID: 18235079

# Code on GitHub per lecturer note so stopwords code is done for us
# http://xpo6.com/list-of-english-stop-words/
# http://edrub.in/CheatSheets/cheatSheetStringr.pdf
# https://cran.r-project.org/web/packages/stringr/stringr.pdf
# https://r4ds.had.co.nz/strings.html
library(stringi)
library(stringr)
library(tidyverse)

stopwords <- c("a", "about", "above", "above", "across", "after", "afterwards", "again",
               "against", "all", "almost", "alone", "along", "already", "also","although",
               "always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another",
               "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",
               "at", "back","be","became", "because","become","becomes", "becoming", "been",
               "before", "beforehand", "behind", "being", "below", "beside", "besides",
               "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can",
               "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe",
               "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either",
               "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every",
               "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify",
               "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty",
               "found", "four", "from", "front", "full", "further", "get", "give", "go", "had",
               "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby",
               "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however",
               "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its",
               "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made",
               "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover",
               "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely",
               "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone",
               "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one",
               "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves",
               "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re",
               "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she",
               "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow",
               "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system",
               "take", "ten", "than", "that", "the", "their", "them", "themselves", "then",
               "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon",
               "these", "they", "thickv", "thin", "third", "this", "those", "though", "three",
               "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward",
               "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us",
               "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence",
               "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon",
               "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole",
               "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you",
               "your", "yours", "yourself", "yourselves", "the")

invalid_characters <- c("--","\\?","\\!","\\.",",","'",":")

# read the file
f_pre <- readLines("/Users/niallguerin/rprogramming/week8-string-text-processing/Chapter02.txt")

# validation of file load
# str(f_pre)
# f_pre[1:10]

# BEGIN function library
convert_to_words_vector <- function(x){
  # I initially split on single white space but could never get it below 1:4513 based on stack overflow
  # standard solution but this is not really how I would normally split a csv in e.g. java.
  # After seeing the extract all tip in the discussion forum went back to this and based on
  # Hadley Wickham advice on preferring word boundaries over simplistic white space split,
  # I took approach I'd normally take with Java. Extract csv lines by SENTENCE structure first.
  # Then see have we still any unusual formatting. I could see an empty sentence " " when viewing
  # the initial extraction when using unlist of the boundary sentence result. Then strip any
  # case where the vector element is ONLY a single white space here among the list of sentences.
  # Finally unlist and only then split the sentence into word tokens using space as separator.
  # I had a slightly cleaner version I felt of this but was coming up with 1:4507 so based on
  # debugging this one only removes the problem sentence space.
  # https://r4ds.had.co.nz/strings.html
  a <- str_extract_all(x, boundary("sentence"))
  a <- unlist(a)
  a <- a[a != " "]
  a <- unlist(strsplit(a, split="\\s"))
  a
}

preprocess_text <- function(x){
  #   # https://cran.r-project.org/web/packages/stringr/stringr.pdf for stringr functions to use
  # remove invalid characters. str_remove_all will do this for us so loop over the invalid
  # characters and removing corresponding character instance from the chapter text
  # use seq_along where possible per R online texts on if using loop over apply - section: Looping Patterns
  # http://adv-r.had.co.nz/Functionals.html#lapply
  for(i in seq_along(invalid_characters))
  {
    x <- str_remove_all(x, invalid_characters[i])
  }
  
  # now remove empty characters. let stringi/stringr packages do the work for us
  x <- stri_remove_empty(x)
  
  # convert vector contents to lowercase: again let stringr do it for us
  x <- str_to_lower(x)
  
  # remove all stopwords (contained in variable stopwords)
  # Used below as template as it is fast and succinct and lends itself to further function creation for reusability
  # Web Reference: https://stackoverflow.com/questions/35790652/removing-words-featured-in-character-vector-from-string/35790955
  # I have not unlisted per the SO thread solution as I already unlisted previously before I passed into my preprocessing function
  # find value of x NOT in stopwords list - must have switched case before you run this line
  # I debugged and removed paste from that sample after initially using paste, then using paste0 following R help docs as so need to do it
  # and then commented out completely and cleaned environment as this single line does exactly what we want.
  x <- x[!x %in% stopwords]
  x
}

create_text_analysis <- function(x){
  word_pattern_analysis <- tibble(Words = "", Pattern = "", WLength = 1:1332)
  
  # creating 3 vectors sized in advance which we assign to the word_pattern_analysis tibble
  # I am setting fixed sizes in advance per recommendation from Wickham that in absence of apply the optimal
  # loop type is to set size in advance to prevent suboptimal loop resizing in memory with the typical loop.
  words <- vector(mode="character", length=1332)
  regex_pattern <- vector(mode="character", length=1332)
  word_length <- vector(mode="integer", length=1332)
  
  # obtain a unique set of tokens so the counts match the assignment tibble spec result counts
  x <- unique(x)
  
  # parse the processed text and build up the patterns and store them for each unique word token
  for(i in seq_along(x))
  {
    words[i] = x[i]
    # use paste to add the regex patterns for ^ and $ on either side of the word element per spec
    # and use word itself as the separator argument
    regex_pattern[i] = paste("^", "$", sep = x[i])
    word_length[i] = str_length(x[i])
  }
  # word_pattern_analysis: assign the vectors to the tibble columns using column references
  # do this here with fixed sizes as we will fail at this point based on tibble dimensions or
  # end up with NA so any exceptions should be caught early.
  word_pattern_analysis$Words <- words
  word_pattern_analysis$Pattern <- regex_pattern
  word_pattern_analysis$WLength <- word_length
  # return the ans tibble
  word_pattern_analysis
}

create_word_freq_analysis <- function(x){
  # using dplyr to do the heavy lifting here after originally using convoluted loop structure.
  # Web Reference: https://stackoverflow.com/questions/26784361/dplyr-put-count-occurrences-into-new-variable?rq=1
  # GitHub source code: dplyr: https://github.com/tidyverse/dplyr/blob/master/R/count-tally.R
  word_freq_analysis <- x %>% group_by(WLength = x$WLength) %>% tally()
  # GitHub dplyr source says they don't have option to name on initial call so you've to rename after
  # I used below based on online samples on SO threads as my rename which is shorter was acting up for me.
  colnames(word_freq_analysis)[colnames(word_freq_analysis) == 'n'] <- 'WFrequency'
  
  # return the frequency tibble
  word_freq_analysis
}

# END function library

# Task 1: Tokenize the chapter from sentences to individual word tokens.
f_pre_vec <- convert_to_words_vector(f_pre)

# Task 2: Perform preprocessing, invalid character removal, empty string removal, stopword removal.
f_post <- preprocess_text(f_pre_vec)

# Task 4: Show analysis based on regex search patterns of words from chapter 2.
# create a tibble with 3 columns - the words, the regex pattern for that word, the length of the word
ans <- create_text_analysis(f_post)

# Task 5: Show frequency of the word length based on result tibble in task 4.
freq <- create_word_freq_analysis(ans)

# Task 6: Plot the word frequency y-axis by the word length x-axis.
freq_word_plot <- ggplot(data = freq, aes(x = WLength, y = WFrequency)) +
  geom_point(color='blue') +
  ylab("Word Frequency") +
  xlab("Word Length") +
  labs(title="", subtitle="Chapter 2: Ulysses: Word Frequency based on Word Length", colour="") + geom_line(linetype = 1)