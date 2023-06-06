#########################################################################
# Introduction to Regular Expressions
#
# Greg Ridgeway
#   gridge@upenn.edu
#   Professor and Chair, Department of Criminology
#   Professor, Department of Statistics and Data Science
#   University of Pennsylvania
# Summer Institute in Computational Social Science
# Philadelphia, PA
# June 6, 2023
#########################################################################


# Our destination: # How can we extract data from websites?
#    https://www.phillypolice.com/ois/


#########################################################################
# grep - {g}lobally search a {r}egular {e}xpression and {p}rint
# Created in 1973
# Used for searching for text
#########################################################################

dataText <- c("3718 Locust Walk",
              "4 Privet Drive",
              "10880 Malibu Point",
              "221B Baker St.",
              "19104-6286",
              "20015",
              "90291",
              "90210",
              "(215) 573-9097",
              "215-573-9097",
              "2155739097",
              "Kendall Roy",
              "Roman Roy",
              "Siobhan Roy",
              "Roy Wood Jr.",
              "Royal Caribbean",
              "Casino Royal",
              "two children",
              "2 children",
              "twins",
              "Philadelphia",
              "Philly",
              "Phila",
              "Dr. Phil",
              "$23456",
              "$10000")


#############################################################################
# Finding text
#############################################################################

# find the letter "a"
# give the indices that have "a"
grep("a", dataText)
# give TRUE/FALSE if it finds "a"
grepl("a", dataText)
# show the values that have "a"
grep("a", dataText, value=TRUE)

# find the number "1"
grep("1", dataText, value=TRUE)

# find numbers
#   square brackets contain lists of characters
grep("[0123456789]", dataText, value=TRUE)
grep("[0-9]", dataText, value=TRUE)

# find odd numbers
grep("[13579]", dataText, value=TRUE)

# find four numbers together
grep("[0-9][0-9][0-9][0-9]", dataText, value=TRUE)
grep("[0-9]{4}", dataText, value=TRUE)

# find capital letters
grep("[A-Z]", dataText, value=TRUE)

# find items with at least two "words" (words=stuff with letters)
#    capital A to lowercase z
grep("[A-z] [A-z]", dataText, value=TRUE)

# find ZIP codes that start with 90
grep("90[0-9]{3}", dataText, value=TRUE)
# Each digit ZIP+4 Codes (19104-6286) is associated with a location
# 1-national area, 91-large city post office, 04-associate post office
# 62-street or building, 86-side of street or floor

#############################################################################
# Exercises
#############################################################################
#1. Find text with vowels

#2. Find text with a number immediately followed by a letter

#3. What's the difference between
grep("Roy", dataText, value=TRUE)
# and
grep("[Roy]", dataText, value=TRUE)


#############################################################################
# starting, ending, not, multiples, special characters, optional 
#############################################################################

# starts with a letter
grep("^[A-z]", dataText, value=TRUE)

# ends with a letter
grep("[A-z]$", dataText, value=TRUE)

# Function of ^ depends on context
#   inside [], ^ means "not"
# find something that's not a letter or space right next to a letter
grep("[^A-z ][A-z]", dataText, value=TRUE)

# numbers with hyphens in them
#   + means at least one of the previous
grep("[0-9]+-[0-9]+", dataText, value=TRUE)


#############################################################################
# Exercises
#############################################################################
#4. Find the Roy children

#5 Text that starts with a number ends with a letter




# phone numbers
grep("[0-9]{3}-[0-9]{3}-[0-9]{4}", dataText, value=TRUE)

# regular expressions have several "special characters"
#   \ ^ $ {} [] () . * + ? | -
#   if you want to search for these "protect" them with \

#   parentheses are special need to "protect" them with \\
#   if it's special to R use one \, special to grep use \\

grep("\\([0-9]{3}\\) [0-9]{3}-[0-9]{4}", dataText, value=TRUE)
grep("[0-9]{10}", dataText, value=TRUE)

# use | as "or" to put them together
grep("[0-9]{3}-[0-9]{3}-[0-9]{4}|\\([0-9]{3}\\) [0-9]{3}-[0-9]{4}|[0-9]{10}",
     dataText, value=TRUE)

# () group together characters as words
# get those with three suspects
# long way
grep("two children|2 children", dataText, value=TRUE)
# compact way
grep("(two|2) children", dataText, value=TRUE)
# all indicators of 2 children
grep("(two|2) children|twins", dataText, value=TRUE)

# get DC and LA ZIP codes
grep("(902|200)[0-9]{2}", dataText, value=TRUE)

# The ? indicates optional text
grep("Phil(adelphia)",  dataText, value=TRUE)
grep("Phil(adelphia)?", dataText, value=TRUE)

# find four letter words
#   \b looks for boundaries (not letters or numbers)
grep("\\b[A-z]{4}\\b", dataText, value=TRUE)
# alternatively
grep("( |^)[A-z]{4}( |$)", dataText, value=TRUE)


#############################################################################
# Exercises
#############################################################################
#6 Find text that only has letters (no numbers, spaces, punctuation)

#7 Find ZIP codes

#8 Find variations on Philadelphia

#9 Find all having capitalized words

#10 Find a shorter way to get phone numbers using ?



#############################################################################
# gsub - global substitution
#   'sub' replaces first occurrence only; 'gsub' replaces all occurrences
#############################################################################


#############################################################################
# basic find/replace
#############################################################################

# remove the Roy's last names
gsub(" Roy$", "", dataText)

# remove anything that is not a number from text
gsub("[^0-9]","", dataText)

# turn all the punctuation to X
gsub("[.)($-]","X", dataText)


#############################################################################
# registers
#############################################################################

# Delete the "plus four" part of the ZIP code
#   this doesn't work since it deletes phone numbers too
gsub("-[0-9]{4}", "", dataText)
#   this doesn't work since it also deletes the first 5 numbers
gsub("[0-9]{5}-[0-9]{4}", "", dataText)
#   Use parentheses to save parts, saved as \\1, \\2, etc.
gsub("([0-9]{5})-[0-9]{4}", "\\1", dataText)

# If more than two words, keep the first two
#   . matches anything
#   * means 0 or more of the previous character
#   [^ ] matches things that are not spaces
gsub("^([^ ]+ [^ ]+) .*$", "\\1", dataText)

# standardize phone numbers
gsub("^\\(?([0-9]{3})(\\) |-)?([0-9]{3})-?([0-9]{4})",
     "\\1-\\3-\\4", dataText)


#############################################################################
# Exercises
#############################################################################
#11 Change Dr. Phil to Dr. Phillip

#12 Convert all Philadelphia variations to "Philadelphia"

#13 Add commas to to the two dollar values


#############################################################################
# Two very useful regular expressions
#  - comma separated files with commas in the data
#  - stripping HTML tags
#############################################################################

# problems with comma separated files
text.w.quotes <- "white,male,\"loitering,narcotics\",arrest,\"knife,gun\",19"
cat(text.w.quotes,"\n")
#  lookahead for blocks of text with no quotes or pairs of quotes
#  change separating commas to a stranger symbol ^, ~, _, |
#     look ahead for no quotes or sequences of text with paired quotes until the end
text.w.quotes.fixed <- gsub(",(?=([^\"]|\"[^\"]*\")*$)",";", text.w.quotes, perl=TRUE)
cat(text.w.quotes.fixed,"\n")


# get the 10,000 most common words in English
a <- scan("https://en.wiktionary.org/wiki/Wiktionary:Frequency_lists/PG/2006/04/1-10000",
          what="", sep="\n")
a[60:80]

i <- grep("title=\"", a)
a[i][1:10]
a <- a[i][1:10000+5]

# Look for < followed by text that is not > followed by >... delete matches
gsub("<[^>]*>", "", a[1])
b <- gsub("<[^>]*>", "", a)


#############################################################################
# Wordle
#############################################################################
# If you want to play Wordle in R, install the 'wordle' package
# install.packages("remotes")
# remotes::install_github("coolbutuseless/wordle")

library(wordle)
wordle_solns

play_wordle(words = wordle_solns)
# play_wordle(target_word = "")

# get all possible solutions
candidates <- wordle_solns

# three basic Wordle filters
# letters that have to be there somewhere
candidates <- grep("^(?=.*e)", candidates, value=TRUE, perl=TRUE)
# letters that must/must not be in specific locations
candidates <- grep(".[^e]...", candidates, value=TRUE)
# letters that are not in the word
candidates <- grep("[^dalt]{5}", candidates, value=TRUE)
