% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/word-search.R
\name{ready_wordsearch}
\alias{ready_wordsearch}
\title{Ready wordsearch}
\usage{
ready_wordsearch(wordsearch)
}
\arguments{
\item{wordsearch}{A produced word search from `gen_word_search()` in list format, containing two elements:
'grid' and 'reference'. The function will replace all empty positions with randomly generated letters.}
}
\value{
A word search list object ready for assembly.
}
\description{
`ready_wordsearch()` takes a raw word search object and turns it into a ready format.
The same can be accomplished by passing "ready" to the type parameter of `gen_word_search()`
}
\examples{
# Raw wordsearch
words <- c("Hi", "Hello", "Howdy")
raw <- gen_word_search(words, "easy", "raw")

# Turn it ready
ready <- ready_wordsearch(raw)
}
\seealso{
[gen_word_serach()]
}
