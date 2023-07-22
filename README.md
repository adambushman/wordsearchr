# {wordsearchr} Package <img src="https://github.com/adambushman/wordsearchr/blob/main/man/figures/wordsearchr_package_hex.png" align="right" width="300"/>

This package allows you to quickly make a traditional word search and style it nicely for printing. The only hard part will be coming up with the words for which to search!

# Getting Started

To get started with the package, install using `remotes`:

```         
install.packages("remotes")

remotes::install_github(
  "adambushman/wordsearchr", 
  build_vignettes = TRUE
)
```

Once installed, load the package using the `library` command below or reference individual components using the `::` framework:

```         
# Load the entire library into the session

library("wordsearchr")

# Reference library components

wordsearchr::demo_wordsearch
```

You'll find helpful functions for generating the word search and rendering it with {ggplot2}.

# Making a word search

Determine the words you'd like to use in your word search and pass them to `gen_word_search()`. Also specify the difficulty and type.

```
words <- c("Hi", "Hello", "Howdy")

wordsearch <- gen_word_search(
  words, "easy", "ready"
)

```

Now your words are mapped and stored in a list object, comprised of a "grid" (the area to search), the "reference" (the words to find), and the "solution" (data to highlight the solution).

To render the data structure in a graphical format, we can call `build_wordsearch()`. We need only pass it our wordsearch object and it will do all the heavy lifting, making use use of the `{ggplot2}` and `{patchwork}` packages.

```
p <- build_wordsearch(wordsearch)
p

```

![Completed Wordsearch](https://github.com/adambushman/wordsearchr/blob/main/man/figures/example_wordsearch.jpeg)

And voilla! You have your word search. Congratulations!

For additional examples and documentation, refer to the vignette.

# Credits

Authored and Maintained by Adam Bushman
