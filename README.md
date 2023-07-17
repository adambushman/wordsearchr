# {wordsearchr} Package <img src="https://github.com/adambushman/wordsearchr/blob/main/word-searchr_package_hex.png" align="right" width="300"/>

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

Now your words are mapped and stored in a list object, comprised of a "grid" (the area to search) and the "reference" (the words to find).

To render the data structure in a graphical format, we can call `render()`. We'll get a `{ggplot2}` object back that we can style to our heart's content. You'll want to render both the grid and the reference separately.

```
p_grid <- render(wordsearch$grid)
p_grid

p_ref <- render(wordsearch$reference)
p_ref

```

With the objects ready, we can assemble our wordsearch (with the grid on top and the reference on the bottom) by callilng `assemble()`. It makes use of the `{patchwork}` package on the back end to make them into a single `{ggplot2}` object.

```
full_ws <- assemble(p_grid, p_ref) # Be sure to pass the grid, then the reference
full_ws
```

And voilla! You have your word search. Congratulations!

For additional examples and documentation, refer to the vignette.

# Credits

Authored and Maintained by Adam Bushman
