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

# Credits

Authored and Maintained by Adam Bushman
