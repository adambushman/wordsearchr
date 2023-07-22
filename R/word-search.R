#' Make Tiles
#'
#' `reorder_words()` reorders a vector of strings by their character length.
#'
#' @param words A vector of character strings.
#' @seealso [gen_word_search()]
#' @returns A vector of words sorted by their character length.
#' @examples
#' words = c("Hi", "Hello", "Howdy")
#'
#' reorder_words(words)
reorder_words <- function(words) {
  data <- data.frame(
    len = nchar(words),
    order = 1:length(words)
  )

  data <- data[order(data$len),]

  return(words[rev(data$order)])
}

#' Make Tiles
#'
#' `get_mapping()` determines the possible positioning of the target word within
#' the word search matrix.
#'
#' @param dims The dimensions of the matrix in number of rows and number of columns.
#' @param word A character string for mapping its placement in the word search matrix.
#' @param orientation A character string detailing its mapping direction, featuring
#' combinations of "top", "bottom", "left", and "right".
#' @seealso [gen_word_search()]
#' @returns A list of elements detailing the start position for mapping the word
#' and the its direction.
#' @import stringr
#' @examples
#' words = c("Hi", "Hello", "Howdy")
#'
#' reorder_words(words)
get_mapping <- function(dims, word, orientation) {

  # Vertical start
  if(stringr::str_detect(orientation, "down")) {
    v_pos = sample(1:(dims[1] - stringr::str_length(word)), 1)
  } else if(stringr::str_detect(orientation, "up")) {
    v_pos = sample(stringr::str_length(word):dims[1], 1)
  } else {
    v_pos = sample(1:dims[1], 1)
  }

  # Horizontal start
  if(stringr::str_detect(orientation, "right")) {
    h_pos = sample(1:(dims[2] - stringr::str_length(word)), 1)
  } else if(stringr::str_detect(orientation, "left")) {
    h_pos = sample(stringr::str_length(word):dims[2], 1)
  } else {
    h_pos = sample(1:dims[2], 1)
  }

  # Vertical change
  if(stringr::str_detect(orientation, "down")) {
    v_chng = 1
  } else if(stringr::str_detect(orientation, "up")) {
    v_chng = -1
  } else {
    v_chng = 0
  }

  # Horizontal change
  if(stringr::str_detect(orientation, "right")) {
    h_chng = 1
  } else if(stringr::str_detect(orientation, "left")) {
    h_chng = -1
  } else {
    h_chng = 0
  }

  return(list(
    start = c(v_pos, h_pos),
    change = c(v_chng, h_chng)
  ))
}

#' Clean up word search matrix
#'
#' `clean_up()` cleans a produced word search matrix, turning empty positions with
#' "-" into randomly generated characters.
#'
#' @param search A produced word search matrix.
#' @seealso [gen_word_search()]
#' @returns A cleaned, ready to render word search matrix.
#' @import stringr
#' @examples
#' x <- matrix(
#'   c("-", "H", "-", "I"),
#'   nrow = 2,
#'   ncol = 2,
#'   byrow = TRUE
#' )
#' print(x)
#'
#' y = clean_up(x)
#' print(y)
clean_up <- function(search) {
  for(i in 1:length(search)) {
    if(search[i] == "-") {
      search[i] = stringr::str_to_upper(sample(base::letters, 1))
    }
  }

  return(search)
}

#' Ideal dimensions
#'
#' `ideal_dims()` determines the proper dimensions for the reference matrix that
#' corresponds to the word search.
#'
#' @param size An integer for the number of words intended for the search.
#' @seealso [gen_word_search()]
#' @returns A vector of length two for the dimensions of the reference matrix in
#' number of rows and number of columns.
#' @examples
#' ideal_dims(10)
ideal_dims <- function(size) {
  row = floor(sqrt(size))
  col = ceiling(sqrt(size))

  if((row * col) < size) {
    col = col + 1
  }

  return(c(row, col))
}

#' Generate word search
#'
#' `gen_word_search()` maps a vector of words onto a matrix.
#'
#' @param words A vector of character strings (words) required for the word search.
#' @param difficulty A character string specifying the difficulty, accepting
#' "easy", "hard", and "mix" values.
#' @param type A character string identifying the type of word search, accepting
#' "ready" and "raw" values. "ready" will create a fully populated word search while
#' "raw" will leave non-mapped positions with a "-" for validation.
#' @seealso [ideal_dims(), clean_up(), get_mapping(), reorder_words(), render()]
#' @returns A list of two data frame table elements: 1) "grid", or the mapped words
#' ready for either render or validation, and 2) "reference", or the words to look
#' up in the grid.
#' @import stringr
#' @examples
#' words <- c("Hi", "Hello", "Howdy")
#'
#' # Creates a word search ready for rendering
#' wordsearch.ready <- gen_word_search(words, "easy", "ready")
#'
#' # Creates a raw word search for validation
#' wordsearch.raw <- gen_word_search(words, "easy", "raw")
#' @export
gen_word_search <- function(
    words,
    difficulty = c("easy", "hard", "mix"),
    type = c("ready", "raw")
  ) {

    if(!is.character(words) | length(words) == 0) {
      stop("A vector of character strings must be passed to 'words'")
    }

    if(length(setdiff(difficulty, c("easy", "hard", "mix"))) != 0) {
      stop("You must pass a valid difficulty value: 'easy', 'hard', 'mix'")
    }

    if(length(setdiff(type, c("ready", "raw"))) != 0) {
      stop("You must pass a valid type value: 'ready', 'raw'")
    }


    words = stringr::str_remove_all(words, " ")
    words = stringr::str_to_upper(words)
    words = reorder_words(words)

    modes = list(
      "easy" = c("down", "right", "down_right", "up_right"),
      "hard" = c("up", "left", "up_left", "down_left")
    )

    if(difficulty == "mix") {
      directions = c(modes$easy, modes$hard)
    } else {
      directions = unlist(modes[difficulty], use.names = FALSE)
    }

    size = max(stringr::str_length(words))
    search = matrix("-", nrow = ceiling(size + length(words) / 2), ncol = ceiling(size + length(words) / 2))
    solution = data.frame(matrix(nrow = 0, ncol = 3))
    colnames(solution) = c("x", "y", "Freq")

    word_orientation = sample(directions, length(words), replace = TRUE)

    for(i in 1:length(words)) {
      wrong = FALSE
      pos_data = get_mapping(dim(search), words[i], word_orientation[i])
      letters = stringr::str_split(words[i], "")[[1]]
      curr_pos = pos_data$start

      # Check for inappropriate intersections
      for(j in 1:length(letters)) {
        if(search[curr_pos[1], curr_pos[2]] != "-") {
          wrong = search[curr_pos[1], curr_pos[2]] != letters[j]
          if(wrong) {
            break
          }
        }
        curr_pos = curr_pos + pos_data$change
      }

      # Remap the words until no inappropriate intersections
      while(wrong) {
        wrong = FALSE
        pos_data = get_mapping(dim(search), words[i], word_orientation[i])
        letters = stringr::str_split(words[i], "")[[1]]
        curr_pos = pos_data$start

        # Check for inappropriate intersections
        for(j in 1:length(letters)) {
          if(search[curr_pos[1], curr_pos[2]] != "-") {
            wrong = search[curr_pos[1], curr_pos[2]] != letters[j]
            if(wrong) {
              break
            }
          }
          curr_pos = curr_pos + pos_data$change
        }
      }

      curr_pos = pos_data$start

      # Add the word
      for(j in 1:length(letters)) {
        search[curr_pos[1], curr_pos[2]] = letters[j]
        curr_pos = curr_pos + pos_data$change
      }

      solution[nrow(solution)+1,] = c(rev(pos_data$start), words[i])
      solution[nrow(solution)+1,] = c(rev(curr_pos - pos_data$change), words[i])
    }

    colnames(search) = 1:ncol(search)
    rownames(search) = 1:nrow(search)

    # Create the reference matrix
    dims = ideal_dims(length(words))
    look_up <- matrix(stringr::str_to_upper(words), dims[1], dims[2])
    colnames(look_up) = 1:ncol(look_up)
    rownames(look_up) = 1:nrow(look_up)


    if(type == "raw") {
      return(list(
        grid = as.data.frame.table(search),
        reference = as.data.frame.table(look_up),
        solution = solution
      ))
    }
    else {
      return(list(
        grid = as.data.frame.table(clean_up(search)),
        reference = as.data.frame.table(look_up),
        solution = solution
      ))
    }
}

#' Ready wordsearch
#'
#' `ready_wordsearch()` takes a raw word search object and turns it into a ready format.
#' The same can be accomplished by passing "ready" to the type parameter of `gen_word_search()`
#'
#' @param wordsearch A produced word search from `gen_word_search()` in list format, containing two elements:
#' 'grid' and 'reference'. The function will replace all empty positions with randomly generated letters.
#' @seealso [gen_word_serach()]
#' @returns A word search list object ready for assembly.
#' @import stringr purrr
#' @examples
#' # Raw wordsearch
#' words <- c("Hi", "Hello", "Howdy")
#' raw <- gen_word_search(words, "easy", "raw")
#'
#' # Turn it ready
#' ready <- ready_wordsearch(raw)
#' @export
ready_wordsearch <- function(wordsearch) {
  if(class(wordsearch) != "list" | length(setdiff(names(wordsearch), c("grid", "reference", "solution"))) != 0) {
    stop("The object you passed doesn't appear to be a wordsearch object")
  }

  get_ready <- function(x) {
    if(x == "-") {
      x = stringr::str_to_upper(sample(base::letters, 1))
    }
    return(x)
  }

  wordsearch$grid$Freq = purrr::map_chr(wordsearch$grid$Freq, get_ready)

  return(wordsearch)
}

#' Assemble complete word search
#'
#' `build_wordsearch()` creates a {ggplot2} object of the assembled, final word search
#'
#' @param wordsearch A produced word search from `gen_word_search()` in list format, containing two elements:
#' 'grid' and 'reference'. The function will replace all empty positions with randomly generated letters.
#' @param solution A logical (TRUE/FALSE) indicating if the solution should be rendered.
#' @seealso [gen_word_search()]
#' @returns An assembled {ggplot2} object with the grid, lookup reference, and solution
#' (if specified).
#' @import ggplot2 patchwork
#' @examples
#' words <- c("Hi", "Hello", "Howdy")
#' wordsearch <- gen_word_search(words, "easy", "ready")
#'
#' # Assembled wordsearch
#' build_wordsearch(wordsearch, TRUE)
#' @export
build_wordsearch <- function(wordsearch, solution = TRUE) {

  if(class(wordsearch) != "list" | length(setdiff(names(wordsearch), c("grid", "reference", "solution"))) != 0) {
    stop("The object you passed doesn't appear to be a wordsearch object")
  }

  if(!is.logical(solution)) {
    stop("You must pass TRUE/FALSE to the 'solution' parameter")
  }

  grid <-
    ggplot2::ggplot() +
    ggplot2::geom_text(
      ggplot2::aes(Var2, Var1, label = Freq),
      wordsearch$grid
    )

  if(solution) {
    grid <-
      grid +
      ggplot2::geom_line(
        ggplot2::aes(x, y, group = Freq, color = Freq),
        wordsearch$solution,
        linewidth = 7,
        lineend = "round",
        alpha = 0.3,
        show.legend = FALSE
      )
  }

  grid <-
    grid +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::theme_void()

  reference <-
    ggplot2::ggplot() +
    ggplot2::geom_text(
      ggplot2::aes(Var2, Var1, label = Freq),
      wordsearch$reference
    ) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::theme_void()

  lay <- "
    1
    1
    1
    1
    1
    2
  "

  p <-
    grid +
    reference +
    patchwork::plot_layout(design = lay)

  return(p)
}
