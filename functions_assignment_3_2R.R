## This file is published on github:
## https://github.com/BartekSadowski/PIPS/blob/main/functions_assignment_3_2R.R

## Functions to Assignmnet 3.2R
## 2024 Programming in Psychological Science (PIPS)
#
# Record of Revisions
#
# Date        Programmer            Descriptions of Change
# ====        ================      ======================
# 27-Jan-24   Bartłomiej Sadowski   Original code


# Function to remind me of my shopping list
remind_me <- function(){
  shopping <- data.frame(
    to_buy = c("bread rolls", "butter", "ham", "cheese", "milk", "cucumbers", "tomatos", "potatos"),
    amount = c(6, 200, 200, 400, 2, 3, 5, 3),
    units = c("units", "grams", "grams", "grams", "liters", "units", "units", "kilograms")
  )
  return(shopping)
}

# Function to show the solution to a given exercise in assignment 3.1.
# The solutions are stored in a separate external file
# After loading, every line of the solution code is stored in a different cell
# in a data frame.
cheat <- function(exercise) {
  solutions <-
    read.delim(
      file = "https://raw.githubusercontent.com/BartekSadowski/PIPS/main/solutions.txt",
      header = FALSE,
      sep = "\n",
      quote = ""
    )
  
  exercise_counter = 0
  answer = ""
  #Loop to merge lines within the required solution
  for (i in solutions[, 1]) {
    if (i == "-----------------------------------------------------------------------") {
      exercise_counter = exercise_counter + 1
    } else {
      if (exercise_counter == exercise) {
        answer <- paste(answer, i, sep = "\n")
      }
      if (exercise_counter > exercise)
        break
      }
  }
  cat(answer)
}

# Function to create random art
make_art <- function(seed,
                     art_type = "random",
                     outer_canvas = 1000,
                     inner_canvas = outer_canvas) {
  
  if(inner_canvas > outer_canvas) {
    stop("Sorry, your art cannot be generated. inner_canvas cannot be larger that outer_canvas")
  }
  
  if (!missing(seed)) {
    set.seed(seed) # Set a random seed when given by an user
  }
  
  # Load and install when necessary libraries needed
  if (!require(reshape2)) {
    install.packages("reshape2")
    library(reshape2)
  }
  if (!require(ggplot2)) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  if (!require(omnibus)) {
    install.packages("omnibus")
    library(omnibus)
  }
      
  # Set a canvas
  art <- matrix(nrow = outer_canvas, ncol = outer_canvas)
  initial <- floor((outer_canvas - inner_canvas) / 2) + 1
  
  # Draw an art type if not specified
  if (art_type != "lines" &&
      art_type != "angles" &&
      art_type != "squares" && art_type != "patches") {
    art_type <- sample(c("lines", "angles", "squares", "patches"), 1)
  }
  
  # Prompt what will be generated
  print(paste0("art type: ", art_type))
  print("creating...")
  
  
  if (art_type == "lines") {
    # Fill the first column of the inner canvas with the same value
    art[initial:(outer_canvas - initial), initial] <- 500
    for (i in initial:(inner_canvas + initial - 1)) {
      for (j in ((initial + 1):(inner_canvas + initial - 1))) {
        # Within each row, the value of the next element is the value of the
        # previous element with some randomness added
        art[i, j] <- art[i, j - 1] + rnorm(1, mean = 0, sd = 50)
      }
    }
  }

  if (art_type == "angles") {
    # Fill the origin (left-down corner) of the inner canvas with one value
    art[initial, initial] <- 500
    for (i in initial:(inner_canvas + initial - 2)) {
      for (j in initial:i) {
        # Fill directly adjacent cells on the top and on the right with the same
        # value with some randomness added
        if (i == j) {
          art[i, j + 1] <- art[i, i] + rnorm(1, mean = 0, sd = 50)
          art[i + 1, j + 1] <-
            art[i, i] + rnorm(1, mean = 0, sd = 50)
          art[i + 1, j] <- art[i, i] + rnorm(1, mean = 0, sd = 50)
        }  else {
          art[j, i + 1] <- art[j, i] + rnorm(1, mean = 0, sd = 50)
          art[i + 1, j] <- art[i, j] + rnorm(1, mean = 0, sd = 50)
        }
      }
    }
  }
  
  if (art_type == "squares") {
    number_of_colors <- sample(5:20, 1)
    square_colors <- 1:number_of_colors
    
    square_size <-
      sample(floor(inner_canvas / 500):floor(inner_canvas / 10), 1)
    
    squares_in_line <- floor(inner_canvas / square_size)
    
    coords <- c((outer_canvas - inner_canvas) / 2,
                (outer_canvas - inner_canvas) / 2)
    
    for (i in 1:squares_in_line) {
      for (j in 1:squares_in_line) {
        # Create square areas of the same value within the canvas
        art[coords[1]:(coords[1] + square_size), coords[2]:(coords[2] + square_size)] <-
          sample(square_colors, 1)
        coords <- coords + c(square_size, 0)
      }
      coords <-
        c((outer_canvas - inner_canvas) / 2, coords[2] + square_size)
    }
    # What colours will the squares have
    fillings <- sample(colors(), number_of_colors) # colours that will be used
    
    # Possible rotation of the art
    rotate <- sample(c(TRUE, FALSE), 1)
    if (rotate) {
      angle <- runif(1, min = 0, max = 360)
      art <- rotateMatrix(art, angle)
      art[is.nan(art)] = NA
    }
  }

  
  if (art_type == "patches") {
    number_of_colors <- sample(5:20, 1)
    fillings <- sample(colors(), number_of_colors) # colours that will be used
    iterations <- sample(5:150, 1) # How many patches
    
    for (i in 1:iterations)
    {
      blob_color <- sample(1:number_of_colors,1)
      size <- sample(10:300, 1)
      # Draw initial position of the patch (the origin)
      blob <- runif(2, initial, inner_canvas - size + initial)
      for (j in 1:size) {
        for(k in 1:size) {
          # Patches are drawn like squares but the further a point is from the
          # origin, the lesser is the chance that a point will be in fact filled
          # with a colour
          p <- (size - min(c(j,k)) - 1)/size
          draw <- sample(1:100, 1)
          if(100*p > draw) {
            art[blob[1]+j, blob[2]+k] <- blob_color
          }
        }
      }
    }
  }
  
  # Plotting
  
  # For squares and patches: many different colours
  
  if (art_type == "squares" || art_type == "patches") {
    art_data <- melt(art) # transform the matrix to the relevant data frame
    print("plotting...")
    x11() # Open the plot in a new window
    p <- ggplot(art_data, aes(x = Var2, y = Var1)) +
      geom_raster(aes(fill = factor(round(value)))) +
      scale_fill_manual(values = fillings, na.value = "white") + # colours
      theme( # No other elements in the plot
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
    print(p)
  }
  
  # For lines and angles: colours from a gardient between two colours
  
  gradient_colors <- sample(colors(),2) # Draw colours of the gradient
  
  if (art_type == "lines" || art_type == "angles") {
    art_data <- melt(art) # transform the matrix to the relevant data frame
    print("plotting...")
    x11() # Open the plot in a new window
    p <- ggplot(art_data, aes(x = Var2, y = Var1)) +
      geom_raster(aes(fill = value)) +
      scale_fill_gradient(low = gradient_colors[1], # colours
                          high = gradient_colors[2],
                          na.value = "white") +
      theme( # No other elements in the plot
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      )
    
    print(p)
  }
  print("Done!")
}
