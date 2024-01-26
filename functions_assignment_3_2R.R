remind_me <- function(){
  shopping <- data.frame(
    to_buy = c("bread rolls", "butter", "ham", "cheese", "milk", "cucumbers", "tomatos", "potatos"),
    amount = c(6, 200, 200, 400, 2, 3, 5, 3),
    units = c("units", "grams", "grams", "grams", "liters", "units", "units", "kilograms")
  )
  return(shopping)
}

cheat <- function(exercise) {
  solutions <- read.delim(file = "https://raw.githubusercontent.com/BartekSadowski/PIPS/main/solutions.txt", header = FALSE, sep = "\n", quote = "")
  exercise_counter = 0
  answer = ""
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


make_art <- function(seed,
                     art_type = "random",
                     outer_canvas = 1000,
                     inner_canvas = 1000) {
  if(!missing(seed)) {
    set.seed(seed)
  }
  
  if(!require(reshape2)){
    install.packages("reshape2")
    library(reshape2)
  }
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  if(!require(omnibus)){
    install.packages("omnibus")
    library(omnibus)
  }
      
  #Setting canvas
  art <<- matrix(nrow = outer_canvas, ncol = outer_canvas)
  initial <<- floor((outer_canvas - inner_canvas) / 2) + 1
  
  if (art_type != "lines" && art_type != "angles" && art_type != "squares" && art_type != "patches") {
    art_type <- sample(c("lines", "angles", "squares", "patches"),1)
  }
  
  print(art_type)
  if (art_type == "lines") {
    art[initial:(outer_canvas - initial), initial] <<- 500
    for(i in initial:(inner_canvas + initial - 1)) {
      for(j in ((initial + 1):(inner_canvas + initial - 1))) {
        art[i,j] <<- art[i, j - 1] + rnorm(1, mean = 0, sd = 50)
      }
    }
  }

  if (art_type == "angles") {
    art[initial, initial] <<- 500
    for (i in initial:(inner_canvas + initial - 2)) {
      for (j in initial:i) {
        if (i == j) {
          art[i, j + 1] <<- art[i, i] + rnorm(1, mean = 0, sd = 50)
          art[i + 1, j + 1] <<- art[i, i] + rnorm(1, mean = 0, sd = 50)
          art[i + 1, j] <<- art[i, i] + rnorm(1, mean = 0, sd = 50)
        }  else {
          art[j, i + 1] <<- art[j, i] + rnorm(1, mean = 0, sd = 50)
          art[i + 1, j] <<- art[i, j] + rnorm(1, mean = 0, sd = 50)
          
        }
      }
    }
  }
  
  if (art_type == "squares") {
    number_of_colors <- sample(5:20, 1)
    square_colors <<- 1:number_of_colors
    square_size <<- sample(floor(inner_canvas/200):floor(inner_canvas/10), 1)
    squares_in_line <<- floor(inner_canvas / square_size)
    coords <<- c((outer_canvas - inner_canvas) / 2, (outer_canvas - inner_canvas) / 2)
    for(i in 1:squares_in_line) {
      for(j in 1:squares_in_line) {
        art[coords[1]:(coords[1]+square_size), coords[2]:(coords[2]+square_size)] <<- sample(square_colors, 1)
        coords <<- coords + c(square_size, 0)
      }
      coords <<- c((outer_canvas - inner_canvas) / 2, coords[2] + square_size)
    }
    fillings <<- sample(colors(), number_of_colors)
    rotate <- sample(c(TRUE, FALSE),1)
    if(rotate) {
      angle <- runif(1, min = 0, max = 360)
      art <- rotateMatrix(art, angle)
      art[is.nan(art)] = NA
    }
  }

  
  if (art_type == "patches") {
    number_of_colors <- sample(5:20, 1)
    fillings <<- sample(colors(), number_of_colors)
    iterations <<- sample(5:150, 1)
    for (i in 1:iterations)
    {
      blob_color <<- sample(1:number_of_colors,1)
      size <<- sample(10:300, 1)
      blob <<- runif(2, initial, inner_canvas - size + initial)
      for (j in 1:size) {
        for(k in 1:size) {
          p <<- (size - min(c(j,k)) - 1)/size
          draw <<- sample(1:100, 1)
          if(100*p > draw) {
            art[blob[1]+j, blob[2]+k] <<- blob_color
          }
        }
      }
    }
  }
  
  #Plotting
  if (art_type == "squares" || art_type == "patches") {
    art_data <<- reshape2::melt(art)
    x11()
    p <<- ggplot(art_data, aes(x = Var2, y = Var1)) + 
      geom_raster(aes(fill=factor(round(value)))) + 
      scale_fill_manual(values=fillings, na.value = "white") + 
      theme(axis.line=element_blank(), axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
    print("printing..")
    print(p)
    #ggsave("plot.png", width = 1000, height = 1000, units = "px")
  }
  gradient_colors <- sample(colors(),2)
  if (art_type == "lines" || art_type == "angles") {
    art_data <<- melt(art)
    x11()
    p <- ggplot(art_data, aes(x = Var2, y = Var1)) + 
      geom_raster(aes(fill=value)) + 
      scale_fill_gradient(low = gradient_colors[1], high=gradient_colors[2], na.value = "white") + 
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
    
    print(p)
  }
}
