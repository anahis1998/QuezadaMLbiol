#function to select a palette of colors 
#The target here is define a function with two options of colors to use in a plot
#This way, I can decide if I want the plot with color blind friendly colors or not


colores <- function(type = "blind_friendly", n = 10) {
  
  if (type == "blind_fr") {
    colors <- viridis::viridis(n)
  } else if (type == "non_blind_fr") {
    colors <- rainbow(n)
  } else {
    stop("error")
  }
  
  return(colors)
}
