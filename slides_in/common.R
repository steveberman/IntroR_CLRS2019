library(tidyverse)
library(knitr)

knitr::opts_chunk$set(
  warning = TRUE
  , error = TRUE
  , echo = TRUE
  , message = FALSE
  , fig.height = 4.5
  , fig.pos = "t"
  , collapse = TRUE
)
knitr::opts_knit$set(root.dir = normalizePath('../'))