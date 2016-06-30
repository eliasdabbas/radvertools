## ------------------------------------------------------------------------
library(advertools)
carnames <- data.frame(broad = rownames(mtcars))
carnames$modifed <- kw_modified_broad(carnames$broad) 
carnames$phrase <- kw_phrase(carnames$broad)
carnames$exact <- kw_exact(carnames$phrase)
carnames$negative <- kw_negative(carnames$exact)
carnames[1:10, ]

