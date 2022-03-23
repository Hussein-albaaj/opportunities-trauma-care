1. No need to load the same package multiple times. I suggest you load
   all packages you need in the beginning of the code, that way it's
   easy for other people to see what packages they'll need to install,
   instead of having to realize halfway through that some packages are
   missing.
2. Subsetting a data.frame can be done using the native R api, you could replace `nks<-select(sega,cat.var,cont.var)` with `nks <- sega[, c(cat.var, cont.var)]`. 
3. Replacing specific values with NA can also be done with the native
   R api, for example: `xy$res_survival[xy$res_survival == 999] <-
   NA`. That way, if you have a list of variables with values that you
   want to replace with NA, you could loop over that list, and avoid
   repeating the same code:
   
   ```{r}
   ## This code could replace line 24-35, provided you added all
   ## variables to the values.to.replace list
   values.to.replace <- list(res_survival = c(999, 99),
                             intub = 999,
                             host_care_level = 999)
   change_to_na <- function(data, value) {
       data[data == value] <- NA
       return (data)
   }
   xy[names(values.to.replace)] <- lapply(names(values.to.replace), function(variable.name) {
       data <- xy[, variable.name]
       values <- values.to.replace[[variable.name]]
       for (value in values) data <- change_to_na(data, value)
       return (data)
   })
   ```
4. If you want to cross-reference tables and figures within the
   manuscript you can do that provided you use the package bookdown's
   output format. So instead of `word_document` you would do
   `bookdown::word_document2: default`, and then cite a table like:
   Table \@ref(tab:missing=data) with the code chunk:
   
   ```{r missing-data, echo = FALSE}
   knitr::kable(sewe[,1:2], format = "markdown", caption = "Missing values per variable")
   ```
5. The `table1` package seems to produce a html table, right? If you
   want to include it in your paper you probably want to use a
   markdown table. You can use the function below to convert the html table to markdown:
   
   ```{r}
   create_table1 <- function(x, ...) {
       table <- as.character(table1(x, ...))
       tmpfile.html <- tempfile(fileext = ".html")
       write(table, tmpfile.html)
       md.table <- rmarkdown::pandoc_convert(tmpfile.html, from = "html", to ="markdown")
       unlink(tmpfile.html)
       return (md.table)
   }

   ```
   
   
   
