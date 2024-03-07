# MetO-Xplorer
MetOXplorer is a web tool designed for exploratory data analysis of methionine sulfoxide (MetO) data, enabling you to uncover the regulatory potential of this reversible post-translational modification. With MetO-Xplorer, you can easily analyze and interpret MetO data to gain insights into protein function and cellular processes.

With MetO-Xplorer, you can:
* Identify significant changes in methionine oxidation with ease,
* Perform enrichment analysis of gene ontology (GO) terms to gain further insights,
* Explore the role of methionine oxidation in cellular regulation and redox signaling

Visit the web application at: https://yilmazs.shinyapps.io/MetO-Xplorer/
## Running Locally
To run MetO-Xplorer locally on your R installation, simply run:
```
library(shiny)
runGitHub("meto-xplorer", "serhan-yilmaz")
```

Note that, this will require the relevant R packages. To install them, consider running the following in your R installation: 
```
if(!require("devtools"))
  install.packages("devtools")
devtools::source_url("https://github.com/serhan-yilmaz/RokaiXplorer/blob/main/src/deploy_from_github.R?raw=TRUE")
installDependencies()
```
For more details on installation of R packages and dependencies, check out the steps #1 and #4 in [a related tutorial](https://github.com/serhan-yilmaz/RokaiXplorer/tree/main/deploy). 
