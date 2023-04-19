Sys.setenv(LANG = "en")
library(rsconnect)
library(BiocManager)

application_title = "MetO-Xplorer"
shinyapps_account = "yilmazs"

options(repos = BiocManager::repositories())
deployApp(appName = application_title, appTitle = application_title, account = shinyapps_account)


