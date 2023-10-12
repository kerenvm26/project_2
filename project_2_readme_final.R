
library(rmarkdown) #To load package

# Render the R Markdown file to README.md
rmarkdown::render("project_2_final.Rmd", output_format = "github_document", output_file = "README.md")


