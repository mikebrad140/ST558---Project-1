library(rmarkdown)

# Script to render the R Markdown file to generate the README.md output
rmarkdown::render("ST558_Project1.Rmd", output_format = "github_document", output_file = "README.md")
