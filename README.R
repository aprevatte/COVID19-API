# author: Alex Prevatte
# date: 10/5/2021
# purpose: Render README.Rmd to a .md file called README.md for my repo.

rmarkdown::render(input = "~/NCSU/ST558/Repos/ST558-Project1/README.Rmd", 
                  output_format = "github_document", 
                  output_file = "README.md",
                  output_options = list(
                    html_preview = FALSE
                  )
)