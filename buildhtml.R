library(rmarkdown)

output_dir <- "../EDSD2019data/docs"


render.this <- "../EDSD2019data/index.Rmd"
render(render.this, output_dir = output_dir, 
       params = list(output_dir = output_dir))

render.this <- "../EDSD2019data/Introductions.Rmd"
render(render.this, output_dir = output_dir, 
       params = list(output_dir = output_dir))



