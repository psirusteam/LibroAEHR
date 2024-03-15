--- 
title: "Análisis de encuestas de hogares con R"
author: "Andrés Gutiérrez^[Experto Regional en Estadísticas Sociales - Comisión Económica para América Latina y el Caribe (CEPAL) -  andres.gutierrez@cepal.org], Cristian Téllez^[Profesor - Universidad Santo Tomás - cristiantellez@usta.edu.co], Stalyn Guerrero^[Consultor - Comisión Económica para América Latina y el Caribe (CEPAL), guerrerostalyn@gmail.com]"
date: "2024-03-15"
documentclass: book
bibliography: [CEPAL.bib]
biblio-style: apalike
link-citations: yes
colorlinks: yes
lot: yes
lof: yes
fontsize: 12pt
geometry: margin = 3cm
header-includes: \usepackage[spanish, spanishkw, onelanguage, linesnumbered]{algorithm2e}
github-repo: psirusteam/LibroAEHR
description: "Este es el repositorio del libro *Análisis de encuestas de hogares con R*."
knit: "bookdown::render_book"
lang: es
linkcolor: blue
output:
  pdf_document:
    toc: true
    toc_depth: 3
    keep_tex: true
    latex_engine: xelatex
  gitbook:
    df_print: kable
    css: "style.css"
# Compilar así:
# bookdown::render_book("index.Rmd", "bookdown::pdf_book")
# bookdown::render_book("index.Rmd", "bookdown::epub_book")
# bookdown::render_book("index.Rmd", "bookdown::word_document2")
# bookdown::preview_chapter("01.Rmd", "bookdown::word_document2")
---



# Prefacio {-}




La versión online de este libro está licenciada bajo una [Licencia Internacional de Creative Commons para compartir con atribución no comercial 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/). 

Este libro es el resultado de un compendio de las experiencias internacionales prácticas adquiridas por los autores y será usado como insumo principal en un documento de CEPAL. Se agradecen los comentarios que puedan surgir, escribiendo al correo electrónico del primer autor. 
