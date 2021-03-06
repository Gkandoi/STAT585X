All code, data, analysis and results from my course project for [STAT585X: Data Technologies for Statistical Analysis](https://stat585-at-isu.github.io/index.html) at Iowa State University will be made available here.

To run the app through RStudio, run the following commands:

**GOexplore: Explore the Gene Ontology Annotations for a species of your choice:**
```{r}
#install.packages("shiny", "ggplot2", "plotly", "readr", "lubridate")
shiny::runGitHub("STAT585X", "Gkandoi", subdir = "Project/GOexplore")
```

**GOcompare: Compare the Gene Ontology Annotations for Humans and Mouse:**
```{r}
#install.packages("shiny", "ggplot2", "plotly", "readr", "lubridate")
shiny::runGitHub("STAT585X", "Gkandoi", subdir = "Project/GOcompare")
```

Or you can clone or download this repository, change working directory to the repository (STAT585X) and run `shiny::runApp("Project/GOexplore")` or `shiny::runApp("Project/GOcompare")`.

Note: The App might take a mintue or two to load in the Gene Ontology Annotations.
