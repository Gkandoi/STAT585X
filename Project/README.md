All code, data, analysis and results from my course project for [STAT585X: Data Technologies for Statistical Analysis](https://stat585-at-isu.github.io/index.html) at Iowa State University will be made available here.

To run the app through RStudio, run the following commands:

```{r}
if (!require('shiny', 'ggplot2', 'plotly', 'readr')) install.packages("shiny", "ggplot2", "plotly", "readr")
if (!require('lubridate')) install.packages("lubridate")
shiny::runGitHub("STAT585X", "Gkandoi", subdir = "Project")
```