# Cross-cultural Somatic Simulation

This repository contains an R simulation designed to analyze consumer purchasing behavior across different cultural contexts, focusing on the choice between ecological ("eco") products and standard ones.

## Project Content

- `simulation.R`: Main script containing the simulation logic, utility functions, and graph generation.
- `LICENSE`: Code license (MIT).
- `data_LICENSE_CC-BY-4.0.txt`: License for generated data and results.

## Simulation Description

The script simulates agents with different cultural profiles:
- **Collectivism vs. Individualism**

For each profile, it evaluates the utility agents gain from purchasing products, considering factors such as price and somatic or ecological impact. Results are visualized through expenditure and accumulated utility graphs over time.

## Requirements

To run the simulation, you will need R installed along with the following libraries:

```r
install.packages(c("ggplot2", "biganalytics", "ggthemes", "gridExtra", 
                   "extrafont", "scales", "grid", "tidyverse", "MASS"))
```

## Execution

Simply run the `simulation.R` file in your R environment (or RStudio). The script will generate several `.png` files with the visual results of the simulation, such as:

- `Graph_Expenditure_Colect_ff.png`
- `Graph_Expenditure_NO_Colect_ff.png`
- Other comparative charts for limits and expenditure.
