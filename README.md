# The Trust Game: A Historical and Methodological Analysis on the Frontier of Experimental and Behavioral Economics

[![Language](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)
[![Repo](https://img.shields.io/badge/GitHub-Repo-green.svg)](https://github.com/ncamilotto/trustgame-paper)

This repository contains the R scripts and supplementary materials for the working paper:

**Title:** *The Trust Game: A Historical and Methodological Analysis on the Frontier of Experimental and Behavioral Economics*  
**Author:** Nicolas Camilotto  

## Abstract

The Trust Game, despite its well-established importance in experimental economics, has not received the same level of historical and methodological scrutiny as other notable games like the Ultimatum or Dictator Game. This paper begins by describing the historical progression of the Trust Game's experimental framework. Subsequently, it identifies two main uses of the game within experimental and behavioral economics. By leveraging concepts from the philosophy of science, the paper characterizes the differences between these uses and highlights the pivotal importance of result validity. This repository provides the code to replicate the bibliometric network analysis that forms the basis of this study.

## Repository Structure

```text
.
├── R/
│   ├── 01_data_preprocessing.R   # Script for cleaning and preparing citation data
│   └── 02_alluvial_diagram.R     # Script for network construction and visualization
├── data/
│   └── database_WOS.RData         # The dataset required to run the analysis
└── README.md
```

## Methodology

The analysis presented in the paper is a **quantitative microhistory** of the Trust Game literature. It uses bibliographic coupling to map the intellectual structure of the field and trace its evolution over time. The workflow is divided into two main stages, each corresponding to an R script in this repository.

### 1. Data Collection and Preprocessing (`01_data_preprocessing.R`)

This script handles the crucial first step of cleaning and structuring the raw bibliographic data exported from the Web of Science (WoS).

-   **Data Source**: The corpus was collected from WoS using the query: `TOPIC: ("Trust Game") OR TOPIC: ("Investment Game")` for the publication years 1995–2024.
-   **Citation Cleaning**: The script parses the raw citation strings (`CR` field from WoS) to extract individual references for each paper in the corpus.
-   **Reference Disambiguation**: To accurately identify unique cited works, a hierarchical matching process is implemented. Since citation data can be noisy (e.g., typos, missing info), the script creates several composite keys for each reference:
    -   `AYP` (Author, Year, Pages)
    -   `AYV` (Author, Year, Volume)
    -   `ARV` (Author, Revue, Volume)
    -   `ARP` (Author, Revue, Pages)
    -   `AYR` (Author, Year, Revue)
-   **Unique ID Assignment**: Unique IDs are assigned to references starting with the most reliable identifier (DOI) and progressively using the composite keys to match remaining references. This ensures that different string representations of the same cited work are grouped together.
-   **Output**: The script produces a clean edge list (`Edges_before_coupling`) where each row connects a source paper (`Noeud`) to a uniquely identified cited reference (`ID_citation`).

### 2. Network Analysis and Visualization (`02_alluvial_diagram.R`)

This script takes the clean edge list and generates the dynamic network analysis visualized in Figure 4 of the paper.

-   **Bibliographic Coupling**: The script first calculates the bibliographic coupling strength between all pairs of papers in the corpus. Two papers are coupled if they cite the same references. The weight of the link corresponds to the similarity of their bibliographies.
-   **Dynamic Network Construction**: To analyze the evolution of the research field, a series of networks are built using a **10-year sliding time window**.
-   **Community Detection**: Within each temporal network, research communities (clusters) are identified using the **Leiden algorithm**, an advanced method for community detection that optimizes modularity.
-   **Intertemporal Cluster Matching**: The script then tracks the evolution of these clusters over time by merging clusters from consecutive time windows that share a significant number of papers (similarity threshold > 0.51). This allows for the creation of a continuous "flow" of research traditions.
-   **Alluvial Diagram**: Finally, the script uses the `ggalluvial` package to generate the alluvial diagram. This visualization effectively shows:
    -   The relative size of each research cluster at different points in time.
    -   The merging, splitting, and persistence of intellectual communities.
    -   The overall dynamic landscape of the Trust Game literature.

## Data Availability

The data required to reproduce this analysis is provided in this repository in the file `data/database_WOS.RData`.

This file was generated from a Web of Science (WoS) export based on the query `TOPIC: ("Trust Game") OR TOPIC: ("Investment Game")` for the years 1995–2024. In accordance with WoS data sharing policies, this file contains **only the columns necessary** to perform the analysis and does not include the full, unabridged record for each publication.

## How to Reproduce the Analysis

This project is fully reproducible. Follow these steps:

### 1. Setup

First, clone this repository to your local machine:
```sh
git clone https://github.com/ncamilotto/trustgame-paper.git
cd trustgame-paper
```

### 2. Install Dependencies

Open an R session in the project's root directory and run the following command to install all the required packages:

```R
install.packages(c(
  "stringr", "tibble", "dplyr", "tidyr", "purrr",  # Tidyverse for data manipulation
  "biblionetwork", "networkflow",                  # For bibliometric network analysis
  "forcats", "ggplot2", "ggalluvial"             # For data visualization
))
```

### 3. Run the Scripts

Execute the R scripts sequentially. The first script prepares the data, and the second one performs the analysis and generates the final plot.

```R
# In your R console:
source("R/01_data_preprocessing.R")
source("R/02_alluvial_diagram.R")
```

After running the second script, the alluvial diagram from the paper will be generated.

## Citation

If you use the code or methods from this repository in your research, please cite the original paper:

```bibtext
TBD
```
