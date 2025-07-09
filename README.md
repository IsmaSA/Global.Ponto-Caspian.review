# Global review of non-native aquatic Ponto-Caspian species

## Overview

This repository accompanies the paper **Global review of non-native aquatic Ponto-Caspian species** and provides the full dataset and scripts used to produce the analyses, figures, and maps.

It includes:
- A curated list of 142 non-native aquatic Ponto-Caspian (NNAPC) species
- Verified taxonomy cross-checked with GBIF
- Pathway classifications (CBD primary and secondary levels) --> See Saul et al. 2016
- Occurrence and first record data --> From GBIF
- Hydrobasin shapefiles for spatial mapping --> From Hydrobasin
- First records --> From Saffer et al. 2024

---

## Repository

PONTO CASPIAN SPECIES LIST/
│
├── SpeciesList/ 
├── Taxonomy_files/ 
├── test_files/, test-master/ 
├── .RData, .Rhistory 
│
├── AllTaxonomyGBIF.xlsx 
├── Backbone.xlsx 
├── First.records.xlsx 
├── Pathways.xlsx 
├── occurrence.txt
├── matrix2.csv 
├── RawPC.List.xlsx 
├── savedrecs.xls 
│
├── HydroRIVERS_* 
├── species_occurrence_3d_map.png 
│
├── Pathways.r 
├── Path.html, Path.pdf
│
├── Taxonomy.html, Taxonomy.pdf
├── *.svg 
└── README.md 


## How to Use

1. **Browse data**: Open the `.xlsx` files to see the species list, pathways, and first record information.
2. **Reproduce plots**: Run the `Pathways.r` script in R to generate pathway visuals.
3. **Map distributions**: Use the HydroRIVERS shapefiles (`.shp`) in GIS software to map basin-level richness.
4. **Check taxonomy**: Cross-reference with the GBIF backbone provided.

---

## Citation

If you use this dataset, please cite:

> Soto I., et al. (2025). *Global review of non-native aquatic Ponto-Caspian species*.

---

## License

This dataset is shared for non-commercial research and education. Please acknowledge the authors in any derived work.

---

## Contact

Questions? Contact:  
**Ismael Soto**  
University of South Bohemia, Faculty of Fisheries and Protection of Waters  
📧 isma-sa@hotmail.com

---

**Last updated:** July 2025