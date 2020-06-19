# <img src="sms/assets/img/logo_harvard_150.png" height="50" width="42" alt="HMS LINCS Center"> The Small Molecule Suite 

The Small Molecule Suite is designed by the [Harvard Program in Therapeutic Sciences](http://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology/about/)to help researchers with data-driven solutions in their studies of
molecular probes and drugs. To date, the suite has three distinct but related applications: Selectivity, Similarity and
Library. The data underlying the applications is obtained from the public, open-access database [ChEMBL (version 25)]
(https://doi.org/10.6019/CHEMBL.database.25) as well as pre-publication data from the
[Laboratory of Systems Pharmacology](https://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology/about/) at Harvard Medical School.

The Selectivity application lets researchers find the most selective probe for a protein of their choice. The similarity app
can help with the hit to lead optimization by showing which other compounds are similar or different in their structure,
induced phenotype or target affinity spectrum. The library app helps researchers to build Mechanism of Action libraries,
aiming at understanding the roles of a set of proteins in a biological system.

All of the web applications are implemented in [R](https://www.r-project.org/), using the
[Shiny](https://shiny.rstudio.com/) framework for interactive applications.

## Publication

Nienke Moret, Nicholas A. Clark, Marc Hafner, Yuan Wang, Eugen Lounkine, Mario
Medvedovic, Jinhua Wang, Nathanael Gray, Jeremy Jenkins, Peter K. Sorger.
<a href = "https://www.cell.com/cell-chemical-biology/fulltext/S2451-9456(19)30073-X" target="_blank">
Cheminformatics Tools for Analyzing and Designing Optimized
Small-Molecule Collections and Libraries.</a> *Cell Chem Biol* (2019).
doi:10.1016/j.chembiol.2019.02.018

## Development and links

Design/idea by [Nienke Moret](https://scholar.harvard.edu/nienkemoret) 


R code by [Nienke Moret](https://scholar.harvard.edu/nienkemoret) and [Clemens Hug](https://scholar.harvard.edu/clemenshug)


Shiny/R web application development by [ZevRoss Spatial Analysis](www.zevross.com)
and [Clemens Hug](https://scholar.harvard.edu/clemenshug) patterned after an earlier application created by
[Nicholas Clark](https://github.com/NicholasClark) (U of Cincinnati - LINCS data coordination and integration center)


The package [morgancpp](https://github.com/labsyspharm/morgancpp), developed by
[Artem Sokolov](https://scholar.harvard.edu/artem-sokolov/home),
[Jeremy Muhlich](https://scholar.harvard.edu/jmuhlich/home) and
[Clemens Hug](https://scholar.harvard.edu/clemenshug), is used for the computation
of tanimoto distances between compound fingerprints.


[RDKit](https://www.rdkit.org/) was used for the computation of molecular fingerprints.
[MolVS](https://molvs.readthedocs.io/en/latest/) was used for salt stripping and tautomer canonicalization
[Chemfp](https://chemfp.com/) was used for canonicalizations in earlier versions of the app.


Data on commercial availability was sourced from [ZINC](http://zinc15.docking.org/).


Supervision by [Peter Sorger](https://sorger.med.harvard.edu/people/peter-sorger-phd/)


This work was supported by NIH grants **U54-HL127365**, **U24-DK116204** and **U54-HL127624**.

## License
This work is published under the 
[Creative Commons Attribution-Share Alike 3.0 Unported License](https://creativecommons.org/licenses/by-sa/3.0/)


