# Small Molecule Suite Website

This repository contains the R Shiny code powering
https://smallmoleculesuite.org.

<p float="left">
<img src="www/sms/assets/img/dcic.png" height="50" width="85" alt="LINCS DCIC">
HMS-LINCS Small Molecule Suite Applications
<img src="www/sms/assets/img/logo_harvard_150.png" height="50" width="42" alt="HMS LINCS Center">
</p>

This website consists of three related applications that were developed as part
of the [NIH LINCS Consortium](http://www.lincsproject.org/) in a collaboration
between Harvard Medical School's [Sorger Lab](http://sorger.med.harvard.edu/)
and [Laboratory of Systems Pharmacology
(LSP)](http://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology/about/)
and the University of Cincinnati's [Laboratory for Statistical Genomics and
Systems Biology](http://eh3.uc.edu/). The former (HMS) is one of the [LINCS Data
and Signature Generation Centers
(DSGCs)](http://www.lincsproject.org/LINCS/centers/data-and-signature-generating-centers)
and the latter (Cincinnati) is part of the [BD2K-LINCS Data Coordination and
Integration Center (DCIC)](http://lincs-dcic.org/). All of the web applications
are implemented in [R](https://www.r-project.org/), using the
[Shiny](https://shiny.rstudio.com/) framework for interactive applications.

## Publication

Nienke Moret, Nicholas A. Clark, Marc Hafner, Yuan Wang, Eugen Lounkine, Mario
Medvedovic, Jinhua Wang, Nathanael Gray, Jeremy Jenkins, Peter K. Sorger.
<a href = "https://www.cell.com/cell-chemical-biology/fulltext/S2451-9456(19)30073-X" target="_blank">
Cheminformatics Tools for Analyzing and Designing Optimized
Small-Molecule Collections and Libraries.</a> *Cell Chem Biol* (2019).
doi:10.1016/j.chembiol.2019.02.018

## Development and links

Design/idea by [Nienke Moret](https://scholar.harvard.edu/nienkemoret) and
[Marc Hafner](https://scholar.harvard.edu/hafner) (HMS - LINCS data and signature generation center)


R code by [Nienke Moret](https://scholar.harvard.edu/nienkemoret) (HMS - LINCS data and signature generation center)
and [Clemens Hug](https://scholar.harvard.edu/clemenshug) (HMS - Laboratory of Systems Pharmacology)


Shiny/R web application development by [ZevRoss Spatial Analysis](www.zevross.com)
and [Clemens Hug](https://scholar.harvard.edu/clemenshug)  (HMS - Laboratory of Systems Pharmacology)
patterned after an earlier application created by
[Nicholas Clark](https://github.com/NicholasClark) (U of Cincinnati - LINCS data
coordination and integration center)


The package [morgancpp](https://github.com/labsyspharm/morgancpp), developed by
[Artem Sokolov](https://scholar.harvard.edu/artem-sokolov/home),
[Jeremy Muhlich](https://scholar.harvard.edu/jmuhlich/home) and
[Clemens Hug](https://scholar.harvard.edu/clemenshug), is used for the computation
of tanimoto distances between compound fingerprints.


[RDKit](https://www.rdkit.org/) was used for the computation of molecular fingerprints.


Data on commercial availability was sourced from [ZINC](http://zinc15.docking.org/).


Supervision by [Peter Sorger](https://sorger.med.harvard.edu/people/peter-sorger-phd/)
(HMS - LINCS data and signature generation center)


Icon design and development by [Vasileios Stathias](http://ccs.miami.edu/team_member/vasileios-vas-stathias/)
(U of Miami - LINCS data coordination and integration center)

This work was supported by NIH grants **U54-HL127365**, **U24-DK116204** and **U54-HL127624**.

## Related web-tools

**LINCS data portal** - A unified resource for accessing all LINCS dataset packages and entities.
<br>http://lincsportal.ccs.miami.edu/dcic-portal/

**iLINCS** - A data analysis platform aimed at developing statistical methods and computational tools for integrative analysis of the data produced by the LINCS program.
<br>http://www.ilincs.org/

## BD2K-LINCS Project <img src="www/sms/assets/img/dcic.png" height = "50" width= "85" alt="BD2K-LINCS">

**HMS-LINCS Small Molecule Library**<br>http://lincs.hms.harvard.edu/db/sm/

**NIH LINCS Consortium**<br>http://www.lincsproject.org/

**HMS LINCS Center**<br>http://lincs.hms.harvard.edu/

**LINCS Data and Signature Generation Centers (DSGCs)**<br>http://www.lincsproject.org/LINCS/centers/data-and-signature-generating-centers

**BD2K-LINCS Data Coordination and Integration Center (DCIC)**<br>http://lincs-dcic.org/<br>

## Harvard Medical School <img src="www/sms/assets/img/logo_harvard_150.png" height = "50" width = "42" alt = "Harvard Medical School">

**Laboratory of Systems Pharmacology (LSP)**<br>http://hits.harvard.edu/the-program/laboratory-of-systems-pharmacology/

**Sorger Lab**<br>http://sorger.med.harvard.edu/

## University of Cincinnati <img src="www/sms/assets/img/uc_logo_crop.png" height = "50" width ="64"  alt = "University of Cincinnati">

**Laboratory for Statistical Genomics and Systems Biology**<br>http://eh3.uc.edu/
