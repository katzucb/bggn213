Class 12: Structural Bioinformatics Drug Discovery
================

Clean up our protein target structure
-------------------------------------

First we download a target (i.e. protein receptor) structure from the main PDB database. We will pick PDB ID "1hsg"

``` r
library(bio3d)

pdb.code <- "1hsg"
file.name <- get.pdb(pdb.code)
```

    ## Warning in get.pdb(pdb.code): ./1hsg.pdb exists. Skipping download

Extract the protein only segment of this PDB entry and write out a new PDB format file. We will also do the same for the bound ligand.

``` r
hiv <- read.pdb(file.name)
hiv
```

    ## 
    ##  Call:  read.pdb(file = file.name)
    ## 
    ##    Total Models#: 1
    ##      Total Atoms#: 1686,  XYZs#: 5058  Chains#: 2  (values: A B)
    ## 
    ##      Protein Atoms#: 1514  (residues/Calpha atoms#: 198)
    ##      Nucleic acid Atoms#: 0  (residues/phosphate atoms#: 0)
    ## 
    ##      Non-protein/nucleic Atoms#: 172  (residues: 128)
    ##      Non-protein/nucleic resid values: [ HOH (127), MK1 (1) ]
    ## 
    ##    Protein sequence:
    ##       PQITLWQRPLVTIKIGGQLKEALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYD
    ##       QILIEICGHKAIGTVLVGPTPVNIIGRNLLTQIGCTLNFPQITLWQRPLVTIKIGGQLKE
    ##       ALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYDQILIEICGHKAIGTVLVGPTP
    ##       VNIIGRNLLTQIGCTLNF
    ## 
    ## + attr: atom, xyz, seqres, helix, sheet,
    ##         calpha, remark, call

Protein extraction first

``` r
prot <- trim.pdb(hiv, "protein")
prot
```

    ## 
    ##  Call:  trim.pdb(pdb = hiv, "protein")
    ## 
    ##    Total Models#: 1
    ##      Total Atoms#: 1514,  XYZs#: 4542  Chains#: 2  (values: A B)
    ## 
    ##      Protein Atoms#: 1514  (residues/Calpha atoms#: 198)
    ##      Nucleic acid Atoms#: 0  (residues/phosphate atoms#: 0)
    ## 
    ##      Non-protein/nucleic Atoms#: 0  (residues: 0)
    ##      Non-protein/nucleic resid values: [ none ]
    ## 
    ##    Protein sequence:
    ##       PQITLWQRPLVTIKIGGQLKEALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYD
    ##       QILIEICGHKAIGTVLVGPTPVNIIGRNLLTQIGCTLNFPQITLWQRPLVTIKIGGQLKE
    ##       ALLDTGADDTVLEEMSLPGRWKPKMIGGIGGFIKVRQYDQILIEICGHKAIGTVLVGPTP
    ##       VNIIGRNLLTQIGCTLNF
    ## 
    ## + attr: atom, helix, sheet, seqres, xyz,
    ##         calpha, call

``` r
prot.filename <- paste(pdb.code, "_protein.pdb", sep = "")
write.pdb(prot, file =prot.filename)
```

Do the same for ligand

``` r
lig <- trim.pdb(hiv, "ligand")
lig
```

    ## 
    ##  Call:  trim.pdb(pdb = hiv, "ligand")
    ## 
    ##    Total Models#: 1
    ##      Total Atoms#: 45,  XYZs#: 135  Chains#: 1  (values: B)
    ## 
    ##      Protein Atoms#: 0  (residues/Calpha atoms#: 0)
    ##      Nucleic acid Atoms#: 0  (residues/phosphate atoms#: 0)
    ## 
    ##      Non-protein/nucleic Atoms#: 45  (residues: 1)
    ##      Non-protein/nucleic resid values: [ MK1 (1) ]
    ## 
    ## + attr: atom, helix, sheet, seqres, xyz,
    ##         calpha, call

``` r
lig.filename <- paste(pdb.code, "_ligand.pdb", sep="")
write.pdb(lig, file =lig.filename)
```

Convert our docking results for viewing in VMD
----------------------------------------------

``` r
res <- read.pdb("all.pdbqt", multi = TRUE)
res
```

    ## 
    ##  Call:  read.pdb(file = "all.pdbqt", multi = TRUE)
    ## 
    ##    Total Models#: 11
    ##      Total Atoms#: 50,  XYZs#: 1650  Chains#: 1  (values: B)
    ## 
    ##      Protein Atoms#: 0  (residues/Calpha atoms#: 0)
    ##      Nucleic acid Atoms#: 0  (residues/phosphate atoms#: 0)
    ## 
    ##      Non-protein/nucleic Atoms#: 50  (residues: 1)
    ##      Non-protein/nucleic resid values: [ MK1 (1) ]
    ## 
    ## + attr: atom, xyz, calpha, call

``` r
write.pdb(res, file = "results.pdb")
```

What is the RMSD to the MK1 compound from the crystal structure
===============================================================

``` r
# res <- read.pdb("all.pdbqt", multi=TRUE)
ori <- read.pdb("ligand.pdbqt")
rmsd(ori, res)
```

    ##  [1]  0.693  4.142 10.527  3.893 10.379  3.547 11.122  8.313  5.652  5.596
    ## [11] 11.115

``` r
rmsd(res)
```

    ## Warning in rmsd(res): No indices provided, using the 50 non NA positions

    ##         [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]   [,8]   [,9]
    ##  [1,]  0.000  4.094 10.418  3.715 10.343  3.581 10.995  8.300  5.596
    ##  [2,]  4.094  0.000 11.178  5.344 11.037  4.663 10.524  8.681  3.818
    ##  [3,] 10.418 11.178  0.000 10.321  3.994 10.648  4.074 11.378 10.794
    ##  [4,]  3.715  5.344 10.321  0.000 11.016  5.028 10.797  8.273  5.184
    ##  [5,] 10.343 11.037  3.994 11.016  0.000 10.339  5.761 12.198 11.652
    ##  [6,]  3.581  4.663 10.648  5.028 10.339  0.000 10.940  9.675  6.516
    ##  [7,] 10.995 10.524  4.074 10.797  5.761 10.940  0.000 11.921 10.107
    ##  [8,]  8.300  8.681 11.378  8.273 12.198  9.675 11.921  0.000  7.775
    ##  [9,]  5.596  3.818 10.794  5.184 11.652  6.516 10.107  7.775  0.000
    ## [10,]  5.480  4.510 11.008  4.732 11.794  6.284 10.607  6.553  3.159
    ## [11,] 11.077 10.934  6.451 11.763  6.678 11.772  5.815 10.575 11.078
    ##        [,10]  [,11]
    ##  [1,]  5.480 11.077
    ##  [2,]  4.510 10.934
    ##  [3,] 11.008  6.451
    ##  [4,]  4.732 11.763
    ##  [5,] 11.794  6.678
    ##  [6,]  6.284 11.772
    ##  [7,] 10.607  5.815
    ##  [8,]  6.553 10.575
    ##  [9,]  3.159 11.078
    ## [10,]  0.000 11.114
    ## [11,] 11.114  0.000
