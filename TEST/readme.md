# Test Data

This folder contains sample metadata and references for the test datasets. Two public GEO datasets are used, covering different Illumina methylation array versions.

| Dataset | Array | Samples | Use |
|---|---|---|---|
| GSE267015 | EPIC (v1) | Retinoblastoma tumours | Default test dataset |
| GSE240469 | EPIC v2.0 | Peripheral blood, healthy individuals | EPIC v2 testing |

---

## Dataset 1 — GSE267015 (EPIC v1)

### Metadata

Sample metadata is compiled in:

```text
targets.csv
```

### Beta Matrix Data

The beta matrix table can be downloaded from GEO using the following link:

https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE267015&format=file&file=GSE267015%5Fryl%5Fet%5Fal%5Fbeta%5Fmatrix%5Ftumors%2Etxt%2Egz

Alternatively, it can be accessed through the GEO accession page:

https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE267015

Supplementary file:

```text
GSE267015_ryl_et_al_beta_matrix_tumors.txt.gz
```

### Citation

If you use this test dataset, please cite:

Ryl T, et al. *A MYCN-driven de-differentiation profile identifies a subgroup of poor-prognosis retinoblastoma with therapeutic vulnerabilities*. Nature Communications. 2024.

PubMed: https://pubmed.ncbi.nlm.nih.gov/39079981/

GEO accession: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE267015

---

## Dataset 2 — GSE240469 (EPIC v2.0)

This dataset is used to test support for the Infinium MethylationEPIC v2.0 BeadChip. It contains genome-wide DNA methylation profiles from peripheral blood of 24 healthy individuals.

### Metadata

Sample metadata is compiled in:

```text
targets_GSE240469.csv
```

### Data

The supplementary files can be downloaded from the GEO accession page:

https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE240469

Supplementary file:

```text
GSE240469_RAW.tar
```

### Citation

If you use this test dataset, please cite the GEO accession:

GSE240469. *Analysis of genome-wide DNA methylation patterns in peripheral blood of healthy individuals*. Gene Expression Omnibus. 2024.

GEO accession: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE240469
