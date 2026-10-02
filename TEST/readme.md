# Test dataset

DNA methylation data from the retinoblastoma study of Ryl et al. (2024), GEO accession [GSE267015](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE267015). Met4All can be tested on it from raw IDATs, which runs the full pipeline, or from the authors' pre-computed beta matrix.

| | From IDATs | From the beta matrix |
|---|---|---|
| Samples | 68 (59 EPIC, 9 450K) | 61 tumours |
| Download | [GSE267015.zip](https://github.com/gcampof/Met4All/releases/download/v1.0/GSE267015.zip), 886 MB | [GEO supplementary file](https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE267015&format=file&file=GSE267015%5Fryl%5Fet%5Fal%5Fbeta%5Fmatrix%5Ftumors%2Etxt%2Egz) |
| Samplesheet | included in the zip | `targets.csv`, in this folder |
| Analyses | all, including QC, beta distribution and CNV | MDS, PCA, UMAP, heatmap, global and differential methylation |

## From IDATs

1. Download [GSE267015.zip](https://github.com/gcampof/Met4All/releases/download/v1.0/GSE267015.zip) (886 MB, 1.6 GB unzipped).
2. In Met4All, choose IDATs and upload the zip as it is.

The zip contains the 136 IDAT files from GEO (two per sample) and `GSE267015_samplesheet_merged.csv`, the GEO sample metadata. Met4All finds both by itself, sorts the samples by array type, processes each array and merges them.

A complete analysis peaks at about 5 GB of memory and uses about 3.9 GB of disk (see [Resource requirements](../README.md#resource-requirements)).

## From the beta matrix

1. Download `GSE267015_ryl_et_al_beta_matrix_tumors.txt.gz` from [GEO](https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE267015&format=file&file=GSE267015%5Fryl%5Fet%5Fal%5Fbeta%5Fmatrix%5Ftumors%2Etxt%2Egz).
2. From the repository root, convert it to CSV and zip it with `targets.csv`:

   ```bash
   gunzip -c GSE267015_ryl_et_al_beta_matrix_tumors.txt.gz | tr '\t' ',' > beta_matrix.csv
   zip -j GSE267015_beta.zip beta_matrix.csv TEST/targets.csv
   ```

3. In Met4All, choose beta matrix, upload `GSE267015_beta.zip`, and use `Sample_Name` as the sample ID in the analyses.

The authors processed this matrix themselves: reduced to the probes shared by 450K and EPIC, normalised with minfi's funnorm, and batch-corrected per array type with ComBat. `targets.csv` adds curated clinical annotation: methylation cluster, heritability, sex, diagnosis, laterality, age at diagnosis, and RB1 and MYCN status.

## Citation

If you use this dataset, please cite:

Ryl T, et al. *A MYCN-driven de-differentiation profile identifies a subgroup of poor-prognosis retinoblastoma with therapeutic vulnerabilities*. Nature Communications. 2024. [PMID 39079981](https://pubmed.ncbi.nlm.nih.gov/39079981/)
