# Dataset Path Inventory

RiboCrypt ships code, documentation, and static application assets rather than genomic datasets. Runtime datasets are supplied through ORFik experiment directories or explicit metadata paths. The paths below are derived from source code and tutorials; paths marked external are not tracked in this repository.

| Path | Filetype | Quick summary |
| --- | --- | --- |
| `~/livemount/Bio_data/ORFik_experiments` (external; `run_dev.R`) | ORFik experiment directory | Local development root containing discoverable experiment definitions and associated study/collection data. |
| `~/livemount/Bio_data/NGS_pipeline/metadata_rc.csv` (external; `run_dev.R`) | CSV | Curated sample metadata passed to the app; it must include a `Run` column. |
| `ORFik::config()["exp"]` (external, configurable) | ORFik experiment directory | Default experiment root used by `RiboCrypt_app()` and the overview vignette. Each experiment supplies annotations, library data, and derived results. |
| `<experiment>/collection_tables_indexed/coverage_index.fst` | FST | Preferred indexed, multi-sample precomputed coverage store. The MegaBrowser reads transcript ranges and selected library columns from it. |
| `<experiment>/collection_tables_indexed/<transcript_id>.fst` or `<experiment>/collection_tables/<transcript_id>.fst` | FST | Per-transcript collection coverage table used as a legacy/fallback format when the indexed store is absent. |
| `<experiment>/meta_collection_tables/*.fst` | FST | Precomputed motif/metagene collection tables detected by the application controller. |
| `<reference>/predicted_translons/predicted_translons_with_sequence.fst` | FST | Predicted translated ORFs with sequence data for the predicted-translons page. |
| `<reference>/UMAP/UMAP_by_gene_counts.fst` | FST | UMAP coordinates and gene-count-derived fields, optionally combined into tissue/cell-line colour labels. |
| `<reference>/protein_structure_predictions/custom_regions.csv` | CSV | Optional custom protein-structure regions, converted to genomic ranges when present. |
| `<library>/bigwig/*_pshifted*` (or fallback BigWig path) | BigWig | Random-access collapsed read coverage used for browser tracks. |
| `<library>/cov_RLE/` and `<library>/cov_RLE_List/` | ORFik coverage files | Full-genome coverage, respectively collapsed or split by read length, used when BigWig is unsuitable. |
| `<experiment>/../trim/` | HTML and FastQC report assets | Default relative location for FastQ quality reports embedded by the FastQ page. |
| `inst/images/` | PNG, TIFF, SVG, ICO | Bundled logos, icons, and tutorial screenshots; presentation assets, not analysis datasets. |
| `inst/rmd/` and `vignettes/` | R Markdown, HTML, JS/CSS assets | Bundled tutorials and generated tutorial dependencies; documentation rather than runtime experiment data. |

## Related Input Formats

The tutorial documents an ORFik experiment as containing genome FASTA (and index), GTF/TxDb annotations, sequencing libraries, serialized count tables (`.qs`), and library-size vectors (`.rds`). These are expected under ORFik-managed experiment/reference locations, but this repository does not prescribe their exact relative filenames.

## Notes for Local Setup

Use `run_dev.R` only after replacing its workstation-specific paths. For a self-contained demonstration, the overview vignette creates an ORFik tutorial experiment from ORFik's `extdata/Homo_sapiens_sample` resources, then registers it beneath `ORFik::config()["exp"]`.
