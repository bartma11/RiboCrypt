# Dataset Path Inventory

RiboCrypt ships code, documentation, and static application assets rather than genomic datasets. Runtime datasets are supplied through ORFik experiment directories or explicit metadata paths. The paths below are derived from source code and tutorials; paths marked external are not tracked in this repository.

## Runtime Dataset Tree

The following Markdown tree is a logical view of the files RiboCrypt can read. ORFik resolves the experiment results, reference, library, and quality-control roots from the selected experiment, so these roots do not have to be children of one physical directory. Entries marked `(optional)` enable individual application features; the application can run without them.

```text
<runtime data>
├── <experiment registry>/                 # ORFik::config()["exp"]
│   └── <experiment definition>             # selected by read.experiment()
├── <experiment results>/                 # ORFik::resFolder(df)
│   ├── collection_tables_indexed/
│   │   └── coverage_index.fst             # preferred multi-sample coverage
│   ├── collection_tables/                 # legacy coverage layout
│   │   └── <transcript_id>.fst
│   └── meta_collection_tables/            # optional motif/metagene tables
│       └── *.fst
├── <reference>/                          # ORFik::refFolder(df)
│   ├── <genome>.fa                         # genome FASTA named by the experiment
│   ├── <annotation>.gtf / <annotation>.rds # GTF or TxDb-style annotation
│   ├── predicted_translons/                # optional translated-ORF results
│   │   ├── predicted_translons_with_sequence.fst
│   │   ├── predicted_translons_with_sequence_ranges.rds
│   │   ├── predicted_translons_with_sequence_pep_linker.fst
│   │   └── TransCode/
│   │       └── predicted_translons_ranges.qs
│   ├── UMAP/
│   │   └── UMAP_by_gene_counts.fst       # optional organism UMAP
│   ├── protein_structure_predictions/
│   │   └── custom_regions.csv             # optional browser annotations
│   ├── phyloP100way/
│   │   └── *.phyloP100way.bw              # optional conservation track
│   └── mapability/
│       └── *28mers_mappability.bw           # optional mappability track
├── <library>/                            # ORFik::libFolder(df)
│   ├── bigwig/
│   │   └── *_pshifted*.bw                 # fast random-access coverage
│   ├── cov_RLE/                            # collapsed full-genome coverage
│   ├── cov_RLE_List/                       # coverage split by read length
│   ├── pshifted/
│   │   └── shifting_table.rds              # optional read-shift metadata
│   └── ../trim/
│       └── *.html                          # optional FastQ/FastQC report
├── <quality control>/                     # ORFik::QCfolder(df)
│   └── totalCounts_mrna.rds               # library sizes for normalization
└── <metadata path>/                      # explicit RiboCrypt_app() input
    └── metadata_rc.csv                    # curated metadata with a Run column
```

Angle-bracket names are placeholders, not literal directory names. The experiment definition records the concrete paths and sample attributes that ORFik uses to resolve them. A deployment only needs the branches used by its enabled pages and data types.

## Path Inventory

| Path | Filetype | Quick summary |
| --- | --- | --- |
| `~/livemount/Bio_data/ORFik_experiments` (external; `run_dev.R`) | ORFik experiment directory | Local development root containing discoverable experiment definitions and associated study/collection data. |
| `~/livemount/Bio_data/NGS_pipeline/metadata_rc.csv` (external; `run_dev.R`) | CSV | Curated sample metadata passed to the app; it must include a `Run` column. |
| `ORFik::config()["exp"]` (external, configurable) | ORFik experiment directory | Default experiment root used by `RiboCrypt_app()` and the overview vignette. Each experiment supplies annotations, library data, and derived results. |
| `<experiment>/collection_tables_indexed/coverage_index.fst` | FST | Preferred indexed, multi-sample precomputed coverage store. The MegaBrowser reads transcript ranges and selected library columns from it. |
| `<experiment>/collection_tables_indexed/<transcript_id>.fst` or `<experiment>/collection_tables/<transcript_id>.fst` | FST | Per-transcript collection coverage table used as a legacy/fallback format when the indexed store is absent. |
| `<experiment>/meta_collection_tables/*.fst` | FST | Precomputed motif/metagene collection tables detected by the application controller. |
| `<reference>/predicted_translons/predicted_translons_with_sequence.fst` | FST | Predicted translated ORFs with sequence data for the predicted-translons page. |
| `<reference>/predicted_translons/predicted_translons_with_sequence_ranges.rds` | RDS | Genomic ranges used to add predicted translons to browser annotations. |
| `<reference>/predicted_translons/predicted_translons_with_sequence_pep_linker.fst` | FST | Optional links between predicted peptide sequences and protein-structure records. |
| `<reference>/predicted_translons/TransCode/predicted_translons_ranges.qs` | QS | Optional TransCode translon ranges used as an additional browser annotation. |
| `<reference>/UMAP/UMAP_by_gene_counts.fst` | FST | UMAP coordinates and gene-count-derived fields, optionally combined into tissue/cell-line colour labels. |
| `<reference>/protein_structure_predictions/custom_regions.csv` | CSV | Optional custom protein-structure regions, converted to genomic ranges when present. |
| `<reference>/phyloP100way/*.phyloP100way.bw` | BigWig | Optional phyloP conservation track displayed beneath the sequence browser. |
| `<reference>/mapability/*28mers_mappability.bw` | BigWig | Optional 28-mer mappability track displayed beneath the sequence browser. |
| `<library>/bigwig/*_pshifted*` (or fallback BigWig path) | BigWig | Random-access collapsed read coverage used for browser tracks. |
| `<library>/cov_RLE/` and `<library>/cov_RLE_List/` | ORFik coverage files | Full-genome coverage, respectively collapsed or split by read length, used when BigWig is unsuitable. |
| `<library>/pshifted/shifting_table.rds` | RDS | Optional read-length shift table used by the heatmap controller. |
| `<quality control>/totalCounts_mrna.rds` | RDS | mRNA library-size totals required for MegaBrowser normalization. |
| `<library>/../trim/` | HTML and FastQC report assets | Default location, relative to the library folder, for FastQ quality reports embedded by the FastQ page. |
| `inst/images/` | PNG, TIFF, SVG, ICO | Bundled logos, icons, and tutorial screenshots; presentation assets, not analysis datasets. |
| `inst/rmd/` and `vignettes/` | R Markdown, HTML, JS/CSS assets | Bundled tutorials and generated tutorial dependencies; documentation rather than runtime experiment data. |

## Related Input Formats

The tutorial documents an ORFik experiment as containing genome FASTA (and index), GTF/TxDb annotations, sequencing libraries, serialized count tables (`.qs`), and library-size vectors (`.rds`). These are expected under ORFik-managed experiment/reference locations, but this repository does not prescribe their exact relative filenames.

## Notes for Local Setup

Use `run_dev.R` only after replacing its workstation-specific paths. For a self-contained demonstration, the overview vignette creates an ORFik tutorial experiment from ORFik's `extdata/Homo_sapiens_sample` resources, then registers it beneath `ORFik::config()["exp"]`.
