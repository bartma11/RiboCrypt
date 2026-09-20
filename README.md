RiboCrypt: R Package for interactive visualization and browsing NGS data. 
==============================================================================
![](inst/images/tutorial_fig1.png)

#### Online app
See our webpage containing thousands of Ribosome profiling datasets to explore!

[RiboCrypt.org](https://ribocrypt.org/)

#### Installation
We advice you to use github devel version, as this package is under
heavy development, to not lose out on new features!

Package is available from bioconductor (3.16, R version >= 4.0.0)
```r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("RiboCrypt")
```

Development version on bioconductor (3.17, R version >= 4.0.0)
```r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("RiboCrypt", version = "devel")
```  

Package is also available here on github
```r
if (!requireNamespace("devtools", quietly=TRUE))
    install.packages("devtools")
devtools::install_github("m-swirski/RiboCrypt")
```  

#### Tutorials

For extensive tutorials, see bioconductor help tutorials (vignettes):

- [Introduction to RiboCrypt](https://bioconductor.org/packages/devel/bioc/vignettes/RiboCrypt/inst/doc/RiboCrypt_overview.html)

#### Docker development with automatic reload

The development container bind-mounts the repository and uses Shiny's
automatic reload support. Changes to R source files and supported web assets
are loaded without rebuilding or replacing the container. Connected Shiny
sessions are recreated during a reload, so transient session state is lost.

Build the development image from the repository root:

```bash
docker compose -f compose.dev.yml build
```

Create the host data directory, then prepare the bundled ORFik demo data and
its SQLite experiment index once. The directory is bind-mounted into the
container and reused on subsequent starts:

```bash
mkdir -p dev-data
docker compose -f compose.dev.yml run --rm ribocrypt-dev \
  Rscript scripts/prepare_dev_data.R
```

The generated index is directly accessible on the host at
`dev-data/RiboCrypt_demo/ribocrypt-demo.sqlite`. The rest of `dev-data/` is
also retained on the host so that all indexed demo libraries and references
remain available. Asset paths beginning with `/home/rstudio/Bio_data` inside
the index correspond to the repository's `dev-data/` directory on the host.

Start the application in the background and open
[http://localhost:3838](http://localhost:3838):

```bash
docker compose -f compose.dev.yml up -d
docker compose -f compose.dev.yml logs -f ribocrypt-dev
```

Edit files in the local checkout while the service is running. The development
runner monitors `R/` and `inst/`, reloads the RiboCrypt package namespace, and
refreshes the connected browser session when a supported file changes. The
container ID remains the same across reloads and can be checked with:

```bash
docker compose -f compose.dev.yml ps -q ribocrypt-dev
```

Use `stop` and `start` when you want to pause and resume the same development
service:

```bash
docker compose -f compose.dev.yml stop
docker compose -f compose.dev.yml start
```

Removing the container and network does not remove the host-side `dev-data/`
directory:

```bash
docker compose -f compose.dev.yml down
```
