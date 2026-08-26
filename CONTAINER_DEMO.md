# Minimal Container Demo

This workflow runs the core RiboCrypt browser with ORFik's bundled simulated human dataset. It does not provide the collection FST, metadata, UMAP, or translon datasets required by MegaBrowser and Observatory.

## Prerequisite

Install Docker Engine or Docker Desktop and confirm that `docker version` succeeds. The image uses the official Bioconductor 3.23 container and requires network access while building.

## Build and Prepare Data

From the repository root, build the application image:

```sh
docker build --pull -t ribocrypt-demo:bioc-3.23 .
```

Create a persistent volume and populate it from ORFik's bundled sample:

```sh
docker volume create ribocrypt-demo-data
docker run --rm \
  -v ribocrypt-demo-data:/home/rstudio/Bio_data \
  ribocrypt-demo:bioc-3.23 \
  Rscript /workspace/RiboCrypt/scripts/prepare_demo_data.R
```

The preparation command is idempotent. It copies the `.ofst` libraries into the writable volume, creates `ORFik_tutorial_data`, generates BigWig tracks, and validates experiment discovery.

## Run the App

```sh
docker run --rm --name ribocrypt-demo \
  -p 3838:3838 \
  -v ribocrypt-demo-data:/home/rstudio/Bio_data \
  ribocrypt-demo:bioc-3.23
```

Open <http://localhost:3838>. To use another host port, map it to the same container port, for example `-p 8080:3838`.

Check container health and logs with:

```sh
docker inspect --format '{{.State.Health.Status}}' ribocrypt-demo
docker logs ribocrypt-demo
```

The app should select `ORFik_tutorial_data`, prefer the documented CAGE, PAS, RFP, and RNA libraries, and render the first available gene automatically. Messages stating that collections or metadata are unavailable are expected for this minimal dataset.

## Reset the Dataset

Stop the application before removing its named volume:

```sh
docker stop ribocrypt-demo
docker volume rm ribocrypt-demo-data
```

Removing the volume deletes the generated experiment and BigWig files. Run the preparation command again to recreate them.
