FROM bioconductor/bioconductor_docker:RELEASE_3_23

WORKDIR /workspace/RiboCrypt

COPY DESCRIPTION .

RUN R -q -e 'if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes"); remotes::install_deps(".", dependencies = NA, upgrade = "never")'

COPY . .

RUN R CMD INSTALL . \
    && mkdir -p /home/rstudio/Bio_data \
    && chown -R rstudio:rstudio /home/rstudio/Bio_data

USER rstudio

VOLUME ["/home/rstudio/Bio_data"]
EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=5 \
  CMD curl --fail --silent http://127.0.0.1:3838/ >/dev/null || exit 1

CMD ["Rscript", "/workspace/RiboCrypt/scripts/run_demo_app.R"]
