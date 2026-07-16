FROM rocker/r-ver:4.4.2

ENV DEBIAN_FRONTEND=noninteractive \
    R_REMOTES_NO_ERRORS_FROM_WARNINGS=true \
    NOT_CRAN=true

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libharfbuzz-dev \
    libproj-dev \
    libssl-dev \
    libtiff-dev \
    libudunits2-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/biomastats
COPY . /opt/biomastats

RUN Rscript -e 'install.packages("remotes", repos = "https://cloud.r-project.org")' \
    && Rscript -e 'remotes::install_deps(dependencies = TRUE, upgrade = "never")'

RUN R CMD INSTALL .

COPY docker/smoke-test.R /usr/local/bin/biomastats-smoke-test.R

CMD ["bash", "-lc", "Rscript /usr/local/bin/biomastats-smoke-test.R && cd /opt && R CMD build --no-build-vignettes --no-manual biomastats && R CMD check --no-manual --no-vignettes biomastats_*.tar.gz"]
