FROM rocker/shiny:4.5.2

RUN apt-get update && apt-get install -y  --no-install-recommends \
    libsodium-dev \
    libpq-dev \
    curl \
    # make \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r -s -n 8 --error \
    # optparse \
    devtools \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

#RUN r -e 'devtools::install_github("robonomist/roboplotr")'

# Newer packages
# RUN r -e 'install.packages(c("shiny", "bslib"), repos = "https://cloud.r-project.org")'

RUN sed -i -e 's/# fi_FI.UTF-8 UTF-8/fi_FI.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG=fi_FI.UTF-8
ENV LANGUAGE=fi_FI:en
ENV LC_ALL=fi_FI.UTF-8

ARG GITHUB_PAT

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN r -e 'devtools::install(upgrade="never")'

EXPOSE 8080
EXPOSE 8000

CMD ["./run_app.R"]
