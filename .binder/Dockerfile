FROM rocker/binder:4.3.2
LABEL maintainer='Andrew Duncan'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}

## Run an install.R script, if it exists.
RUN if [ -f install.R ]; then R --quiet -f install.R; fi