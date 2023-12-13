FROM rocker/binder:4.3.2
LABEL maintainer='Andrew Duncan'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}



RUN wget https://github.com/aj2duncan/leaflet-draw/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2023-12-13/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
