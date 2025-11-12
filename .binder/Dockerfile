FROM rocker/binder:4.4.0
LABEL maintainer='daniel_nothaft'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}



RUN wget https://github.com/danote/Oman-14CH4/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2024-06-14/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
