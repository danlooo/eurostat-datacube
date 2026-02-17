FROM rocker/geospatial:4.5.2
WORKDIR /work
RUN  R -e "install.packages('renv')"
COPY renv.lock .
RUN R -e "renv::restore()"
COPY *.R .
CMD R -e "targets::tar_make()"