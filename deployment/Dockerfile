FROM r-base:latest 
RUN apt-get update && apt-get install -y -t unstable \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    procps \ 
    libssl-dev \
    libgdal-dev

#libssl-dev for compiling plotly library, procps for ps utility
# Download and install shiny server 
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    R -e "install.packages(c('shiny', 'rmarkdown','reshape2','devtools'), repos='https://cran.rstudio.com/')" && \
    R -e "install.packages(c('rgdal', 'leaflet','shinyjs','shinythemes'), repos='https://cran.rstudio.com/')" && \
    R -e "devtools::install_github('ropensci/plotly')" && \  
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/

EXPOSE 3838 

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server"]

