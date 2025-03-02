# Start with a minimal Ubuntu base image
FROM ubuntu:22.04

# Set environment variables to avoid interactive prompts during installation
ENV DEBIAN_FRONTEND=noninteractive



RUN apt-get update && apt-get install -y bash

# Install system dependencies
RUN apt-get update && apt-get install -y \
    gdebi-core \
    wget \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libcairo2 \
    libxt-dev \
    libnss3 \
    cmake \
    g++ \
    wget \
    unzip \
    && rm -rf /var/lib/apt/lists/*




# Install required dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    software-properties-common \
    dirmngr \
    gnupg2 \
    curl \
    ca-certificates && \
    curl -fsSL https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc && \
    add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
    r-base r-base-dev r-recommended && \
    apt-get clean && rm -rf /var/lib/apt/lists/*



# Install NLopt from the official GitHub source
WORKDIR /tmp
RUN wget https://github.com/stevengj/nlopt/archive/v2.7.1.tar.gz && \
    tar -xzf v2.7.1.tar.gz && \
    cd nlopt-2.7.1 && \
    mkdir build && cd build && \
    cmake .. -DCMAKE_INSTALL_PREFIX=/usr && \
    make && make install && \
    cd ../.. && rm -rf nlopt-2.7.1 v2.7.1.tar.gz


# Set default CRAN repository
RUN echo 'options(repos = c(CRAN = "https://cloud.r-project.org/"))' >> /etc/R/Rprofile.site

RUN R -e "install.packages('nloptr', dependencies=TRUE, type='source')"


# Install Shiny R package

RUN R -e 'install.packages(c("shiny"),)'
RUN R -e 'install.packages(c("shinydashboard"),)'
RUN R -e 'install.packages(c("ggplot2"),)'
RUN R -e 'install.packages(c("bsicons"),)'
RUN R -e 'install.packages(c("shinyBS"),)'

RUN R -e 'install.packages(c("shinyWidgets"),)'
RUN R -e 'install.packages(c("colourpicker"),)'
RUN R -e 'install.packages(c("readxl"),)'
RUN R -e 'install.packages(c("readr"),)'
RUN R -e 'install.packages(c("DT"),)'
RUN R -e 'install.packages(c("lmerTest"),)'
RUN R -e 'install.packages(c("emmeans"),)'

RUN R -e 'install.packages(c("remotes"),)'
RUN R -e 'remotes::install_github("ashenoy-cmbi/grafify@*release")'
RUN R -e 'install.packages(c("grafify"),)'




# Download and install Shiny Server
#RUN https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb \
#    && dpkg -i shiny-server-1.5.22.1017-amd64.deb || apt-get install  -yf 


RUN wget -O shiny-server.deb https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb \
    && dpkg -i shiny-server.deb \
    && apt-get install -yf \
    && rm shiny-server.deb


# Create a new user 'shinyx' and set permissions
RUN useradd -m -s /bin/bash shinyx && \
    mkdir -p /home/shinyx/ShinyApps && \
    chown -R shinyx:shinyx /home/shinyx/ShinyApps

# Set working directory to 'geoff' home
WORKDIR /home/shinyx/ShinyApps

# Copy the Shiny app into the user directory
COPY ./grafify_app /home/shinyx/ShinyApps/app

# Change ownership so 'geoff' has access
RUN chown -R shiny:shiny /home/shinyx/ShinyApps

# Expose Shiny Server's default port
EXPOSE 3838

# Switch to user 'shiny' for security
USER shinyx


# Run the Shiny app directly with R instead of shiny-server
CMD ["R", "-e", "shiny::runApp('/home/shinyx/ShinyApps/app', host='0.0.0.0', port=3838)"]

# Set bash as the default shell
#CMD ["/bin/bash"]