FROM r-base
RUN apt-get update && apt-get install -y libcurl4-openssl-dev
ADD ./context /context/
RUN /context/bin/context_bootstrap /context
