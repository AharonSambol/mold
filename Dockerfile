FROM rust:slim-bullseye AS osbase

RUN apt-get update --fix-missing && \
    apt-get install -y --no-install-recommends software-properties-common

# Work around tzdata, which python depends on, and which has an interactive installation
ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Asia/Jerusalem
RUN apt-get install -y --no-install-recommends tzdata

FROM osbase AS osbase-python
# RUN add-apt-repository ppa:deadsnakes/ppa
RUN apt-get update && apt-get install -y --no-install-recommends python3-pip
ENV PYTHON=/usr/bin/python3
# RUN $PYTHON --version

FROM osbase-python AS with-mold
WORKDIR /opt/mold
COPY src src/
COPY pretty-print-tree pretty-print-tree/
COPY Cargo.toml .

FROM with-mold AS mold-built
RUN cargo build

FROM mold-built

ENV CARGO_ROOT /usr/local/cargo
ENV PATH $CARGO_ROOT/bin:$PATH
ENV USER_SOURCE_CODE /opt/src_code

ENTRYPOINT ["cargo", "run"]
CMD ["$USER_SOURCE_CODE/in.mo"]
