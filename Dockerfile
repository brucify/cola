# Build stage 0
FROM amazonlinux:latest

RUN yum install -y git \
    gcc make automake autoconf libreadline-dev \
    libncurses-dev libssl-dev libyaml-dev \
    libxslt-dev libffi-dev libtool unixodbc-dev \
    unzip curl \
    openssl-devel ncurses-devel \
    libiodbc procps

RUN amazon-linux-extras install java-openjdk11

RUN git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.8.0 && \
    . $HOME/.asdf/asdf.sh && \
    asdf plugin add erlang && \
    asdf install erlang 23.3.1 && \
    asdf global erlang 23.3.1

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot
ENV APP=cola

# Copy our Erlang test application
COPY . $APP
RUN chmod +x /buildroot/$APP/rebar3
ENV PATH="/buildroot/$APP:$HOME/.asdf/installs/erlang/23.3.1/bin:${PATH}"

# And build the release
WORKDIR $APP
RUN rebar3 compile
RUN rebar3 as prod release

# Install the released application
COPY --from=0 /buildroot/$APP/_build/prod/rel/$APP /$APP

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD ["/$APP/bin/$APP", "foreground"]