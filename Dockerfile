ARG ERLANG_VERSION

FROM erlang:${ERLANG_VERSION} AS builder

ARG THRIFT_VERSION
ARG BUILDARCH
RUN wget -q -O- "https://github.com/valitydev/thrift/releases/download/${THRIFT_VERSION}/thrift-${THRIFT_VERSION}-linux-${BUILDARCH}.tar.gz" \
    | tar -xvz -C /usr/local/bin/

RUN mkdir /build
COPY . /build/
WORKDIR /build
RUN rebar3 compile
RUN rebar3 as prod release

FROM erlang:${ERLANG_VERSION}-slim
ARG SERVICE
ENV CHARSET=UTF-8
ENV LANG=C.UTF-8
COPY --from=builder /build/_build/prod/rel/${SERVICE} /opt/${SERVICE}
WORKDIR /opt/${SERVICE}
ENTRYPOINT []
CMD /opt/${SERVICE}/bin/${SERVICE} foreground
EXPOSE 8022
