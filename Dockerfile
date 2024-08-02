FROM alpine:latest

RUN apk add --no-cache curl bash build-base sbcl git docker

WORKDIR /work
RUN curl -o quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --noinform --no-userinit --no-sysinit --non-interactive --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))"

WORKDIR /work
RUN git clone https://github.com/lem-project/micros.git
RUN git clone https://github.com/lem-project/lem-mailbox.git
RUN git clone https://github.com/lem-project/lem-base16-themes.git
RUN git clone https://github.com/lem-project/async-process.git
RUN git clone https://github.com/cxxxr/sblint.git
RUN git clone https://github.com/fukamachi/rove.git
RUN git clone https://github.com/lem-project/cl-sdl2.git
RUN git clone https://github.com/lem-project/cl-sdl2-ttf.git
RUN git clone https://github.com/lem-project/cl-sdl2-image.git
RUN git clone https://github.com/cxxxr/jsonrpc.git
RUN git clone https://github.com/lem-project/lem.git
RUN git clone https://github.com/ak-coram/cl-frugal-uuid
RUN git clone https://github.com/Shinmera/dissect.git

WORKDIR /work/lem-server
COPY . .

RUN mkdir -p ~/.config/common-lisp/
RUN echo "(:source-registry (:tree \"/work/\") :inherit-configuration)" > ~/.config/common-lisp/source-registry.conf

RUN sbcl --dynamic-space-size 1GB --disable-ldb --eval '(ql:quickload :lem-server)' --eval '(asdf:make :lem-server/executable)' --quit
RUN sbcl --dynamic-space-size 1GB --disable-ldb --eval '(ql:quickload :lem-replica)' --eval '(asdf:make :lem-replica/executable)' --quit

RUN chmod 755 /work/lem-server/lem-replica
RUN chmod 755 /work/lem-server/lem-server

RUN adduser -D user
RUN chown user:user -R /work/
USER user
WORKDIR /home/user

RUN sbcl --load /work/quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))"

ENTRYPOINT ["/work/lem-server/lem-server"]
