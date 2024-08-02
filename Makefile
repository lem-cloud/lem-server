all: lem-server lem-replica

lem-server:
	qlot install
	rm -f lem-server
	sbcl --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp --eval '(ql:quickload :lem-server)' --eval '(asdf:make :lem-server/executable)' --quit

lem-replica:
	qlot install
	rm -f lem-replica
	sbcl --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp --eval '(ql:quickload :lem-replica)' --eval '(asdf:make :lem-replica/executable)' --quit

docker:
	docker build -t lem-server .

clean:
	rm lem-replica lem-server
