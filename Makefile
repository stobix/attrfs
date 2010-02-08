#all: filsystem_app filsystem filsystem_sup move_bin

all src/*erl: 
	cd src &&\
	erlc *.erl
	cd priv &&\
	gcc attr_server.c -Wall -lattr -o attr_server

filsystem_app src/filsystem_app.erl:
	cd src && \
	erlc filsystem_app.erl
filsystem src/filsystem.erl:
	cd src && \
	erlc filsystem.erl

filsystem_sup src/filsystem_sup.erl:
	cd src && \
	erlc filsystem_sup.erl


move_bin:
	mv src/*beam ebin -v
