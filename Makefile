#all: filsystem_app filsystem filsystem_sup move_bin

all: 
	cd priv &&\
	gcc xattr_server.c -Wall -lattr -o xattr_server
	cd src &&\
	erlc *.erl

#priv priv/attr_server.c priv/attr_server:
priv: 
	cd priv &&\
	gcc xattr_server.c -Wall -lattr -o xattr_server


filsystem_app:
	cd src && \
	erlc filsystem_app.erl
filsystem:
	cd src && \
	erlc filsystem.erl

filsystem_sup:
	cd src && \
	erlc filsystem_sup.erl


move_bin:
	mv src/*beam ebin -v
