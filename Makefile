#all: filsystem_app filsystem filsystem_sup move_bin

all: 
	cd src &&\
	erlc *.erl

move_bin:
	mv src/*beam ebin -v

clean:
	rm ebin/*beam -v

