LIBS=-lortp -lpthread -lrt

all: compile

compile:
	gcc -o ../voice_client rtpsend.c $(LIBS)

clean:
	rm  ../voice_client