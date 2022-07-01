## What is it?

WebRTP is a web application for sending audio messages to clients of the local telephone network on Asterisk. <br>
On the Asterisk server, 6 users are allocated: 1 server user - <1000> (this application will connect to it) and 5 client users <1001-1005>. <br>
You can read more about how the Asterisk server works in the description of the server image that you need to install - https://hub.docker.com/repository/docker/dannmaj/pbx

## How does it work?

On startup, the web server starts running at a local address (localhost:8080 by default). <br>
The website is an HTML page with a two-field form: a drop-down list of prospective clients and a text to translate into audio. <br>
After submitting the form to the server, an attempt is made to establish a connection and make invite (SIP INVITE). <br>
When user answers the call, he receives a message synthesized by the Yandex SpeechKit service. <br>
<!> **The message has a character limit of 255 units for English and 20 units for Russian** <!>
## What do you need to start
* Docker
* Git
## Run
To start, you need to:
* Download this project
* Go to the key directory
* In files release.yaml and dev.yaml change argument IP on your local IP address (like 192.168.xxx.xxx)
* Run the following commands:

```
$ docker run -t -i -p "5060:5060/udp" -p "5060:5060/tcp" -p "10000-10010:10000-10010/udp" --name asterisk dannmaj/pbx:release -d
$ docker-compose -f release.yaml -p webrtp up -d
```
Also, if you want to try something change - you can start WebRTP in dev mode with command:
```
$ docker-compose -f dev.yaml -p webrtp-dev up -d
```
## Executables
If there is a problem with the architecture or if there is a need to recompile the voice_client executable, then run the following commands from the application directory (/app) in the docker container:
```
$ cd c_src && make && cd ..
```
The tests were carried out using Erlang 21.3 placed in a Docker container (GNU/Linux Alpine), Google Chrome browser and Asterisk 11.

## Important stuff
* When testing receiving audio messages as a SIP client Zoiper 5 due to fine tuning of audio codecs
* It is not recommended to use login 1000 for the client, because he is busy with the server
