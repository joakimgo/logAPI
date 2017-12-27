# LogAPI

## Overview
A simple web service to expose log files. 
The service exposes files that have changed the last 7 days in **/var/log** by default.

## API
The service listens on port **3000** by default and have these endpoints
* /list - list files that have changed the last 7 days as a HTML page
* /file?name=filename - returns web page with the content of filename
* /tail?lines=100&name=filename - returns web page with the last 100 lines from file filename
* /json/list - returns list of files in JSON format
* /json/file?name=filenam - returns file content as JSON format
* /json/tail?lines=100&name=filename - returns last 100 lines from filename as JSON format


## Building and Running
There are a couple of different way of building this project.

### Docker
If you have docker-compose installed you can build with,
```
docker-compose build
```
and run with
```
docker-compose up
```
be sure to be on a trusted network if you run this command since log files will be exposed to the network.

If you are on an untrusted network use the docker-compose-secure.yaml file. It defines a system with two docker instances, one of them being nginx configured to require clients to present a certificate in order to be allowed to connect. Client certificate required to connect and CA file and be found in the ./certs/ directory. I.e.
```
docker-compose -f docker-compose-secure.yaml up
```
The password for the client certificate pfx file is logapi.

### Nix
Nix can be installed with
```
curl https://nixos.org/nix/install | sh
```
To build application and create docker tar archive
```
nix-build --attr logAPI-docker-container-small release.nix
```
this tar archive can then be loaded into docker with
```
docker load < result
```
the image size is aboud 30 MB.
To build application without packaging it within docker
```
nix-build --attr logAPI release.nix
```
To enter a development environment with all the required dependencies use
```
nix-shell --attr logAPI.env release.nix
```
from there the project can be build with standard cabal commands.


