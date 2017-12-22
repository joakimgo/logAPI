FROM nixos/nix:1.11 as build

RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update

RUN mkdir -p /opt/build
COPY . /opt/build
RUN cd /opt/build && nix-build --attr logAPI-small ./release.nix
RUN mkdir -p /opt/appclosure
RUN nix-store --export $(nix-store -qR $(readlink /opt/build/result)) > /opt/appclosure/app.closure
RUN cp -P /opt/build/result /opt/appclosure/

FROM nixos/nix:1.11
RUN mkdir -p /opt/app
WORKDIR /opt/app
COPY --from=build /opt/appclosure .
RUN nix-store --import < app.closure
RUN rm app.closure

#CMD bash
CMD /opt/app/result/bin/logAPI


