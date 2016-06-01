#!/bin/bash

#Keep the container up-to-date
#docker pull jare/spacemacs:latest

cd /tmp

SPACE_HOME="$(docker inspect --format '{{.Config.Labels.HOME}}' \
jare/spacemacs:latest)"

echo '3...2...1...'

docker run -ti --rm -v $('pwd'):"${SPACE_HOME}/workspace" \
 -v /etc/localtime:/etc/localtime:ro                      \
 -v /home/jare/.ssh/id_rsa:${SPACE_HOME}/.ssh/id_rsa:ro   \
 -v /var/run/dbus:/var/run/dbus                           \
 -v /tmp/.X11-unix:/tmp/.X11-unix                         \
 -v /etc/machine-id:/etc/machine-id:ro                    \
 -e DISPLAY=$DISPLAY                                      \
 -e "GEMAIL=w3techplayground@gmail.com"                   \
 -e "GNAME=JAremko"                                       \
 -p 80:80 -p 8080:8080 -p 443:443 -p 3000:3000            \
 --name spacemacs2 jare/spacemacs:latest
