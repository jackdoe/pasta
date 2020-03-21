#!/bin/bash

root=$(dirname $0)
for i in pasta-server pasta-copy pasta-paste; do
    pushd $root/cmd/$i
    go build && sudo install $i /usr/bin/
    popd
done

cp $root/pasta-server.service ~/.config/systemd/user
systemctl --user daemon-reload
systemctl enable --user pasta-server
systemctl restart --user pasta-server
