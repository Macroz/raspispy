#!/bin/sh
sudo hciconfig hci0 up
node target/server_dev/raspispy.js
