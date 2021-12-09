#!/bin/bash

sed -i -e 's/$PORT/'"$PORT"'/g' /opt/marvel/src/backend/prod.nginxconf
PORT=3002 /opt/marvel/bin/marvel-api &
nginx -c /opt/marvel/src/backend/prod.nginxconf -g 'daemon off;'
