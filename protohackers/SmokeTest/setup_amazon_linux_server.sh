#!/usr/bin/env bash

sudo yum install -y docker
sudo systemctl enable docker
sudo systemctl start docker
sudo usermod -a -G docker ec2-user
newgrp docker
docker run -d -p 5050:5050 -e SVC_PORT=5050 smoketest