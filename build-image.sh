#!/bin/bash
cd docker/code && docker build -t hs_code_server .
docker pull evenchange4/docker-tfjs-converter