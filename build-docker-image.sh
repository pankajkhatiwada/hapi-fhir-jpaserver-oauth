#!/bin/sh

mvn package && \
  docker build -t ccavero/hapi-fhir-jpaserver-oauth:0.0.1 .

