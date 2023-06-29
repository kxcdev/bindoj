#!/bin/bash
echo $ apt-get install -y libgmp-dev
apt-get install -y libgmp-dev || (apt-get update && apt-get install -y libgmp-dev)
