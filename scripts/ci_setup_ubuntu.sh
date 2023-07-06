#!/bin/bash

echo $ apt-get install -y libgmp-dev netbase
apt-get install -y libgmp-dev netbase || (apt-get update && apt-get install -y libgmp-dev netbase)
