#!/bin/bash

# (1) Load required routines
. ../common/environ.sh
. ../common/usage.sh
. ../common/parse_inputs.sh
. ../common/build.sh
. ../common/run.sh
. ../common/check_inputdata.sh

parse_inputs $@

build
run

