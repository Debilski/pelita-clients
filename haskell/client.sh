#!/bin/bash
SCRIPTPATH=$(dirname "$0")
(cd "$SCRIPTPATH" && stack exec my-pelita-client $1)

