#!/usr/bin/env bash

TEMP=`getopt hw:t:r: "$@"`

if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi

eval set -- "$TEMP"

RETRIES=100
ARGLIST=
while true; do
  case "$1" in
    -h ) ARGLIST="$ARGLIST -h"; shift ;;
    -w ) ARGLIST="$ARGLIST -w $2"; shift 2 ;;
    -t ) ARGLIST="$ARGLIST -t $2"; shift 2 ;;
    -r ) RETRIES=$2; shift 2 ;;
    -- ) shift; break ;;
    * ) break ;;
  esac
done

for (( c=1; c<=$RETRIES; c++ ))
do
  echo ">>>>>>> Try #$c <<<<<<<"
   `dirname $0`/multi_threaded_runner.rb $ARGLIST $*
   if [ $? != 0 ]; then
     echo "Error: Halting."
     exit $?
   fi
done

