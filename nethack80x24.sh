#!/bin/sh

stty cols 80
stty rows 24
exec nethack -X $@

