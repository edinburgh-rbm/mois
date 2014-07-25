---
layout: default
title: Quickstart
---

Quickstart for Debian GNU/Linux derived systems

~~~~
## install prerequisites
sudo apt-get install openjdk-7-jre git

## prepare directories, I like to make them like this
cd ~
mkdir -p bin lib src

# download sbt from http://www.scala-sbt.org/

## unpack / install sbt into lib
cd ~/lib; gzip -dc ~/Downloads/sbt*.tgz

## make a symbolic link into the bin directory in your
## home so the sbt program is on your path

cd ~/bin; ln -s ~/lib/sbt/bin/sbt .

## source your shell start-up files to add ~/bin to your
## path if it is not there already (or start a new terminal)
cd ~; . ./.profile

## get the source code from github
cd ~/src
git clone https://github.com/edinburgh-rbm/mois
git clone https://github.com/edinburgh-rbm/mois-examples

~~~~

Now proceed as in the [guide to running mois]

[guide to running mois](/mois-examples/running.html)
