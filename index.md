---
layout: default
title: Module Integration Simulator
---

Module Integration Simulator
============================

Pared down version of the integration core.

Reimplementation of Dominik Bucher's original at
https://github.com/edinburgh-rbm/moi-sim.git

  * [Examples and Narrative Documentation](https://edinburgh-rbm.github.io/mois-examples/)
  * [API Documentation](https://edinburgh-rbm.github.io/mois/api/current)

Building and Releasing
======================

This section is mostly of interest to those working on MOIS itself. It
may also be useful if you need to use a non-released version with
other software.

MOIS is an [http://www.scala-sbt.org](SBT) project and you need SBT to
build it. Once it is installed, MOIS can be fetched and the tests run
like so:

~~~~
git clone git@github.com:edinburgh-rbm/mois.git
cd mois
sbt test
~~~~

If the tests have run successfully, MOIS may be deployed to the local
IVY cache so that it can be found by other SBT projects that include
it as a dependency:

~~~~
sbt publish-local
~~~~

If you are a core developer and have the correct permissions and keys,
a new release of MOIS may be published to the global Maven
repository. This is so that users of the software can just list it in
their dependencies. This is done with:

~~~~
### increment version number in build.sbt
git tag -s -u 84225CBC mois-vAA.BB.CC
sbt publish-signed
~~~~
