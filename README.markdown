Tools for working with ROS in Haskell.

[ROS](http://www.ros.org) is a software framework developed by [Willow
Garage](http://www.willowgarage.com/) that aims to provide a
standard software architecture for robotic systems. The main idea of
the framework is to support the development and execution of loosely
coupled /Node/s connected by typed /Topic/s. Each Node represents a
locus of processing, ideally with a minimal interface specified in
terms of the types of Topics it takes as input and offers as output.

This package provides libraries for creating new ROS Nodes in Haskell,
along with the `roshask` executable for creating new ROS packages and
generating Haskell code from message definition files (see the ROS
documentation for information on message types).

See [the wiki](http://github.com/acowley/roshask/wiki) for more information on
getting started.

[![Build Status](https://travis-ci.org/acowley/roshask.png)](https://travis-ci.org/acowley/roshask)
