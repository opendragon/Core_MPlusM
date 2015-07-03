Core_MPlusM
===========

Movement and Meaning Middleware (also known as m+m) is a software system that acts as an intermediary between
subsystems that provide sensor data, such as accelerometers and motion capture cameras, and actuators such as projectors and sound systems. It provides mechanisms for reporting and interrogating the protocols used by the sensors and actuators, as well as a standard architecture for creating services.

An m+m installation consists of a set of programs that interconnect using the YARP networking protocols, along with libraries that can be linked to applications to provide access to the m+m features.

Note that m+m uses YARP (Yet Another Robotics Platform) [http://wiki.icub.org/yarpdoc] and ACE (the Adaptive Communication Environment) [http://www.cs.wustl.edu/~schmidt/ACE.html]. It has been tested with YARP 2.3.62 and ACE 6.2; the standard m+m installer includes copies of these, in a location that will not conflict with already installed copies of these systems.

m+m applications will not execute properly unless there is a 'reachable' YARP server running.
