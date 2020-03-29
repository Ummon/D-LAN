TEMPLATE = subdirs
CONFIG += ordered
MAKEFILE = Makefile-Client
SUBDIRS = Common \
   Common/LogManager \
   Common/RemoteCoreController \
   Client
