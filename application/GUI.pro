TEMPLATE = subdirs
CONFIG += ordered
MAKEFILE = Makefile-Gui
SUBDIRS = Common/Common.pro \
   Common/LogManager/LogManager.pro \
   Common/RemoteCoreController/RemoteCoreController.pro \
   GUI/GUI.pro
