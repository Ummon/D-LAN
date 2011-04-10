TEMPLATE = subdirs
CONFIG += ordered
MAKEFILE = Makefile-Core
SUBDIRS = Common/Common.pro \
   Common/LogManager/LogManager.pro \
   Core/FileManager/FileManager.pro \
   Core/PeerManager/PeerManager.pro \
   Core/UploadManager/UploadManager.pro \
   Core/DownloadManager/DownloadManager.pro \
   Core/NetworkListener/NetworkListener.pro \
   Core/RemoteControlManager/RemoteControlManager.pro \
   Core/Core.pro
