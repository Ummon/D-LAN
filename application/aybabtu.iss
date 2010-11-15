[code]
#define AppName "Aybabtu"
#define ExePath ".\Core\output\release\AybabtuCore.exe"
#define QtDir "C:/Qt/4.7.1"
#define MingwDir "C:/Qt/qtcreator-2.0.93/mingw"
#define Version GetStringFileInfo(ExePath, 'ProductVersion')
#define VersionTag GetStringFileInfo(ExePath, 'VersionTag')
#define BuildTime GetStringFileInfo(ExePath, 'BuildTime')

[Setup]
AppName={#AppName}
AppVersion={#Version} {#VersionTag}
DefaultDirName={pf}/{#AppName}
DefaultGroupName={#AppName}
UninstallDisplayIcon={app}/MyProg.exe
Compression=lzma2
SolidCompression=yes
OutputDir=Installations
OutputBaseFilename={#AppName} {#Version} {#VersionTag} - {#BuildTime} - Setup

[Files]
Source: Core/output/release/AybabtuCore.exe; DestDir: {app}
Source: GUI/output/release/AybabtuGUI.exe; DestDir: {app}
Source: Tools/LogViewer/output/release/LogViewer.exe; DestDir: {app}
Source: {#QtDir}/bin/QtCore4.dll; DestDir: {app}
Source: {#QtDir}/bin/QtGui4.dll; DestDir: {app}
Source: {#QtDir}/bin/QtNetwork4.dll; DestDir: {app}
Source: {#QtDir}/bin/QtNetwork4.dll; DestDir: {app}
Source: {#MingwDir}/bin/mingwm10.dll; DestDir: {app}
Source: {#MingwDir}/bin/libgcc_s_dw2-1.dll; DestDir: {app}

[Icons]
Name: "{group}\Aybabtu GUI"; Filename: {app}\AybabtuGUI.exe; WorkingDir: "{app}"
