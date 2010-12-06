[code]
#define QtDir "C:/Qt/4.7.1"
#define MingwDir "C:/Qt/qtcreator-2.0.94/mingw"

#define AppName "Aybabtu"
#define ExePath ".\Core\output\release\AybabtuCore.exe"
#define Version GetStringFileInfo(ExePath, 'ProductVersion')
#define VersionTag GetStringFileInfo(ExePath, 'VersionTag')
#define BuildTime GetStringFileInfo(ExePath, 'BuildTime')

[Setup]
AppName={#AppName}
AppVersion={#Version} {#VersionTag} - {#BuildTime}
DefaultDirName={pf}/{#AppName}
DefaultGroupName={#AppName}
UninstallDisplayIcon={app}/MyProg.exe
Compression=lzma2
SolidCompression=yes
OutputDir=Installations
OutputBaseFilename={#AppName}-{#Version}{#VersionTag}-{#BuildTime}-Setup

[Files]
Source: Core/output/release/AybabtuCore.exe; DestDir: {app}; Flags: comparetimestamp; 
Source: GUI/output/release/AybabtuGUI.exe; DestDir: {app}; Flags: comparetimestamp; 
Source: Tools/LogViewer/output/release/LogViewer.exe; DestDir: {app}; Flags: comparetimestamp; 
Source: {#QtDir}/bin/QtCore4.dll; DestDir: {app}; Flags: comparetimestamp; 
Source: {#QtDir}/bin/QtGui4.dll; DestDir: {app}; Flags: comparetimestamp; 
Source: {#QtDir}/bin/QtNetwork4.dll; DestDir: {app}; Flags: comparetimestamp; 
Source: {#QtDir}/bin/QtNetwork4.dll; DestDir: {app}; Flags: comparetimestamp; 
Source: {#MingwDir}/bin/mingwm10.dll; DestDir: {app}; Flags: comparetimestamp; 
Source: {#MingwDir}/bin/libgcc_s_dw2-1.dll; DestDir: {app}; Flags: comparetimestamp; 

[Icons]
Name: "{group}\Aybabtu GUI"; Filename: {app}\AybabtuGUI.exe; WorkingDir: "{app}"

[Run]
Filename: {app}\AybabtuCore.exe; Parameters: -i; Description: Install the Aybabtu service; Flags: RunHidden; 

[UninstallRun]
Filename: {app}\AybabtuCore.exe; Parameters: -u;
