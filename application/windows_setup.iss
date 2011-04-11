[code]
#define QtDir "E:/Qt/4.7.2"
#define MingwDir "E:/Qt/qtcreator-2.1.0/mingw"
#define ProtoBufDir "E:/protobuf"

#define AppName "D-LAN"
#define ExePath ".\Core\output\release\D-LAN.Core.exe"
#define Version GetStringFileInfo(ExePath, 'ProductVersion')
#define VersionTag GetStringFileInfo(ExePath, 'VersionTag')
#define BuildTime GetStringFileInfo(ExePath, 'BuildTime')

[Setup]
AppName={#AppName}
AppVersion={#Version} {#VersionTag} - {#BuildTime}
SetupIconFile=.\Common\ressources\icon.ico
DefaultDirName={pf}/{#AppName}
DefaultGroupName={#AppName}
UninstallDisplayIcon={app}/D-LAN.Core.exe
Compression=lzma2
SolidCompression=yes
OutputDir=Installations
OutputBaseFilename={#AppName}-{#Version}{#VersionTag}-{#BuildTime}-Setup

[Files]
Source: Core/output/release/D-LAN.Core.exe; DestDir: {app}; Flags: comparetimestamp; 
Source: GUI/output/release/D-LAN.GUI.exe; DestDir: {app}; Flags: comparetimestamp; 
Source: Tools/LogViewer/output/release/LogViewer.exe; DestDir: {app}; Flags: comparetimestamp; 
Source: Tools/PasswordHasher/output/release/PasswordHasher.exe; DestDir: {app}; Flags: comparetimestamp; 
Source: {#QtDir}/bin/QtCore4.dll; DestDir: {app}; Flags: comparetimestamp; 
Source: {#QtDir}/bin/QtGui4.dll; DestDir: {app}; Flags: comparetimestamp; 
Source: {#QtDir}/bin/QtNetwork4.dll; DestDir: {app}; Flags: comparetimestamp; 
Source: {#QtDir}/bin/QtNetwork4.dll; DestDir: {app}; Flags: comparetimestamp; 
Source: {#MingwDir}/bin/mingwm10.dll; DestDir: {app}; Flags: comparetimestamp; 
Source: {#MingwDir}/bin/libgcc_s_dw2-1.dll; DestDir: {app}; Flags: comparetimestamp; 
;Source: {#ProtoBufDir}/src/.libs/libprotobuf-7.dll; DestDir: {app}; Flags: comparetimestamp; 

[Icons]
Name: "{group}\D-LAN GUI"; Filename: {app}\D-LAN.GUI.exe; WorkingDir: "{app}"
Name: "{group}\Password Hasher"; Filename: {app}\PasswordHasher.exe; WorkingDir: "{app}"

[Tasks]
Name: Firewall; Description: "Add an exception to the Windows Firewall"; MinVersion: 0,5.01.2600sp2;

[Run]
Filename: {sys}\netsh.exe; Parameters: "firewall add allowedprogram ""{app}\D-LAN.Core.exe"" ""D-LAN.Core"" ENABLE ALL"; Flags: runhidden; MinVersion: 0,5.01.2600sp2; Tasks: Firewall; 
Filename: {app}\D-LAN.Core.exe; Parameters: --reset-settings -i; Description: Install the D-LAN service; Flags: RunHidden; 

[UninstallRun]
Filename: {app}\D-LAN.Core.exe; Parameters: -u;
Filename: {sys}\netsh.exe; Parameters: "firewall delete allowedprogram program=""{app}\D-LAN.Core.exe"""; Flags: runhidden; MinVersion: 0,5.01.2600sp2; Tasks: Firewall; 
