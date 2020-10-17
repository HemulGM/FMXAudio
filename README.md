# FMXAudio

Handle initialization
```delphi
if not FMXPlayer.Init(Handle) then
begin
  ShowMessage('Error ' + FMXPlayer.LastErrorCode.ToString);
end;
```
## Sync
Play stream
```delphi
FMXPlayer.StreamURL := StreamUrl;
FMXPlayer.Play;
```
Play file
```delphi
FMXPlayer.FileName := FileName;
FMXPlayer.Play;
```
## Async
Play stream **async**
```delphi
FMXPlayer.StreamURL := StreamUrl;
FMXPlayer.PlayAsync;
```
Play file **async**
```delphi
FMXPlayer.FileName := FileName;
FMXPlayer.PlayAsync;
```
You can use properties: AutoInit, AutoPlay and Async

You can use the library to play sounds. And to prevent sounds from breaking off with new sounds, you must set the flag KeepPlayChannel = True;

## Windows
<pre>
1. Download bass24.zip from http://www.un4seen.com (direct link http://uk.un4seen.com/files/bass24.zip ).
2. Extract the archive somewhere, copy extracted bass.dll to your project output directory.
</pre>

## Android
Project -> Deployment -> Add Files
<pre>
1 : Library->Android->armeabi
	a : libbass.so
	b : libbass_aac.so	
	c : libbassflac.so
	Remote Path : library\lib\armeabi\
</pre>
<pre>
2 : Library->Android->x86
	a : libbass.so
	b : libbass_aac.so	
	c : libbassflac.so
	Remote Path : library\lib\x86\
</pre>
<pre>
3 : Library->Android->armeabi-v7a
	a : libbass.so
	b : libbass_aac.so	
	c : libbassflac.so
	Remote Path : library\lib\armeabi-v7a\
</pre>
Make sure library files remote path name, must be added in the Deployment window.

## Info
BASS is an audio library for use in software on several platforms. Its purpose is to provide developers with powerful and efficient sample, stream (MP3, MP2, MP1, OGG, WAV, AIFF, custom generated, and more via OS codecs and add-ons), MOD music (XM, IT, S3M, MOD, MTM, UMX), MO3 music (MP3/OGG compressed MODs), and recording functions. All in a compact DLL that won't bloat your distribution.

#Android
http://www.un4seen.com/forum/?topic=13225

#IOS
http://www.un4seen.com/forum/?topic=10910

#WinCE
http://www.un4seen.com/forum/?topic=9534

#ARMLINUX
http://www.un4seen.com/forum/?topic=13804
