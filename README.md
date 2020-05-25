# FMXAudio

Initialization
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
TThread.CreateAnonymousThread(
  procedure
  begin
    FMXPlayer.Play;
  end).Start;
```
Play file **async**
```delphi
FMXPlayer.FileName := FileName;
TThread.CreateAnonymousThread(
  procedure
  begin
    FMXPlayer.Play;
  end).Start;
```
You can use the library to play sounds. And to prevent sounds from breaking off with new sounds, you must set the flag KeepPlayChannel = True;
