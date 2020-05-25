# FMXAudio

Initialization
```delphi
if not FMXPlayer.Init(Handle) then
begin
  ShowMessage('Аудио не инициализировано ' + FMXPlayer.GetLibPath);
end;
```
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
