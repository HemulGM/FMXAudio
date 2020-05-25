# FMXAudio
 FMXAudio

Инициализация
```delphi
if not FMXPlayer.Init(Handle) then
begin
  ShowMessage('Аудио не инициализировано ' + FMXPlayer.GetLibPath);
end;
```
Запуск из потока
```delphi
FMXPlayer.StreamURL := StreamUrl;
TThread.CreateAnonymousThread(
  procedure
  begin
    FMXPlayer.Play;
  end).Start;
```
Запуск из файла
```delphi
FMXPlayer.FileName := FileName;
TThread.CreateAnonymousThread(
  procedure
  begin
    FMXPlayer.Play;
  end).Start;
```
