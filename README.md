# FMXAudio
 FMXAudio

Инициализация
```
if not FMXPlayer.Init(Handle) then
begin
  ShowMessage('Аудио не инициализировано ' + FMXPlayer.GetLibPath);
end;
```
Запуск из потока
```
FMXPlayer.StreamURL := StreamUrl;
TThread.CreateAnonymousThread(
  procedure
  begin
    FMXPlayer.Play;
  end).Start;
```
Запуск из файла
```
FMXPlayer.FileName := FileName;
TThread.CreateAnonymousThread(
  procedure
  begin
    FMXPlayer.Play;
  end).Start;
```
