unit BufferedStream;

interface

Uses
    Classes;

type
    TBufferedStream = class(TStream)
    private
        FBuf: array[0..1023] of Byte;
        FBufPos: Integer;
        FBufSize: Integer;
        FStream: TStream;
        FStreamPos: Int64;
        FStreamSize: Int64;
        FWriting: Boolean;
        procedure WriteBuffer;
    protected
        function GetSize: Int64; override;
        procedure SetSize(const NewSize: Int64); override;
    public
        constructor Create(Stream: TStream);
        destructor Destroy; override;
        function Read(var Buffer; Count: Longint): Longint; override;
        function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
        function Write(const Buffer; Count: Longint): Longint; override;
    end;

implementation

Uses
    Math;

constructor TBufferedStream.Create(Stream: TStream);
begin
    inherited Create;
    FStream := Stream;
    FStreamPos := Stream.Position;
    FStreamSize := Stream.Size;
end;

destructor TBufferedStream.Destroy;
begin
    if FWriting and (FBufSize > 0) then
        FStream.WriteBuffer(FBuf, FBufSize);
    inherited;
end;

function TBufferedStream.GetSize: Int64;
begin
    Result := FStreamSize;
end;

function TBufferedStream.Read(var Buffer; Count: Longint): Longint;
var
    i: Integer;
    P: PByte;
begin
    if FWriting then begin
        if FBufSize > 0 then
            FStream.WriteBuffer(FBuf, FBufSize);
        FWriting := False;
        Inc(FStreamPos, FBufPos);
        FStream.Position := FStreamPos;
        FBufPos := 0;
        FBufSize := 0;
    end;
    if Count > SizeOf(FBuf) then begin
        FStream.Seek(FStreamPos + FBufPos, soBeginning);
        Result := FStream.Read(Buffer, Count);
        FStreamPos := FStream.Position;
        FBufPos := 0;
        FBufSize := 0;
        Exit;
    end;
    Result := 0;
    P := @Buffer;
    while Count > 0 do begin
        if FBufPos = FBufSize then begin
            Inc(FStreamPos, FBufSize);
            FBufSize := FStream.Read(FBuf, SizeOf(FBuf));
            FBufPos := 0;
            if FBufSize = 0 then
                Break;
        end;
        i := Min(Count, FBufSize - FBufPos);
        Move(FBuf[FBufPos], P^, i);
        Inc(FBufPos, i);
        Inc(P, i);
        Dec(Count, i);
        Inc(Result, i);
    end;
end;

function TBufferedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
    Result := Offset;
    case Origin of
        soCurrent:
            Inc(Result, FStreamPos + FBufPos);
        soEnd:
            Result := FStreamSize + Result;
    end;
    if Result < 0 then
        Result := 0
    else if Result > FStreamSize then
        Result := FStreamSize;
    if (Result >= FStreamPos) and (Result <= FStreamPos + FBufSize) then
        FBufPos := Result - FStreamPos
    else begin
        if FWriting and (FBufSize > 0) then
            WriteBuffer;
        FStream.Position := Result;
        FStreamPos := Result;
        FBufPos := 0;
        FBufSize := 0;
    end;
end;

procedure TBufferedStream.SetSize(const NewSize: Int64);
begin
    if FStreamSize <> NewSize then begin
        if FWriting and (FBufSize > 0) then
            WriteBuffer;
        FStream.Size := NewSize;
        FStreamPos := NewSize;
        FStreamSize := NewSize;
    end;
end;

function TBufferedStream.Write(const Buffer; Count: Longint): Longint;
var
    i: Integer;
    P: PByte;
begin
    if Count > SizeOf(FBuf) then begin
        if FWriting
        AND (FBufSize > 0)
        then begin
            WriteBuffer;
            FWriting := False;
        end;
        Result := FStream.Write(Buffer, Count);
        FStreamPos := FStream.Position;
        FStreamSize := FStream.Size;
        FBufPos := 0;
        FBufSize := 0;
        Exit;
    end;
    if not FWriting then begin
        FWriting := True;
        Inc(FStreamPos, FBufPos);
        FStream.Position := FStreamPos;
        FBufPos := 0;
        FBufSize := 0;
    end;
    Result := 0;
    P := @Buffer;
    while Count > 0 do begin
        i := Min(Count, SizeOf(FBuf) - FBufPos);
        Move(P^, FBuf[FBufPos], i);
        Inc(FBufPos, i);
        if FBufPos > FBufSize then begin
            FBufSize := FBufPos;
            if FStreamPos + FBufSize > FStreamSize then
                FStreamSize := FStreamPos + FBufSize;
        end;
        Inc(P, i);
        Dec(Count, i);
        Inc(Result, i);
        if FBufPos = SizeOf(FBuf) then
            WriteBuffer;
    end;
end;

procedure TBufferedStream.WriteBuffer;
begin
    FStream.WriteBuffer(FBuf, FBufSize);
    Inc(FStreamPos, FBufSize);
    FBufPos := 0;
    FBufSize := 0;
end;

end.
