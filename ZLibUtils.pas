unit ZLibUtils;

interface

uses
  SysUtils, Classes,
  AuxTypes, MemoryBuffer,
  ZLibCommon, ZLibStatic;

type
  TZCompressionLevel = (
    zclNoCompression   = Z_NO_COMPRESSION,
    zclBestSpeed       = Z_BEST_SPEED,
    zclBestCompression = Z_BEST_COMPRESSION,
    zclDefault         = Z_DEFAULT_COMPRESSION{%H-},
    zclLevel0          = 0,
    zclLevel1          = 1,
    zclLevel2          = 2,
    zclLevel3          = 3,
    zclLevel4          = 4,
    zclLevel5          = 5,
    zclLevel6          = 6,
    zclLevel7          = 7,
    zclLevel8          = 8,
    zclLevel9          = 9);

  TZMemLevel = (
    zmlDefault = DEF_MEM_LEVEL,
    zmlLevel1  = 1{%H-},
    zmlLevel2  = 2,
    zmlLevel3  = 3,
    zmlLevel4  = 4,
    zmlLevel5  = 5,
    zmlLevel6  = 6,
    zmlLevel7  = 7,
    zmlLevel8  = 8,
    zmlLevel9  = 9);

  TZStrategy = (
    zsFiltered = Z_FILTERED,
    zsHuffman  = Z_HUFFMAN_ONLY,
    zsRLE      = Z_RLE,
    zsFixed    = Z_FIXED,
    zsDefault  = Z_DEFAULT_STRATEGY{%H-});

  TZStreamType = (zstZLib,zstGZip,zstRaw,zstDefault = zstZLib{%H-});

  EZError = class(Exception)
  public
    constructor ZCreate(ErrCode: int; ZStream: z_stream);
  end;

  EZCompressionError   = class(EZError);
  EZDecompressionError = class(EZError);

const
  ZInvalidOp = 'Invalid operation';

  PROC_BUFFSIZE = 128 * 1024;      {128KiB}
  STRM_BUFFSIZE = 8 * 1024 * 1024;   {8MiB}

type
  TZProcessorOutEvent = procedure(Sender: TObject; Data: Pointer; Size: TMemSize) of object;
  TZProcessorOutCallback = procedure(Sender: TObject; Data: Pointer; Size: TMemSize);  

  TZProcessor = class(TObject)
  protected
    fZLibState:         z_stream;
    fOutBuffer:         TMemoryBuffer;
    fTotalCompressed:   UInt64;
    fTotalUncompressed: UInt64;
    fOnOutputEvent:     TZProcessorOutEvent;
    fOnOutputCallback:  TZProcessorOutCallback;
    fUserData:          PtrInt;
    Function GetCompressionRatio: Single;
    procedure DoOutput(OutSize: TMemSize); virtual;
  public
    procedure Init; virtual;
    procedure Update(Data: Pointer; Size: uInt); virtual; abstract;
    procedure Final; virtual;
    property TotalCompressed: UInt64 read fTotalCompressed;
    property TotalUncompressed: UInt64 read fTotalUncompressed;
    property CompressionRatio: Single read GetCompressionRatio;
    property OnOutputEvent: TZProcessorOutEvent read fOnOutputEvent write fOnOutputEvent;
    property OnOutputCallback: TZProcessorOutCallback read fOnOutputCallback write fOnOutputCallback;
    property UserData: PtrInt read fUserData write fUserData;
  end;

  TZCompressor = class(TZProcessor)
  protected
    fCompressionLevel:  TZCompressionLevel;
    fMemLevel:          TZMemLevel;
    fStrategy:          TZStrategy;
    fWindowBits:        int;
  public
    constructor Create(CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; WindowBits: int); overload;
    constructor Create(CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; StreamType: TZStreamType); overload;
    constructor Create(CompressionLevel: TZCompressionLevel; WindowBits: int); overload;
    constructor Create(CompressionLevel: TZCompressionLevel; StreamType: TZStreamType); overload;
    constructor Create(CompressionLevel: TZCompressionLevel = zclDefault); overload;
    procedure Init; override;
    procedure Update(Data: Pointer; Size: uInt); override;
    procedure Final; override;
    property CompressionLevel: TZCompressionLevel read fCompressionLevel;
    property MemLevel: TZMemLevel read fMemLevel;
    property Strategy: TZStrategy read fStrategy;
    property WindowBits: int read fWindowBits;
  end;

  TZDecompressor = class(TZProcessor)
  protected
    fWindowBits:  int;
  public
    constructor Create(WindowBits: int); overload;
    constructor Create(StreamType: TZStreamType = zstDefault); overload;
    procedure Init; override;
    procedure Update(Data: Pointer; Size: uInt); override;
    procedure Final; override;
    property WindowBits: int read fWindowBits write fWindowBits;
  end;
(*
  TZCustomStream = class(TStream)
  protected
    fZLibState:   z_stream;
    fBuffer:      Pointer;
    fTotalComp:   UInt64;
    fTotalUncomp: UInt64;
  public
    constructor Create;
    destructor Destroy; override;
    property TotalCompressed: UInt64 read fTotalComp;
    property TotalUncompressed: UInt64 read fTotalUncomp;  
  end;

  TZCompressionStream = class(TZCustomStream)
  protected
    fCompressionLevel:  TZCompressionLevel;
    fMemLevel:          TZMemLevel;
    fStrategy:          TZStrategy;
    fWindowBits:        int;
    fBufferUsed:        TMemSize;
    fOutBuffer:         Pointer;
    fDestination:       TStream;
    procedure FlushBuffer; virtual;
    procedure Accumulate(OutSize: uInt); virtual;
  public
    constructor Create(Dest: TStream; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; WindowBits: int); overload;
    constructor Create(Dest: TStream; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; StreamType: TZStreamType); overload;
    constructor Create(Dest: TStream; CompressionLevel: TZCompressionLevel; WindowBits: int); overload;
    constructor Create(Dest: TStream; CompressionLevel: TZCompressionLevel; StreamType: TZStreamType); overload;
    constructor Create(Dest: TStream; CompressionLevel: TZCompressionLevel = zclDefault); overload;
    destructor Destroy; override;
    Function Read(var {%H-}Buffer; {%H-}Count: LongInt): LongInt; override;
    Function Write(const Buffer; Count: LongInt): LongInt; override;
    Function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property CompressionLevel: TZCompressionLevel read fCompressionLevel;
    property MemLevel: TZMemLevel read fMemLevel;
    property Strategy: TZStrategy read fStrategy;
    property WindowBits: int read fWindowBits;
  end;
*)
(*
  TZDecompressionStream = class(TZCustomStream);
  protected
    fBuffer:        Pointer;
    fBufferUsed:    TMemSize;
    fSource:        TStream;
    fDecompressor:  TZDecompressor;
    Function GetWindowBits: int;
    procedure FlushBuffer; override;
    procedure OutHandler(Sender: TObject; OutBuffer: Pointer; OutSize: TMemSize); override;
  public
    constructor Create(Src: TStream; WindowBits: int); overload;
    constructor Create(Src: TStream; StreamType: TZStreamType = zstDefault); overload;
    destructor Destroy; override;
    Function Read(var Buffer; Count: LongInt): LongInt; override;
    Function Write(const Buffer; Count: LongInt): LongInt; override;
    Function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property WindowBits: int read GetWindowBits;
  end;
*)
implementation

Function CompressionErrCheck(ErrCode: int; State: z_stream; RaiseDictionaryError: Boolean = True): int;
begin
Result := ErrCode;
If (ErrCode < 0) or ((ErrCode = Z_NEED_DICT) and RaiseDictionaryError) then
  raise EZCompressionError.ZCreate(ErrCode,State)
end;

//------------------------------------------------------------------------------

Function DecompressionErrCheck(ErrCode: int; State: z_stream; RaiseDictionaryError: Boolean = True): int;
begin
Result := ErrCode;
If (ErrCode < 0) or ((ErrCode = Z_NEED_DICT) and RaiseDictionaryError) then
  raise EZDecompressionError.ZCreate(ErrCode,State);
end;

//------------------------------------------------------------------------------

Function GetStreamTypeWBits(StreamType: TZStreamType): int;
begin
case StreamType of
  zstZLib:  Result := WBITS_ZLIB;
  zstGZip:  Result := WBITS_GZIP;
  zstRaw:   Result := WBITS_RAW;
else
  raise EZError.CreateFmt('GetStreamTypeWBits: Unknown stream type (%d).',[Ord(StreamType)]);
end;
end;

//------------------------------------------------------------------------------
//==============================================================================
//------------------------------------------------------------------------------

constructor EZError.ZCreate(ErrCode: int; ZStream: z_stream);
begin
If Assigned(ZStream.msg) then
  CreateFmt('%s (%s)',[zError(ErrCode),ZStream.msg])
else
  CreateFmt('%s',[zError(ErrCode)])
end;

//------------------------------------------------------------------------------
//==============================================================================
//------------------------------------------------------------------------------

Function TZProcessor.GetCompressionRatio: Single;
begin
If fTotalCompressed <> 0 then
  Result := fTotalUncompressed / fTotalCompressed
else
  Result := 0.0;
end;

//==============================================================================

procedure TZProcessor.DoOutput(OutSize: TMemSize);
begin
If OutSize > 0 then
  begin
    If Assigned(fOnOutputEvent) then
      fOnOutputEvent(Self,fOutBuffer.Memory,OutSize);
    If Assigned(fOnOutputCallback) then
      fOnOutputCallback(Self,fOutBuffer.Memory,OutSize);
  end;
end;

//==============================================================================

procedure TZProcessor.Init;
begin
FillChar(fZLibState,SizeOf(fZLibState),0);
GetBuffer(fOutBuffer,PROC_BUFFSIZE);
fTotalCompressed := 0;
fTotalUncompressed := 0;
end;

//------------------------------------------------------------------------------

procedure TZProcessor.Final;
begin
FreeBuffer(fOutBuffer);
end;

//------------------------------------------------------------------------------
//==============================================================================
//------------------------------------------------------------------------------

constructor TZCompressor.Create(CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; WindowBits: int);
begin
inherited Create;
fCompressionLevel := CompressionLevel;
fMemLevel := MemLevel;
fStrategy := Strategy;
fWindowBits := WindowBits;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressor.Create(CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; StreamType: TZStreamType);
begin
Create(CompressionLevel,MemLevel,Strategy,GetStreamTypeWBits(StreamType));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressor.Create(CompressionLevel: TZCompressionLevel; WindowBits: int);
begin
Create(CompressionLevel,zmlDefault,zsDefault,WindowBits);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressor.Create(CompressionLevel: TZCompressionLevel; StreamType: TZStreamType);
begin
Create(CompressionLevel,zmlDefault,zsDefault,GetStreamTypeWBits(StreamType));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressor.Create(CompressionLevel: TZCompressionLevel = zclDefault);
begin
Create(CompressionLevel,zmlDefault,zsDefault,zstDefault);
end;

//------------------------------------------------------------------------------

procedure TZCompressor.Init;
begin
inherited;
CompressionErrCheck(deflateInit2(@fZLibState,Ord(fCompressionLevel),Z_DEFLATED,fWindowBits,Ord(fMemLevel),Ord(fStrategy)),fZLibState);
end;

//------------------------------------------------------------------------------

procedure TZCompressor.Update(Data: Pointer; Size: uInt);
var
  OutSize:  TMemSize;
begin
If Size > 0 then
  begin
    Inc(fTotalUncompressed,Size);
    fZLibState.next_in := Data;
    fZLibState.avail_in := Size;
    repeat
      fZLibState.next_out := fOutBuffer.Memory;
      fZLibState.avail_out := uInt(fOutBuffer.Size);
      CompressionErrCheck(deflate(@fZLibState,Z_NO_FLUSH),fZLibState);
      OutSize := TMemSize(fOutBuffer.Size - TMemSize(fZLibState.avail_out));
      Inc(fTotalCompressed,OutSize);
      DoOutput(OutSize);
    until fZLibState.avail_in <= 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TZCompressor.Final;
var
  ResultCode: int;
  OutSize:    TMemSize;
begin
try
  // flush what is left in zlib internal state
  fZLibState.next_in := nil;
  fZLibState.avail_in := 0;
  repeat
    fZLibState.next_out := fOutBuffer.Memory;
    fZLibState.avail_out := uInt(fOutBuffer.Size);
    ResultCode := CompressionErrCheck(deflate(@fZLibState,Z_FINISH),fZLibState);
    OutSize := TMemSize(fOutBuffer.Size - TMemSize(fZLibState.avail_out));
    Inc(fTotalCompressed,OutSize);
    DoOutput(OutSize);
  until ResultCode = Z_STREAM_END;
finally
  CompressionErrCheck(deflateEnd(@fZLibState),fZLibState);
end;
inherited;
end;

//------------------------------------------------------------------------------
//==============================================================================
//------------------------------------------------------------------------------

constructor TZDecompressor.Create(WindowBits: int);
begin
inherited Create;
fWindowBits := WindowBits;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZDecompressor.Create(StreamType: TZStreamType = zstDefault);
begin
Create(GetStreamTypeWBits(StreamType));
end;

//------------------------------------------------------------------------------

procedure TZDecompressor.Init;
begin
inherited;
DecompressionErrCheck(inflateInit2(@fZLibState,fWindowbits),fZLibState,True);
end;

//------------------------------------------------------------------------------

procedure TZDecompressor.Update(Data: Pointer; Size: uInt);
var
  ResultCode: int;
  OutSize:    TMemSize;
begin
Inc(fTotalCompressed,Size);
fZLibState.next_in := Data;
fZLibState.avail_in := Size;
repeat
  fZLibState.next_out := fOutBuffer.Memory;
  fZLibState.avail_out := uInt(fOutBuffer.Size);
  ResultCode := DecompressionErrCheck(inflate(@fZLibState,Z_NO_FLUSH),fZLibState,True);
  OutSize := TMemSize(fOutBuffer.Size - TMemSize(fZLibState.avail_out));
  Inc(fTotalUncompressed,OutSize);
  DoOutput(OutSize);
until (ResultCode = Z_STREAM_END) or (fZLibState.avail_in <= 0);
end;

//------------------------------------------------------------------------------

procedure TZDecompressor.Final;
begin
DecompressionErrCheck(inflateEnd(@fZLibState),fZLibState,True);
inherited;
end;

//------------------------------------------------------------------------------
//==============================================================================
//------------------------------------------------------------------------------
(*
constructor TZCustomStream.Create;
begin
inherited;
FillChar(fZLibState,SizeOf(fZLibState),0);
GetMem(fBuffer,STRM_BUFFSIZE);
fTotalComp := 0;
fTotalUncomp := 0;
end;

//------------------------------------------------------------------------------

destructor TZCustomStream.Destroy;
begin
FreeMem(fBuffer,STRM_BUFFSIZE);
inherited;
end;

//------------------------------------------------------------------------------
//==============================================================================
//------------------------------------------------------------------------------

procedure TZCompressionStream.FlushBuffer;
begin
fDestination.WriteBuffer(fBuffer^,fBufferUsed);
fBufferUsed := 0;
end;

//------------------------------------------------------------------------------

procedure TZCompressionStream.Accumulate(OutSize: uInt);
begin
If (fBufferUsed + OutSize) > STRM_BUFFSIZE then
  FlushBuffer;
Move(fOutBuffer^,Pointer(PtrUInt(fBuffer) + fBufferUsed)^,OutSize);
Inc(fBufferUsed,OutSize);
end;

//==============================================================================

constructor TZCompressionStream.Create(Dest: TStream; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; WindowBits: int);
begin
inherited Create;
fCompressionLevel := CompressionLevel;
fMemLevel := MemLevel;
fStrategy := Strategy;
fWindowBits := WindowBits;
fBufferUsed := 0;
GetMem(fOutBuffer,PROC_OUTBUFFSIZE);
fDestination := Dest;
CompErrCheck(deflateInit2(@fZLibState,Ord(fCompressionLevel),Z_DEFLATED,fWindowBits,Ord(fMemLevel),Ord(fStrategy)),fZLibState);
end;

//------------------------------------------------------------------------------

constructor TZCompressionStream.Create(Dest: TStream; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; StreamType: TZStreamType);
var
  wbits:  int;
begin
case StreamType of
  zstZLib:  wbits := WBITS_ZLIB;
  zstGZip:  wbits := WBITS_GZIP;
  zstRaw:   wbits := WBITS_RAW;
else
  raise EZError.CreateFmt('TZCompressor.Create: Unknown stream type (%d).',[Ord(StreamType)]);
end;
Create(Dest,CompressionLevel,MemLevel,Strategy,wbits);
end;

//------------------------------------------------------------------------------

constructor TZCompressionStream.Create(Dest: TStream; CompressionLevel: TZCompressionLevel; WindowBits: int);
begin
Create(Dest,CompressionLevel,zmlDefault,zsDefault,WindowBits);
end;
 
//------------------------------------------------------------------------------

constructor TZCompressionStream.Create(Dest: TStream; CompressionLevel: TZCompressionLevel; StreamType: TZStreamType);
var
  wbits:  int;
begin
case StreamType of
  zstZLib:  wbits := WBITS_ZLIB;
  zstGZip:  wbits := WBITS_GZIP;
  zstRaw:   wbits := WBITS_RAW;
else
  raise EZError.CreateFmt('TZCompressor.Create: Unknown stream type (%d).',[Ord(StreamType)]);
end;
Create(Dest,CompressionLevel,zmlDefault,zsDefault,wbits);
end;

//------------------------------------------------------------------------------

constructor TZCompressionStream.Create(Dest: TStream; CompressionLevel: TZCompressionLevel = zclDefault);
begin
Create(Dest,CompressionLevel,zmlDefault,zsDefault,zstDefault);
end;

//------------------------------------------------------------------------------

destructor TZCompressionStream.Destroy;
var
  ResultCode: int;
  OutSize:    TMemSize;
begin
try
  // flush what is left in zlib internal state
  fZLibState.next_in := nil;
  fZLibState.avail_in := 0;
  repeat
    fZLibState.next_out := fOutBuffer;
    fZLibState.avail_out := uInt(PROC_OUTBUFFSIZE);
    ResultCode := CompErrCheck(deflate(@fZLibState,Z_FINISH),fZLibState);
    OutSize := TMemSize(PROC_OUTBUFFSIZE - TMemSize(fZLibState.avail_out));
    Inc(fTotalComp,OutSize);
    Accumulate(OutSize);
  until ResultCode = Z_STREAM_END;
finally
  CompErrCheck(deflateEnd(@fZLibState),fZLibState);
end;
FlushBuffer;
FreeMem(fOutBuffer,PROC_OUTBUFFSIZE);
inherited;
end;

//------------------------------------------------------------------------------

Function TZCompressionStream.Read(var Buffer; Count: LongInt): LongInt;
begin
{$IFDEF FPC}
Result := 0;
{$ENDIf}
raise EZCompressionError.Create('TZCompressionStream.Read: ' + ZInvalidOp);
end;

//------------------------------------------------------------------------------

Function TZCompressionStream.Write(const Buffer; Count: LongInt): LongInt;
var
  ResultCode: int;
  OutSize:    TMemSize;
begin
//fCompressor.Update(@Buffer,uInt(Count));
//Result := Count;
end;

//------------------------------------------------------------------------------

Function TZCompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
If (Origin = soCurrent) and (Offset = 0) then
  Result := fTotalUncomp
else
  raise EZCompressionError.Create('TZCompressionStream.Seek: ' + ZInvalidOp);
end;
*)
end.
