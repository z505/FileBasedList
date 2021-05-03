unit BigLists;

interface

uses
  Classes, SysUtils;

const
  FLUSH_CAPACITY = 10000;

type
  //TIntegerSize = (szByte, szInt16, szInt32, szInt64);

  TBigList = class
    constructor Create(FilePath: string);
    destructor Destroy;
    procedure SetCount(count: int64);
    class procedure Error(Msg: PResStringRec; Data: NativeInt);
  private
    FCount: int64;
    FFile: TFileStream;
    FFlushed: boolean;
    FCapAmount: integer;
    FlushPosition: int64;
  public
    FileHandle: integer;
    property Count: Int64 read FCount write SetCount;
    procedure CloseFile;

  end;

  // stores one ore more up to millions of numbers in a list by using a file
  TBigIntegerList = class(TBigList)
    procedure Add(number: int64);
    procedure AddStr(number: string);
    private
      // function GetIntSize
      function GetValue(const Index: int64): int64;
      procedure SetValue(const Index, value: int64);
    public
      //property IntegerType: TIntegerSize; read GetIntSize write SetIntSize;
      property Items[const Index: int64]: int64 read GetValue write SetValue;
      // property ItemsStr[const Number: Int64]: string read GetStrValue write SetStrValue;

  end;

   TByteArrayBuf = array [1..FLUSH_CAPACITY] of byte;

   TBigByteList = class(TBigList)
     procedure Add(number: byte);
     procedure AddStr(number: string);
   private
     FCapBuf: TByteArrayBuf;
     // function GetIntSize
     function GetValue(const Index: int64): byte;
     procedure SetValue(const Index: int64; const value: byte);
     procedure Flush;
   public
     //property IntegerType: TIntegerSize; read GetIntSize write SetIntSize;
     property Items[const Index: int64]: byte read GetValue write SetValue;
     // property ItemsStr[const Number: Int64]: string read GetStrValue write SetStrValue;
     procedure Init10MillionZero;
     procedure BeginUpdate;
     procedure EndUpdate;
     procedure AddFast(number: byte);

   end;

  type
    TIntegerPair = record
      First: int64;
      Second: int64;
    end;

  // like the BigIntegerList but stores a pair of numbers (two) per each item
  TBigIntegerPairList = class(TBigList)
    procedure Add(First, Second: int64);
    procedure AddStr(First, Second: string);
  private
    // function GetIntSize
    function GetValues(const Index: int64): TIntegerPair;
    procedure SetValues(const Index: int64; value: TIntegerPair);

  public
    //property IntegerType: TIntegerSize; read GetIntSize write SetIntSize;
    property Items[const Index: int64]: TIntegerPair read GetValues write SetValues;
    // property ItemsStr[const Number: Int64]: string read GetStrValue write SetStrValue;

  end;


resourcestring
  reInvalidListItem = 'List item out of bounds, list file too small to obtain index %d';

implementation

type
  TArray10000Bytes = array [1..10000] of byte;


function MakeFile(Fpath: string; var FileHandle: integer): boolean;
begin
  FileHandle := FileCreate(Fpath);
  if FileHandle = INVALID_HANDLE_VALUE then
  begin
    result := false;
  end else
  begin
    FileClose(FileHandle);
  end;
end;

constructor TBigList.Create(FilePath: string);
begin
  MakeFile(FilePath, FileHandle);
  FFile := TFileStream.create(FilePath, fmOpenReadWrite);
  FFile.Position := 0;
  FCount := 0;
  FFlushed := false;
  FCapAmount := 0;
  FlushPosition := 0;
end;

procedure TBigList.CloseFile;
begin
  if FFile <> nil then
  begin
    FFile.free;
    FFile := nil;
  end;
end;

destructor TBigList.Destroy;
begin
  if FFile <> nil then begin
    FFile.free;
    FFile := nil;
  end;
end;

procedure TBigIntegerList.Add(number: int64);
begin
  FFile.Position := SizeOf(number)*Count;
  inc(FCount);
  FFile.Write(number, SizeOf(number));
end;

procedure TBigIntegerList.AddStr(number: string);
begin
  Add(StrToInt(number));
end;

procedure TBigList.SetCount(count: Int64);
begin
  FCount := count;
end;

function TBigIntegerList.GetValue(const Index: int64): int64;
var
  buf: int64;
  FilePos: int64;
  item: int64;
begin
  FilePos := SizeOf(item)*index;
  if FilePos > FFile.Size then begin
    Error(@reInvalidListItem, Index);
  end else begin
    FFile.Position := FilePos;
    FFile.Read(buf, SizeOf(buf));
    result := buf;
  end;
end;

procedure TBigIntegerList.SetValue(const index, value: int64);
var
  item: int64;
begin
  FFile.Position := SizeOf(item)*index;
  FFile.write(value, SizeOf(value));
end;

function TBigByteList.GetValue(const Index: int64): byte;
var
  buf: int64;
  FilePos: int64;
  b: byte;
begin
  FilePos := SizeOf(b)*index;
  if FilePos > FFile.Size then begin
    Error(@reInvalidListItem, Index);
  end else begin
    FFile.Position := FilePos;
    FFile.Read(buf, SizeOf(buf));
    result := buf;
  end;
end;

procedure TBigByteList.SetValue(const index: int64; const value: byte);
var
  b: byte;
begin
  FFile.Position := SizeOf(b)*index;
  FFile.write(value, SizeOf(value));
end;

class procedure TBigList.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]) at ReturnAddress;
end;

procedure TBigIntegerPairList.Add(First, Second: int64);
begin
  FFile.Position := (SizeOf(First)*Count + SizeOf(Second)*Count) -1;
  inc(FCount);
  FFile.Write(First, SizeOf(First));
  FFile.Write(Second, SizeOf(Second));
end;

procedure TBigIntegerPairList.AddStr(First, Second: string);
begin
  Add(StrToInt(First), StrToInt(Second));
end;

function TBigIntegerPairList.GetValues(const Index: int64): TIntegerPair;
var
  buf: array [1..2] of int64;
  FilePos: int64;
  item: int64;
begin
  FilePos := (SizeOf(item)*index)*2;
  if FilePos > FFile.Size then begin
    Error(@reInvalidListItem, Index);
  end else begin
    FFile.Position := FilePos;
    FFile.Read(buf, SizeOf(buf));
    result.First := buf[1];
    result.Second := buf[2];
  end;
end;

procedure TBigIntegerPairList.SetValues(const index: int64; value: TIntegerPair);
var
  item: int64;
begin
  FFile.Position := (SizeOf(item)*index)*2;
  FFile.write(value.First, SizeOf(value.First));
  FFile.write(value.Second, SizeOf(value.Second));
end;

procedure TBigByteList.Add(number: byte);
begin
  FFile.Position := SizeOf(number)*Count;
  inc(FCount);
  FFile.Write(number, SizeOf(number));
end;

procedure TBigByteList.BeginUpdate;
begin

end;

procedure TBigByteList.Flush;
var
  item: byte;
begin
  if not FFlushed then
  begin
    FFile.Position := FlushPosition;
    FFile.Write(FCapBuf, FCapAmount);
    FlushPosition := (FlushPosition + FCapAmount) * SizeOf(item);
    FFlushed := true;
    FCapAmount := 0;

  end;
end;

procedure TBigByteList.EndUpdate;
begin
  Flush;
end;

// use in combination with BeginUpdate and EndUpdate for speed.
// This is bufferred using the CapString algorithm (stores a certain
// capacity in memory buffer before writing)
procedure TBigByteList.AddFast(number: byte);
begin
  inc(FCapAmount);
  inc(FCount);
  FCapBuf[FCapAmount] := number;
  FFlushed := false;
  if FCapAmount = FLUSH_CAPACITY then
    Flush;
end;

procedure InitBuf10000(var buf: TArray10000Bytes);
var
  i: integer;
begin
  for i := 1 to length(buf) do
    buf[i] := 0;
end;

procedure TBigByteList.Init10MillionZero;
var
  buf: TArray10000Bytes;
  i: integer;
begin
  FFile.Position := 0;
  InitBuf10000(buf);
  for i := 1 to 1000 do
  begin
    inc(FCount, 10000);
    FFile.Write(buf, SizeOf(buf));
  end;
end;


procedure TBigByteList.AddStr(number: string);
begin
  Add(StrToInt(number));
end;


end.
