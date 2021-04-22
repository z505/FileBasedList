unit BigLists;

interface

uses
  Classes, SysUtils;

type
  //TIntegerSize = (szByte, szInt16, szInt32, szInt64);

  TBigList = class
    constructor Create(FilePath: string);
    destructor Free;
    procedure SetCount(count: int64);
    class procedure Error(Msg: PResStringRec; Data: NativeInt);
  private
    FCount: int64;
    FFile: TFileStream;
  public
    property Count: Int64 read FCount write SetCount;
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

function MakeFile(Fpath: string): boolean;
var
  h: integer;
  i: integer;
begin
  h := FileCreate(Fpath);
  if h = INVALID_HANDLE_VALUE then
  begin
    result := false;
  end else
    FileClose(h);
end;

constructor TBigList.Create(FilePath: string);
begin
  MakeFile(FilePath);
  FFile := TFileStream.create(FilePath, fmOpenReadWrite);
  FFile.Position := 0;
  FCount := 0;
end;

destructor TBigList.Free;
begin
  FFile.free;
  FFile := nil;
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
begin
  FilePos := SizeOf(index)*index;
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
  buf: int64;
begin
  FFile.Position := SizeOf(index)*index;
  FFile.write(value, SizeOf(value));
end;

class procedure TBigList.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]) at ReturnAddress;
end;

procedure TBigIntegerPairList.Add(First, Second: int64);
begin
  FFile.Position := SizeOf(First)*Count + SizeOf(Second)*Count;
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
begin
  FilePos := (SizeOf(index)*index)*2;
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
  buf: int64;
begin
  FFile.Position := (SizeOf(index)*index)*2;
  FFile.write(value.First, SizeOf(value.First));
  FFile.write(value.Second, SizeOf(value.Second));
end;


end.
