unit BigLists;

interface

uses
  Classes, SysUtils;

type
  //TIntegerSize = (szByte, szInt16, szInt32, szInt64);

  // stores more than millions of numbers if required by using a file
  TBigIntegerList = class
    constructor Create(FilePath: string);
    destructor Free;
    procedure Add(number: int64);
    procedure AddStr(number: string);
    private
      FCount: int64;
      FFile: TFileStream;
      // function GetIntSize
      procedure SetCount(count: int64);
      function GetValue(const Index: int64): int64;
      procedure SetValue(const Index, value: int64);
      class procedure Error(Msg: PResStringRec; Data: NativeInt);

    public
      //property IntegerType: TIntegerSize; read GetIntSize write SetIntSize;
      property Count: Int64 read FCount write SetCount;
      property Items[const Index: int64]: int64 read GetValue write SetValue;
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

constructor TBigIntegerList.Create(FilePath: string);
begin
  MakeFile(FilePath);
  FFile := TFileStream.create(FilePath, fmOpenReadWrite);
  FFile.Position := 0;
  FCount := 0;
end;

destructor TBigIntegerList.Free;
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

procedure TBigIntegerList.SetCount(count: Int64);
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
    FFile.Position := SizeOf(index)*index;
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

class procedure TBigIntegerList.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]) at ReturnAddress;
end;



end.
