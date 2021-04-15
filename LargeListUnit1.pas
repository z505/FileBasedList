unit LargeListUnit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, FileBasedList2;

type
  TForm1 = class(TForm)
    mStatus: TMemo;
    bAddThousands: TButton;
    bAddMillion: TButton;
    procedure bAddThousandsClick(Sender: TObject);
    procedure bAddMillionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  LargeList: TBigIntegerList;


implementation

{$R *.dfm}

procedure status(s: string); overload;
begin
  form1.mStatus.Lines.add(s);
end;

procedure status(number: int64); overload;
begin
  form1.mStatus.Lines.add(inttostr(number));
end;

procedure Init100000(var list: TBigIntegerList);
var
  i: int64;
begin
  for i := 0 to 99999 do
  begin
    list.Add(i);
  end;
end;

procedure InitOneMillion(var list: TBigIntegerList);
var
  i: int64;
begin
  for i := 0 to 999999 do
  begin
    list.Add(i);
  end;
end;

procedure TForm1.bAddThousandsClick(Sender: TObject);
begin
  LargeList := TBigIntegerList.create('tmplist.bin');
  try
    status('Adding 100,000 (one hundred thousand) integers to the list');
    Init100000(LargeList);
    status('Accessing an integer, number 63481: ');
    status(LargeList.items[63481]);
  finally
    LargeList.free;
    LargeList := nil;
  end;

end;

procedure TForm1.bAddMillionClick(Sender: TObject);
begin

  LargeList := TBigIntegerList.create('tmplist.bin');
  try
    status('Adding 1,000,000 (million) integers to the list');
    InitOneMillion;
    status('Accessing an integer, number 784023: ');
    status(LargeList.items[784023]);
  finally
    LargeList.free;
    LargeList := nil;
  end;
end;


end.

