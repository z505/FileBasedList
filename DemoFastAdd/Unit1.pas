unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BigLists, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// adds 1 million items quickly using a capacity buffer
procedure TForm1.Button1Click(Sender: TObject);
var
  BigList: TBigByteList;
  i: Integer;
begin
  BigList := TBigByteList.create('tmplist.bin');
  try
    for i := 0 to 1000000 do
    begin
      if i = 250000 then
        BigList.AddFast(250)
      else
        BigList.AddFast(1);
    end;
    BigList.EndUpdate; // must call this when using AddFast
    memo1.lines.add('BigList item 250,000: ' + inttostr(BigList.items[250000]));
    memo1.lines.add('BigList item 0: ' + inttostr(BigList.items[0]));
    memo1.lines.add('BigList item 1: ' + inttostr(BigList.items[1]));

  finally
    BigList.Free;
    BigList := nil;
  end;
end;

end.
