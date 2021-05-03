unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, BigLists,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    mStatus: TMemo;
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

{$R *.fmx}

procedure status(first: int64; s: string; second: int64); overload;
begin
  form1.mStatus.Lines.add(inttostr(first)+s+inttostr(second));
end;

procedure status(s: string; number: int64); overload;
begin
  form1.mStatus.Lines.add(s +inttostr(number));
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  Numbers: TBigIntegerPairList;
begin
  Numbers := TBigIntegerPairList.Create('tmpnumbers.bin');
  try
    Numbers.Add(998, 765453);
    Numbers.Add(4534875, 28934574);
    Numbers.Add(844353, 1234334);

    status(Numbers.Items[0].First,',',Numbers.Items[0].Second);

    status(Numbers.Items[1].First,',',Numbers.Items[1].Second);

    status(Numbers.Items[2].First,',',Numbers.Items[2].Second);

    status('Count: ', Numbers.Count);
  finally
    Numbers.Free; Numbers := nil;
  end;
end;

end.
