program DemoLargeList;

uses
  Vcl.Forms,
  LargeListUnit1 in 'LargeListUnit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
