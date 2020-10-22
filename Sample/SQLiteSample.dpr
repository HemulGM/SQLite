program SQLiteSample;

uses
  Vcl.Forms,
  SQLiteSample.Main in 'SQLiteSample.Main.pas' {Form1},
  HGM.SQLite in '..\HGM.SQLite.pas',
  HGM.SQLang in '..\HGM.SQLang.pas',
  HGM.SQLite.Wrapper in '..\HGM.SQLite.Wrapper.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
