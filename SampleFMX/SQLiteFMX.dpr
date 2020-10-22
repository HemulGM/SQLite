program SQLiteFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  SQLiteFMX.Main in 'SQLiteFMX.Main.pas' {Form2},
  HGM.SQLite in '..\HGM.SQLite.pas',
  HGM.SQLite.Wrapper in '..\HGM.SQLite.Wrapper.pas',
  HGM.SQLang in '..\HGM.SQLang.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
