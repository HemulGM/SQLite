program TMC;

uses
  Vcl.Forms,
  TMC.Main in 'TMC.Main.pas' {FormMain},
  SQLLang in '..\SQLLang.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Onyx Blue');
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
