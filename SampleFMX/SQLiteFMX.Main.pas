unit SQLiteFMX.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  HGM.SQLite, HGM.SQLang;

{$R *.fmx}

procedure TForm2.FormCreate(Sender: TObject);
var
  DB: TSQLiteDatabase;
  Query: TSQLiteTable;
begin
  TSQLiteDatabase.LoadSqliteLib('sqlite3.dll');

  DB := TSQLiteDatabase.Create('data.db');
  DB.Params.Add('@a', 'à');
  DB.Params.Add('@b', 'á');
  DB.Params.Add('@c', 'â');
  with SQL.Select('(' +
        SQL.Select('', '@a as col').GetSQL(True) + ' union all ' +
        SQL.Select('', '@b').GetSQL(True) + ' union all ' +
        SQL.Select('', '@c').GetSQL(True) + ')', [])
  do
  begin
    AddAsField('UPPER(col)', 'col');
    Query := DB.Query(GetSQL(True));
  end;
  //Query := DB.Query('select UPPER(col) from (select @a as col union all select @b union all select @c)');
  while not Query.EoF do
  begin
    Memo1.Lines.Add(Query.FieldAsString(0));
    Query.Next;
  end;
  Query.DisposeOf;

  DB.CreateFunction('UPPER_RU', @SQLiteUpper);
  with SQL.Select(
        SQL.Select('(select "à" as col union all select "á" union all select "â")', '*').AsSubquery,
        ['UPPER_RU(col)'])
  do
  begin
    WhereField('UPPER_RU(col)', '=', 'Á');
    Query := DB.Query(GetSQL(True));
  end;

  while not Query.EoF do
  begin
    Memo1.Lines.Add(Query.FieldAsString(0));
    Query.Next;
  end;
  Query.DisposeOf;
  //
  DB.DisposeOf;
end;

end.

