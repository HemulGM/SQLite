unit SQLiteSample.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  HGM.SQLite, HGM.SQLang;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  DB: TSQLiteDatabase;
  Query: TSQLiteTable;
begin
  TSQLiteDatabase.LoadSqliteLib('sqlite3ex.dll');

  DB := TSQLiteDatabase.Create(':memory:');
  with SQL.CreateTable('test1') do
  begin
    AddField('id', ftInteger, True, True);
    AddField('text', ftString);
    DB.ExecSQL(GetSQL(True));
  end;
  Memo1.Lines.Add(DB.TotalChanges.ToString);
  Memo1.Lines.Add(DB.RowsChanged.ToString);

  with SQL.InsertInto('test1') do
  begin
    AddValue('text', 'text1');
    DB.ExecSQL(GetSQL(True));
  end;

  with SQL.InsertInto('test1') do
  begin
    AddValue('text', 'text1');
    DB.ExecSQL(GetSQL(True));
  end;

  with SQL.InsertInto('test1') do
  begin
    AddValue('text', 'text1');
    DB.ExecSQL(GetSQL(True));
  end;

  Memo1.Lines.Add(DB.TotalChanges.ToString);
  Memo1.Lines.Add(DB.RowsChanged.ToString);

  Query := DB.Query('select UPPER(col) from (select ? as col union all select ? union all select ?)', ['à', 'á', 'â']);
  while not Query.EoF do
  begin
    Memo1.Lines.Add(Query.FieldAsString(0));
    Query.Next;
  end;
  Query.DisposeOf;

  DB.CreateFunction('UPPER_RU', @SQLiteUpper);
  Query := DB.Query('select UPPER_RU(col) as C from (select "à" as col union all select "á" union all select "â") WHERE C = "Á"');
  while not Query.EoF do
  begin
    Memo1.Lines.Add(Query.FieldAsString(0));
    Query.Next;
  end;
  Query.DisposeOf;

  Memo1.Lines.Add(DB.TotalChanges.ToString);
  Memo1.Lines.Add(DB.RowsChanged.ToString);
  //
  DB.DisposeOf;
end;

end.

