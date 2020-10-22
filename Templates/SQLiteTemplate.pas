unit SQLiteTemplate;

interface

uses
  System.Generics.Collections, HGM.SQLite, SQLiteHGM, HGM.SQLang;

type
  TMyRecord = record
    ID: Integer;
    Name: string;
    Desc: string;
    DateCreate: TDateTime;
    //procedure SetFromTable(Table: TSQLiteTable);
  end;

  TMyTable = class(TList<TMyRecord>)
    const
      tnTableName = 'my_table';
      fnID = 'mtID';
      fnName = 'mtName';
      fnDesc = 'mtDesc';
      fnDateCreate = 'mtCreate';
  private
    FDB: TDB;
  public
    constructor Create(ADB: TDB);
    destructor Destroy; override;
    procedure Load;                              //Загрузка данных из базы
    procedure Update(var Item: TMyRecord); overload; //Обновление/Вставка записи в таблицу
    procedure Update(Index: Integer); overload;  //Обновление/Вставка записи в таблицу
    procedure Delete(Index: Integer);            //Удаление по порядковому номеру в списке из базы
    procedure DeleteByID(ID: Integer);           //Удаление по ИД записи из базы
    procedure Drop;                              //Очистка таблицы (не фактический Drop)
    procedure Save;                              //Сохранение таблицы (Обновление измененых записей, добавление новых)
  end;

implementation

{ TMyTable }

constructor TMyTable.Create(ADB: TDB);
begin
  inherited Create;
  FDB := ADB;
  if not FDB.DB.TableExists(tnTableName) then
  begin
    with SQL.CreateTable(tnTableName) do
    begin
      AddField(fnID, ftInteger, True, True);
      AddField(fnName, ftString);
      AddField(fnDesc, ftString);
      AddField(fnDateCreate, ftDateTime);
      FDB.DB.ExecSQL(GetSQL);
      EndCreate;
    end;
  end;
end;

destructor TMyTable.Destroy;
begin
  Clear;
  inherited;
end;

procedure TMyTable.Load;
var
  Table: TSQLiteTable;
  Item: TMyRecord;
begin
  Clear;
  with SQL.Select(tnTableName, [fnID, fnName, fnDesc, fnDateCreate]) do
  begin
    OrderBy(fnName, True);
    Table := FDB.DB.GetTable(GetSQL);
    EndCreate;
    Table.MoveFirst;
    while not Table.EOF do
    begin
      Item.ID := Table.FieldAsInteger(fnID);
      Item.Name := Table.FieldAsString(fnName);
      Item.Desc := Table.FieldAsString(fnDesc);
      Item.DateCreate := Table.FieldAsDateTime(fnDateCreate);
      Add(Item);
      Table.Next;
    end;
    Table.Free;
  end;
end;

procedure TMyTable.Update(Index: Integer);
var
  Item: TMyRecord;
begin
  Item := Items[Index];
  Update(Item);
  Items[Index] := Item;
end;

procedure TMyTable.Update(var Item: TMyRecord);
{var
  Mem: TMemoryStream;}
begin
  if Item.ID < 0 then
    with SQL.InsertInto(tnTableName) do
    begin
      AddValue(fnName, Item.Name);
      AddValue(fnDesc, Item.Desc);
      AddValue(fnDateCreate, Item.DateCreate);
      FDB.DB.ExecSQL(GetSQL);
      Item.ID := FDB.DB.GetLastInsertRowID;
      EndCreate;
    end
  else
    with SQL.Update(tnTableName) do
    begin
      AddValue(fnName, Item.Name);
      AddValue(fnDesc, Item.Desc);
      AddValue(fnDateCreate, Item.DateCreate);
      WhereFieldEqual(fnID, Item.ID);
      FDB.DB.ExecSQL(GetSQL);
      EndCreate;
    end;

  {if Assigned(Item.Image) then
    with SQL.UpdateBlob(tnTableName, fnImage) do
    begin
      WhereFieldEqual(fnID, Item.ID);
      Item.Image.SaveToStream(Mem);
      DataBase.DB.UpdateBlob(GetSQL, Mem);
      Mem.Free;
      EndCreate;
    end;  }
end;

procedure TMyTable.Delete(Index: Integer);
begin
  with SQL.Delete(tnTableName) do
  begin
    WhereFieldEqual(fnID, Items[Index].ID);
    FDB.DB.ExecSQL(GetSQL);
    EndCreate;
  end;
  inherited;
end;

procedure TMyTable.DeleteByID(ID: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].ID = ID then
    begin
      Delete(i);
      Exit;
    end;
  end;
end;

procedure TMyTable.Drop;
begin
  Clear;
  with SQL.Delete(tnTableName) do
  begin
    FDB.DB.ExecSQL(GetSQL);
    EndCreate;
  end;
end;

procedure TMyTable.Save;
var
  i: Integer;
  Item: TMyRecord;
begin
  for i := 0 to Count - 1 do
  begin
    Item := Items[i];
    Update(Item);
    Items[i] := Item;
  end;
end;

end.

