# SQLite - БД провайдер и Конструктор запросов для Delphi

Модуль и интерфейс для работы с файловой базой данных sqlite


Модуль позволяет выполнить подключение к файлу БД через библиотеку SqliteEx.dll (Библиотека с возможностью шифрования БД AES-256)

Конструктор позволяет составить запрос посредством кода Delphi (SQL.Select, SQL.Delete, SQL.Update ...)
Конструктор избавляет от написания запросов и хранения его в коде.

Доступные конструкции: *select, update, delete, insert into, drop table, create table, where, order by, left | inner | outer | right join, pragma, incfield\*, decfield\*.*

**SELECT**
```Delphi
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
```
**INSERT**
```Delphi
    with SQL.InsertInto(tnTableName) do
      begin
        AddValue(fnName, Item.Name);
        AddValue(fnDesc, Item.Desc);
        AddValue(fnDateCreate, Item.DateCreate);
        FDB.DB.ExecSQL(GetSQL);
        Item.ID := FDB.DB.GetLastInsertRowID;
        EndCreate;
      end;
```  
  **UPDATE**
```Delphi  
    with SQL.Update(tnTableName) do
      begin
        AddValue(fnName, Item.Name);
        AddValue(fnDesc, Item.Desc);
        AddValue(fnDateCreate, Item.DateCreate);
        WhereFieldEqual(fnID, Item.ID);
        FDB.DB.ExecSQL(GetSQL);
        EndCreate;
      end;
```
**UPDATE BLOB**
```Delphi
    with SQL.UpdateBlob(tnTableName, fnImage) do
      begin
        WhereFieldEqual(fnID, Item.ID);
        Item.Image.SaveToStream(Mem);
        FDB.DB.UpdateBlob(GetSQL, Mem);
        Mem.Free;
        EndCreate;
      end;
```
**DELETE**
```Delphi
    with SQL.Delete(tnTableName) do
      begin
        WhereFieldEqual(fnID, Items[Index].ID);
        FDB.DB.ExecSQL(GetSQL);
        EndCreate;
      end;
```
**CREATE TABLE**
```Delphi
    with SQL.CreateTable(tnTableName) do
      begin
        AddField(fnID, ftInteger, True, True);
        AddField(fnName, ftString);
        AddField(fnDesc, ftString);
        AddField(fnDateCreate, ftDateTime);
        FDB.DB.ExecSQL(GetSQL);
        EndCreate;
      end;
```
<!--stackedit_data:
eyJoaXN0b3J5IjpbLTEwODU1NDI0NTNdfQ==
-->
