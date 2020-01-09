# SQLite - БД провайдер и Конструктор запросов для Delphi

Модуль и интерфейс для работы с файловой базой данных sqlite


Модуль позволяет выполнить подключение к файлу БД через библиотеку SqliteEx.dll (Библиотека с возможностью шифрования БД)

Конструктор позволяет составить запрос посредством кода Delphi (SQL.Select, SQL.Delete, SQL.Update ...)
Конструктор избавляет от написания запросов и хранения его в коде.

Доступные конструкции: *select, update, delete, insert into, drop table, create table, where, order by, left | inner | outer | right join, pragma, incfield*, decfield**.

**SELECT**

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

<!--stackedit_data:
eyJoaXN0b3J5IjpbLTE3NzgxMzMwODJdfQ==
-->