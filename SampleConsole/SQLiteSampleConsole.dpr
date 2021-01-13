program SQLiteSampleConsole;

uses
  HGM.SQLite in '..\HGM.SQLite.pas',
  HGM.SQLite.Wrapper in '..\HGM.SQLite.Wrapper.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  //Создаем подключание к БД (без файла, значит в озу)
  var Database := TSQLiteDatabase.Create;
  //Создаем таблицу
  Database.ExecSQL('create table users ( first_name text, last_name text, email text )');
  //Вставляем данные
  Database.ExecSQL('insert into users values (?, ?, ?)', ['МихаилИвнко', 'Иваночив', '1mich@mail.ru']);
  Database.ExecSQL('insert into users values ("Антон", "Перун", "antper@mail.ru")');
  Database.ExecSQL('insert into users values ("Дмитрий", "Казанцев", "dikaz@mail.com")');
  //Просим данные
  var Query := Database.Query('select * from users');
  //Берём их как массив
  for var Item in Query.ToArray do
  begin
    for var Field in Item do
      Write(Field, #9'  ');
    Writeln('');
  end;
  Query.Free;
  //Сохраняем бд на диск
  Database.Backup('data.db');
  //Прикрепляем сохраненный файл к текущему подключению
  Database.AttachDatabase('data.db', 'data');
  //Просим данные из бд на диске через нашу бд
  Query := Database.Query('select * from data.users');
  //Берём данные как массив
  for var Item in Query.ToArray do
  begin
    for var Field in Item do
      Write(Field, #9'  ');
    Writeln('');
  end;
  Query.Free;
  //Освобождаем
  Database.Free;
  Readln;
end.

