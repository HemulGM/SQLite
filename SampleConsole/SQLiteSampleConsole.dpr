program SQLiteSampleConsole;

uses
  HGM.SQLite in '..\HGM.SQLite.pas',
  HGM.SQLite.Wrapper in '..\HGM.SQLite.Wrapper.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  //������� ����������� � �� (��� �����, ������ � ���)
  var Database := TSQLiteDatabase.Create;
  //������� �������
  Database.ExecSQL('create table users ( first_name text, last_name text, email text )');
  //��������� ������
  Database.ExecSQL('insert into users values (?, ?, ?)', ['�����������', '��������', '1mich@mail.ru']);
  Database.ExecSQL('insert into users values ("�����", "�����", "antper@mail.ru")');
  Database.ExecSQL('insert into users values ("�������", "��������", "dikaz@mail.com")');
  //������ ������
  var Query := Database.Query('select * from users');
  //���� �� ��� ������
  for var Item in Query.ToArray do
  begin
    for var Field in Item do
      Write(Field, #9'  ');
    Writeln('');
  end;
  Query.Free;
  //��������� �� �� ����
  Database.Backup('data.db');
  //����������� ����������� ���� � �������� �����������
  Database.AttachDatabase('data.db', 'data');
  //������ ������ �� �� �� ����� ����� ���� ��
  Query := Database.Query('select * from data.users');
  //���� ������ ��� ������
  for var Item in Query.ToArray do
  begin
    for var Field in Item do
      Write(Field, #9'  ');
    Writeln('');
  end;
  Query.Free;
  //�����������
  Database.Free;
  Readln;
end.

