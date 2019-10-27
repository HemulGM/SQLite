unit SQLiteHGM;

interface

uses
  SQLite3, SQLLang, SQLiteTable3, System.Generics.Collections,
  HGM.Controls.VirtualTable;

type
  TOnLogEvent = procedure(Text: string) of object;

  TDB = class
  private
    FDataBase: TSQLiteDatabase;
    FDataBaseName: string;
    FOnLog: TOnLogEvent;
    FSecretKey: string;
    procedure FDoLog(Text: string);
    procedure FOnQuery(Sender: TObject; SQL: string);
    procedure SetOnLog(const Value: TOnLogEvent);
    procedure SetSecretKey(const Value: string);
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    /// <summary>
    /// Логировать от имени БД
    /// </summary>
    procedure Log(Text: string);
    /// <summary>
    /// Передать пароль для дешифровки. Можно использовать только после создания БД
    /// </summary>
    /// <param name="Phrase: string">Пароль</param>
    /// <param name="DoXOR: Boolean = True (не обязательный)">Используя SecretKey сделать XOR пароля</param>
    procedure Password(Phrase: string; DoXOR: Boolean = True);
    /// <summary>
    /// Изменить пароль для дешифровки
    /// </summary>
    /// <param name="NewPhrase - System.string">Новый пароль</param>
    /// <param name="DoXOR - System.Boolean = True (не обязательный)">Используя SecretKey сделать XOR пароля</param>
    procedure ChangePassword(NewPhrase: string; DoXOR: Boolean = True);
    /// <summary>
    /// Инициализированный файл БД
    /// </summary>
    property DataBaseName: string read FDataBaseName;
    /// <summary>
    /// БД SQLite
    /// </summary>
    property DB: TSQLiteDatabase read FDataBase;
    /// <summary>
    /// Событие логирования БД (включая все SQL запросы)
    /// </summary>
    property OnLog: TOnLogEvent read FOnLog write SetOnLog;
    /// <summary>
    /// Секретная строка, которая участвует в XOR преобразовании пароля
    /// </summary>
    property SecretKey: string read FSecretKey write SetSecretKey;
  end;


function XORString(Text, Key: string; Offset: Integer = 0): string;

implementation

function XORString(Text, Key: string; Offset: Integer): string;
var
  i: Integer;
begin
  Result := '';
  if (Key = '') or (Text = '') then
    Exit;

  while Length(Key) < Length(Text) do
    Key := Key + Key;

  for i := 1 to Length(Text) do
    Result := Result + AnsiChar(Ord(Text[i]) xor not (Ord(Key[i mod Length(Key) + 1])));
end;

{ TDB }

constructor TDB.Create(FileName: string);
begin
  FSecretKey := 'D6AFC382FE1JGHDF557496789A56EDDE25AF244AE30D4EDA268CC12AC9E4B92F8';
  FDataBaseName := FileName;
  FDataBase := TSQLiteDatabase.Create(FDataBaseName);
  FDataBase.OnQuery := FOnQuery;
end;

destructor TDB.Destroy;
begin
  FDataBase.Free;
  inherited;
end;

procedure TDB.FDoLog(Text: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Text);
end;

procedure TDB.Log(Text: string);
begin
  FDoLog(Text);
end;

procedure TDB.Password(Phrase: string; DoXOR: Boolean);
begin
  if DoXOR then
    FDataBase.ExecSQL(SQL.PRAGMA('key', XORString(Phrase, FSecretKey)))
  else
    FDataBase.ExecSQL(SQL.PRAGMA('key', Phrase));
end;

procedure TDB.ChangePassword(NewPhrase: string; DoXOR: Boolean);
begin
  if DoXOR then
    FDataBase.ExecSQL(SQL.PRAGMA('rekey', XORString(NewPhrase, FSecretKey)))
  else
    FDataBase.ExecSQL(SQL.PRAGMA('rekey', NewPhrase));
end;

procedure TDB.FOnQuery(Sender: TObject; SQL: string);
begin
  FDoLog('SQL: ' + SQL);
end;

procedure TDB.SetOnLog(const Value: TOnLogEvent);
begin
  FOnLog := Value;
end;

procedure TDB.SetSecretKey(const Value: string);
begin
  FSecretKey := Value;
end;

end.

