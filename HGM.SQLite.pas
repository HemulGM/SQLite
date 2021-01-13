unit HGM.SQLite;

{
  Simple classes for using SQLite's exec and get_table.

  TSQLiteDatabase wraps the calls to open and close an SQLite database.
  It also wraps SQLite_exec for queries that do not return a result set

  TSQLiteTable wraps execution of SQL query.
  It run query and read all returned rows to internal buffer.
  It allows accessing fields by name as well as index and can move through a
  result set forward and backwards, or randomly to any row.

  TSQLiteUniTable wraps execution of SQL query.
  It run query as TSQLiteTable, but reading just first row only!
  You can step to next row (until not EOF) by 'Next' method.
  You cannot step backwards! (So, it is called as UniDirectional result set.)
  It not using any internal buffering, this class is very close to Sqlite API.
  It allows accessing fields by name as well as index on actual row only.
  Very good and fast for sequentional scanning of large result sets with minimal
    memory footprint.

  Warning! Do not close TSQLiteDatabase before any TSQLiteUniTable,
    because query is closed on TSQLiteUniTable destructor and database connection
    is used during TSQLiteUniTable live!

  SQL parameter usage:
    You can add named parameter values by call set of AddParam* methods.
    Parameters will be used for first next SQL statement only.
    Parameter name must be prefixed by ':', '$' or '@' and same prefix must be
    used in SQL statement!
    Sample:
      table.AddParamText(':str', 'some value');
      s := table.GetTableString('SELECT value FROM sometable WHERE id=:str');

   Notes from Andrew Retmanski on prepared queries
   The changes are as follows:

   SQLiteTable3.pas
   - Added new boolean property Synchronised (this controls the SYNCHRONOUS pragma as I found that turning this OFF increased the write performance in my application)
   - Added new type TSQLiteQuery (this is just a simple record wrapper around the SQL string and a TSQLiteStmt pointer)
   - Added PrepareSQL method to prepare SQL query - returns TSQLiteQuery
   - Added ReleaseSQL method to release previously prepared query
   - Added overloaded BindSQL methods for Integer and String types - these set new values for the prepared query parameters
   - Added overloaded ExecSQL method to execute a prepared TSQLiteQuery

   Usage of the new methods should be self explanatory but the process is in essence:

   1. Call PrepareSQL to return TSQLiteQuery 2. Call BindSQL for each parameter in the prepared query 3. Call ExecSQL to run the prepared query 4. Repeat steps 2 & 3 as required 5. Call ReleaseSQL to free SQLite resources

   One other point - the Synchronised property throws an error if used inside a transaction.

   Acknowledments
   Adapted by Tim Anderson (tim@itwriting.com)
   Originally created by Pablo Pissanetzky (pablo@myhtpc.net)
   Modified and enhanced by Lukas Gebauer
   Modified and enhanced by Tobias Gunkel

   Внесены изменения 04.05.2017 HemulGM
   1. Обработаны новые исключения
   2. Введены новые типы исключений
   3. Исправлены ошибки форматировния и наведён косметический порядок

   Внесены изменения 26.10.2020 HemulGM
   1. Global refactoring
   2. Расширен класс базы данных
   3. Отложенная инициализация
   4. Инициализация нужной библиотеки
   5. Работа с PRAGMA key (sqlite3ex.dll)
   6. Fix unicode binds
   7. Измненён список параметров
   8. Создание функций
}

{$UNDEF UNICODE}

interface

uses
  {$IFDEF WIN32}
  Winapi.Windows,
  {$ENDIF}
  HGM.SQLite.Wrapper, System.Classes, System.SysUtils, System.Generics.Collections;

const
  ERROR_EOF = 'Table is at End of File';

type
  ESQLiteException = class(Exception);

  ESQLiteBlob = class(ESQLiteException);

  ESQLiteOpen = class(ESQLiteException);

  ESQLiteQueryError = class(ESQLiteException);

  ESQLiteUnknown = class(ESQLiteException);

  ESQLiteUnknownStringType = class(ESQLiteException);

  ESQLiteUnhandledPointer = class(ESQLiteException);

  ESQLiteUnhandledObjectType = class(ESQLiteException);

  ESQLiteUnhandledBinding = class(ESQLiteException);

  ESQLiteTransaction = class(ESQLiteException);

  ESQLiteInitializeBackup = class(ESQLiteException);

  ESQLiteIsBusy = class(ESQLiteException);

  ESQLiteFieldNotFound = class(ESQLiteException);

  ESQLiteTableEOF = class(ESQLiteException);

  ESQLiteTypeError = class(ESQLiteException);

  //

  TSQLiteDatabase = class;

  TSQLiteTable = class;

  TSQLiteUniTable = class;

  TSQLiteSynchronous = (ssOFF, ssNORMAL, ssFULL, ssEXTRA);

  TSQLiteSynchronousHelper = record helper for TSQLiteSynchronous
    function ToString: string; inline;
  end;

  THookQuery = procedure(Sender: TObject; SQL: string) of object;

  TSQLErrorHandle = procedure(Sender: TSQLiteDatabase; ErrorCode: Integer; Msg: string; var Handle: Boolean) of object;

  TSQLiteParam = class
  public
    Name: string;
    ValueType: Integer;
    ValueInteger: Int64;
    ValueFloat: Double;
    ValueData: string;
  end;

  TSQLiteParams = class(TObjectList<TSQLiteParam>)
    procedure Add(Name: string); overload;
    procedure Add(Name: string; Value: Double); overload;
    procedure Add(Name: string; Value: Int64); overload;
    procedure Add(Name: string; Value: string); overload;
  end;

  TSQLiteQuery = record
    SQL: string;
    Statement: TSQLiteStmt;
  end;

  TSQLiteDatabase = class
  private
    class var
      FIsInit: Boolean;
  private
    FDBInstance: TSQLiteDB;
    FInTrans: Boolean;
    FOnQuery: THookQuery;
    FParams: TSQLiteParams;
    FSync: TSQLiteSynchronous;
    FSQLErrorHandle: TSQLErrorHandle;
    FSecretKey: string;
    FTimeout: Integer;
    function GetRowsChanged: Integer;
    function GetTotalChanges: Int64;
    function GetLastInsertRowID: Int64;
    procedure SetSecretKey(const Value: string);
    procedure BindData(Stmt: TSQLiteStmt; const Bindings: array of const);
    procedure RaiseError(s: string; SQL: string);
    procedure SetParams(Stmt: TSQLiteStmt);
    procedure SetSynchronised(Value: TSQLiteSynchronous);
    procedure SetTimeout(Value: Integer);
  protected
    procedure DoQuery(Value: string);
  public
    class procedure LoadSqliteLib(SQLiteLib: string = ''); static;
    //
    function Backup(TargetDB: TSQLiteDatabase): Integer; overload;
    function Backup(TargetDB: TSQLiteDatabase; TargetName: string; SourceName: string): Integer; overload;
    function Backup(TargetDB: string): Integer; overload;
    function Backup(TargetDB: string; TargetName: string; SourceName: string): Integer; overload;
    //
    /// <summary>
    /// Строки базы данных, которые были изменены (или вставлены, или удалены) последним оператором SQL
    /// </summary>
    property RowsChanged: Integer read GetRowsChanged;
    /// <summary>
    /// Строки базы данных, которые были изменены (или вставлены, или удалены) последним оператором SQL с момента создания подключения
    /// </summary>
    property TotalChanges: Int64 read GetTotalChanges;
    /// <summary>
    /// ROWID последней вставленной строки
    /// </summary>
    property LastInsertRowID: Int64 read GetLastInsertRowID;
    //

    function GetTable(const SQL: string): TSQLiteTable; overload; deprecated 'Use Query or GetUniTable';
    function GetTable(const SQL: string; const Bindings: array of const): TSQLiteTable; overload; deprecated 'Use Query or GetUniTable';
    function Query(const SQL: string): TSQLiteTable; overload;
    function Query(const SQL: string; const Bindings: array of const): TSQLiteTable; overload;
    function GetTableValue(const SQL: string): Int64; overload;
    function GetTableValue(const SQL: string; const Bindings: array of const): Int64; overload;
    function GetUniTable(const SQL: string): TSQLiteUniTable; overload;
    function GetUniTable(const SQL: string; const Bindings: array of const): TSQLiteUniTable; overload;
    function GetTableString(const SQL: string): string; overload;
    function GetTableString(const SQL: string; const Bindings: array of const): string; overload;
    function GetSQLofTable(const TableName: string; DataBase: string = ''): string;
    function GetTableStrings(const SQL: string; const Value: TStrings): Boolean; overload;
    function GetTableStrings(const SQL: string; const Value: TStrings; FieldNum: Integer; const Bindings: array of const): Boolean; overload;
    //
    procedure ExecSQL(const SQL: string); overload;
    procedure ExecSQL(const SQL: string; const Bindings: array of const); overload;
    procedure ExecSQL(Query: TSQLiteQuery); overload;
    procedure UpdateBlob(const SQL: string; BlobData: TStream);
    //
    function PrepareSQL(const SQL: string): TSQLiteQuery;
    procedure ReleaseSQL(Query: TSQLiteQuery);
    //
    procedure AttachDatabase(const FileName, Alias: string);
    procedure DetachDatabase(const Alias: string);
    procedure CreateFunction(const FuncName: string; Addr: TxFunc; ArgCount: Integer = 1);
    function TableExists(TableName: string): Boolean;
    function Version: string;
    //
    procedure BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: Integer); overload;
    procedure BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: string); overload;
    //
    /// <summary>
    /// Добавляет сопоставление с именем SYSTEM для правильной сортировки данных по языку пользователя
    /// </summary>
    procedure AddSystemCollate;
    /// <summary>
    /// Добавляет сопоставление с именем SYSTEM для правильной сортировки данных по языку пользователя
    /// </summary>
    procedure AddCustomCollate(Name: string; Compare: TCollateCompare);
    //
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;
    property IsTransactionOpen: Boolean read FInTrans;
    //
    constructor Create(const FileName: string = ':memory:'); overload; virtual;
    destructor Destroy; override;
    //props
    property Instance: TSQLiteDB read FDBInstance;
    //
    property OnQuery: THookQuery read FOnQuery write FOnQuery;
    property ErrorHandler: TSQLErrorHandle read FSQLErrorHandle write FSQLErrorHandle;
    property Synchronous: TSQLiteSynchronous read FSync write SetSynchronised;
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
    procedure ChangePassword(Phrase: string; DoXOR: Boolean = True);
    /// <summary>
    /// Секретная строка, которая участвует в XOR преобразовании пароля
    /// </summary>
    property SecretKey: string read FSecretKey write SetSecretKey;
    //
    property Timeout: Integer read FTimeout write SetTimeout;
    /// <summary>
    ///
    /// </summary>
    property Params: TSQLiteParams read FParams;
  end;

  TSQLiteTable = class
  private
    FColCount: Cardinal;
    FCols: TStringList;
    FColTypes: TList<Integer>;
    FResults: TList;
    FRow: Cardinal;
    FRowCount: Cardinal;
    function GetBOF: Boolean;
    function GetColumns(i: integer): string;
    function GetCount: Integer;
    function GetCountResult: Integer;
    function GetEOF: Boolean;
    function GetFieldByName(FieldName: string): string;
    function GetFieldIndex(FieldName: string): Integer;
    function GetFields(i: Cardinal): string;
  public
    function ToArray: TArray<TArray<string>>;
    function FieldsToArray: TArray<string>;
    function FieldAsBlob(i: Cardinal): TMemoryStream; overload;
    function FieldAsBlobText(i: Cardinal): string; overload;
    function FieldAsDouble(i: Cardinal): Double; overload;
    function FieldAsDateTime(i: Cardinal): TDateTime; overload;
    function FieldAsInteger(i: Cardinal): Int64; overload;
    function FieldAsBoolean(i: Cardinal): Boolean; overload;
    function FieldAsString(i: Cardinal): string; overload;
    function FieldIsNull(i: Cardinal): Boolean; overload;
    function FieldAsBlob(FieldName: string): TMemoryStream; overload;
    function FieldAsBlobText(FieldName: string): string; overload;
    function FieldAsDouble(FieldName: string): Double; overload;
    function FieldAsDateTime(FieldName: string): TDateTime; overload;
    function FieldAsInteger(FieldName: string): Int64; overload;
    function FieldAsBoolean(FieldName: string): Boolean; overload;
    function FieldAsString(FieldName: string): string; overload;
    function FieldIsNull(FieldName: string): Boolean; overload;
    function MoveFirst: Boolean;
    function MoveLast: Boolean;
    function MoveTo(Position: Cardinal): Boolean;
    function Next: Boolean;
    function Previous: Boolean;
    property BoF: Boolean read GetBOF;
    property ColCount: Cardinal read FColCount;
    property Columns[i: Integer]: string read GetColumns;
    property Count: Integer read GetCount;
    // The property CountResult is used when you execute count(*) queries.
    // It returns 0 if the result set is empty or the value of the
    // first field as an integer.
    property CountResult: Integer read GetCountResult;
    property EoF: Boolean read GetEOF;
    property FieldByName[FieldName: string]: string read GetFieldByName;
    property FieldIndex[FieldName: string]: integer read GetFieldIndex;
    property Fields[i: Cardinal]: string read GetFields;
    property Row: Cardinal read FRow;
    property RowCount: Cardinal read FRowCount;
    constructor Create(DB: TSQLiteDatabase; const SQL: string); overload;
    constructor Create(DB: TSQLiteDatabase; const SQL: string; const Bindings: array of const); overload;
    destructor Destroy; override;
  end;

  TSQLiteUniTable = class
  private
    FColCount: Cardinal;
    FCols: TStringList;
    FDB: TSQLiteDatabase;
    FEoF: Boolean;
    FRow: Cardinal;
    FSQL: string;
    FStmt: TSQLiteStmt;
    function GetColumns(i: Integer): string;
    function GetFieldByName(FieldName: string): string;
    function GetFieldIndex(FieldName: string): Integer;
    function GetFields(i: Cardinal): string;
  public
    function FieldAsBlob(i: Cardinal): TMemoryStream; overload;
    function FieldAsBlobPtr(i: Cardinal; out iNumBytes: integer): Pointer; overload;
    function FieldAsBlobText(i: Cardinal): string; overload;
    function FieldAsDouble(i: Cardinal): Double; overload;
    function FieldAsInteger(i: Cardinal): Int64; overload;
    function FieldAsString(i: Cardinal): string; overload;
    function FieldIsNull(i: Cardinal): Boolean; overload;
    function FieldAsDateTime(i: Cardinal): TDateTime; overload;
    function FieldAsBoolean(i: Cardinal): Boolean; overload;
    function FieldAsBlob(FieldName: string): TMemoryStream; overload;
    function FieldAsBlobText(FieldName: string): string; overload;
    function FieldAsDouble(FieldName: string): Double; overload;
    function FieldAsInteger(FieldName: string): Int64; overload;
    function FieldAsString(FieldName: string): string; overload;
    function FieldIsNull(FieldName: string): Boolean; overload;
    function FieldAsDateTime(FieldName: string): TDateTime; overload;
    function FieldAsBoolean(FieldName: string): Boolean; overload;
    function Next: Boolean;
    property ColCount: Cardinal read FColCount;
    property Columns[i: Integer]: string read GetColumns;
    property EoF: Boolean read FEoF;
    property FieldByName[FieldName: string]: string read GetFieldByName;
    property FieldIndex[FieldName: string]: Integer read GetFieldIndex;
    property Fields[i: Cardinal]: string read GetFields;
    property Row: Cardinal read FRow;
    constructor Create(DB: TSQLiteDatabase; const SQL: string); overload;
    constructor Create(DB: TSQLiteDatabase; const SQL: string; const Bindings: array of const); overload;
    destructor Destroy; override;
  end;

function SQLiteErrorStr(SQLiteErrorCode: Integer): string;

function SQLiteFieldType(SQLiteFieldTypeCode: Integer): string;

procedure DisposePointer(ptr: pointer); cdecl;

procedure SQLiteUpper(Context: Pointer; Arg: Integer; Args: PPointerArray); cdecl;

procedure SQLiteLower(Context: Pointer; Arg: Integer; Args: PPointerArray); cdecl;

procedure SQLiteContains(Context: Pointer; Arg: Integer; Args: PPointerArray); cdecl;

function SystemCollate(Userdta: pointer; Buf1Len: integer; Buf1: pointer; Buf2Len: integer; Buf2: pointer): integer; cdecl;

implementation

{$WARNINGS OFF}
function XORString(Text, Key: string; Offset: Integer = 0): string;
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
{$WARNINGS ON}

function SQLiteErrorStr(SQLiteErrorCode: Integer): string;
begin
  case SQLiteErrorCode of
    SQLITE_OK:
      Result := 'Успешный результат';
    SQLITE_ERROR:
      Result := 'Ошибка SQL или отсутствует база данных';
    SQLITE_INTERNAL:
      Result := 'Внутренняя логическая ошибка SQLite';
    SQLITE_PERM:
      Result := 'Отказано в доступе';
    SQLITE_ABORT:
      Result := 'Процедура обратного вызова запросила отмену';
    SQLITE_BUSY:
      Result := 'Файл базы данных заблокирован';
    SQLITE_LOCKED:
      Result := 'Таблица в базе данных заблокирована';
    SQLITE_NOMEM:
      Result := 'Ошибка malloc()';
    SQLITE_READONLY:
      Result := 'Попытка записи в базу данных только для чтения';
    SQLITE_INTERRUPT:
      Result := 'Операция завершена sqlite3_interrupt()';
    SQLITE_IOERR:
      Result := 'Произошла ошибка дискового ввода-вывода';
    SQLITE_CORRUPT:
      Result := 'Образ диска базы данных имеет неверный формат';
    SQLITE_NOTFOUND:
      Result := '(Только для внутреннего использования) Таблица или запись не найдены';
    SQLITE_FULL:
      Result := 'Ошибка при вставке, поскольку база данных заполнена';
    SQLITE_CANTOPEN:
      Result := 'Не удалось открыть файл базы данных';
    SQLITE_PROTOCOL:
      Result := 'Ошибка протокола базы данных';
    SQLITE_EMPTY:
      Result := 'База данных пуста';
    SQLITE_SCHEMA:
      Result := 'Схема базы данных изменена';
    SQLITE_TOOBIG:
      Result := 'Слишком много данных для одной строки таблицы';
    SQLITE_CONSTRAINT:
      Result := 'Прервано из-за нарушения ограничения';
    SQLITE_MISMATCH:
      Result := 'Несовпадение типа данных';
    SQLITE_MISUSE:
      Result := 'Библиотека используется неправильно';
    SQLITE_NOLFS:
      Result := 'Использует функции ОС, не поддерживаемые на хосте';
    SQLITE_AUTH:
      Result := 'Авторизация запрещена';
    SQLITE_FORMAT:
      Result := 'Ошибка форматирования вспомогательной базы данных';
    SQLITE_RANGE:
      Result := 'Второй параметр для sqlite3_bind вне диапазона';
    SQLITE_NOTADB:
      Result := 'Открыт файл, который не является файлом базы данных';
    SQLITE_ROW:
      Result := 'Sqlite3_step() имеет еще одну строку';
    SQLITE_DONE:
      Result := 'Выполнение sqlite3_step() завершено';
  else
    Result := 'Неизвестный код ошибки SQLite "' + IntToStr(SQLiteErrorCode) + '"';
  end;
end;

function SQLiteFieldType(SQLiteFieldTypeCode: Integer): string;
begin
  case SQLiteFieldTypeCode of
    SQLITE_INTEGER:
      Result := 'Integer';
    SQLITE_FLOAT:
      Result := 'Float';
    SQLITE_TEXT:
      Result := 'Text';
    SQLITE_BLOB:
      Result := 'Blob';
    SQLITE_NULL:
      Result := 'Null';
  else
    Result := 'TYPE(' + SQLiteFieldTypeCode.ToString + ')';
  end;
end;

procedure SQLiteContains(Context: Pointer; Arg: Integer; Args: PPointerArray);
var
  S1, S2: string;
  Result: Integer;
begin
  S1 := AnsiUpperCase(string(SQLite3_Value_Text(Args[0])));
  S2 := AnsiUpperCase(string(SQLite3_Value_Text(Args[1])));
  Result := Ord(Pos(S2, S1) <> 0);
  SQLite3_Result_Int(Context, Result);
end;

procedure SQLiteUpper(Context: Pointer; Arg: Integer; Args: PPointerArray);
var
  S: string;
begin
  S := string(SQLite3_Value_Text(Args[0]));
  S := AnsiUpperCase(S);
  SQLite3_Result_Text(Context, PAnsiChar(AnsiString(S)), S.Length, nil);
end;

procedure SQLiteLower(Context: Pointer; Arg: Integer; Args: PPointerArray);
var
  S: string;
begin
  S := string(SQLite3_Value_Text(Args[0]));
  S := AnsiLowerCase(S);
  SQLite3_Result_Text(Context, PAnsiChar(AnsiString(S)), S.Length, nil);
end;

procedure DisposePointer(ptr: Pointer); cdecl;
begin
  if Assigned(ptr) then
    FreeMem(ptr);
end;

function SystemCollate(Userdta: pointer; Buf1Len: integer; Buf1: pointer; Buf2Len: integer; Buf2: pointer): integer; cdecl;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0, PWideChar(Buf1), Buf1Len, PWideChar(Buf2), Buf2Len) - 2;
end;

{ TSQLiteDatabase }

procedure TSQLiteDatabase.Password(Phrase: string; DoXOR: Boolean);
begin
  if DoXOR then
    ExecSQL('PRAGMA key = "' + XORString(Phrase, FSecretKey) + '"')
  else
    ExecSQL('PRAGMA key = "' + Phrase + '"');
end;

procedure TSQLiteDatabase.ChangePassword(Phrase: string; DoXOR: Boolean);
begin
  if DoXOR then
    ExecSQL('PRAGMA key = "' + XORString(Phrase, FSecretKey) + '"')
  else
    ExecSQL('PRAGMA key = "' + Phrase + '"');
end;

procedure TSQLiteDatabase.SetSecretKey(const Value: string);
begin
  FSecretKey := Value;
end;

class procedure TSQLiteDatabase.LoadSqliteLib(SQLiteLib: string);
begin
  if not FIsInit then
  begin
    InitSQL(SQLiteLib);
    FIsInit := True;
  end;
end;

constructor TSQLiteDatabase.Create(const FileName: string);
var
  Msg: PAnsiChar;
begin
  LoadSqliteLib;
  inherited Create;
  FSecretKey := 'D6AFC382FE1JGHDF557496789A56EDDE25AF244AE30D4EDA268CC12AC9E4B92F8';
  FParams := TSQLiteParams.Create;
  Self.FInTrans := False;
  Msg := nil;
  try
    if SQLite3_Open(PAnsiChar(AnsiString(FileName)), FDBInstance) <> SQLITE_OK then
    begin
      if Assigned(FDBInstance) then
      begin
        Msg := Sqlite3_ErrMsg(FDBInstance);
        raise ESQLiteOpen.CreateFmt('Failed to open database "%s" : %s', [FileName, Msg]);
      end
      else
        raise ESQLiteOpen.CreateFmt('Failed to open database "%s" : unknown error', [FileName]);
    end;

    //set a few configs
    //L.G. Do not call it here. Because busy handler is not setted here,
    // any share violation causing exception!
    //    self.ExecSQL('PRAGMA SYNCHRONOUS=NORMAL;');
    //    self.ExecSQL('PRAGMA temp_store = MEMORY;');
  finally
    if Assigned(Msg) then
      SQLite3_Free(Msg);
  end;
end;

procedure TSQLiteDatabase.CreateFunction(const FuncName: string; Addr: TxFunc; ArgCount: Integer);
begin
  SQLite3_Create_Function(Instance, PWideChar(FuncName), ArgCount, SQLITE_ANY, nil, Addr, nil, nil);
end;

destructor TSQLiteDatabase.Destroy;
begin
  if FInTrans then
    Rollback;
  if Assigned(FDBInstance) then
    SQLite3_Close(FDBInstance);
  FParams.Free;
  inherited;
end;

function TSQLiteDatabase.GetLastInsertRowID: Int64;
begin
  Result := Sqlite3_LastInsertRowID(FDBInstance);
end;

function TSQLiteDatabase.GetTotalChanges: Int64;
begin
  Result := SQLite3_TotalChanges(FDBInstance);
end;

procedure TSQLiteDatabase.RaiseError(s: string; SQL: string);
var
  Msg: string;
  ErrCode: Integer;
  Handled: Boolean;
begin
  ErrCode := SQLite3_ErrCode(FDBInstance);
  Msg := string(SQLite3_ErrMsg(FDBInstance));
  if Assigned(FSQLErrorHandle) then
  begin
    FSQLErrorHandle(Self, ErrCode, Msg, Handled);
    if Handled then
      Exit;
  end;
  if ErrCode <> SQLITE_OK then
  begin
    case ErrCode of
      SQLITE_NOTADB:
        raise ESQLiteOpen.CreateFmt('Failed to open database: %s ', [Msg]);
      SQLITE_ERROR:
        raise ESQLiteQueryError.CreateFmt('SQL error or missing database: %s', [Msg]);
      SQLITE_BUSY:
        raise ESQLiteIsBusy.Create('Database is locked');
    end;
  end;

  if Msg <> '' then
    raise ESQLiteUnknown.CreateFmt(s + '.'#13'Error [%d]: %s.'#13'"%s": %s', [ErrCode, SQLiteErrorStr(ErrCode), SQL, Msg])
  else
    raise ESQLiteUnknown.CreateFmt(s, [SQL, 'No message']);
end;

procedure TSQLiteDatabase.SetSynchronised(Value: TSQLiteSynchronous);
begin
  if Value <> FSync then
  begin
    ExecSQL(' PRAGMA synchronous = "' + Value.ToString + '"');
    FSync := Value;
  end;
end;

procedure TSQLiteDatabase.BindData(Stmt: TSQLiteStmt; const Bindings: array of const);
var
  BlobMemStream: TCustomMemoryStream;
  BlobStdStream: TStream;
  DataPtr: Pointer;
  DataSize: Integer;
  AnsiStr: AnsiString;
  AnsiStrPtr: PAnsiString;
  I: Integer;
begin
  for I := Low(Bindings) to High(Bindings) do
  begin
    case Bindings[I].VType of
      vtString, vtAnsiString, vtPChar, vtWideString, vtPWideChar, vtChar, vtWideChar, vtUnicodeString:
        begin
          case Bindings[I].VType of
            vtString:
              begin // ShortString
                AnsiStr := Bindings[I].VString^;
                DataPtr := PAnsiChar(AnsiStr);
                DataSize := Length(AnsiStr) + 1;
              end;
            vtUnicodeString:
              begin
                DataPtr := PAnsiChar(UTF8Encode(string(@Bindings[I].VUnicodeString^)));
                DataSize := -1;
              end;
            vtPChar:
              begin
                DataPtr := Bindings[I].VPChar;
                DataSize := -1;
              end;
            vtAnsiString:
              begin
                AnsiStrPtr := PAnsiString(@Bindings[I].VAnsiString);
                DataPtr := PAnsiChar(AnsiStrPtr^);
                DataSize := Length(AnsiStrPtr^) + 1;
              end;
            vtPWideChar:
              begin
                DataPtr := PAnsiChar(UTF8Encode(WideString(Bindings[I].VPWideChar)));
                DataSize := -1;
              end;
            vtWideString:
              begin
                DataPtr := PAnsiChar(UTF8Encode(PWideString(@Bindings[I].VWideString)^));
                DataSize := -1;
              end;
            vtChar:
              begin
                DataPtr := PAnsiChar(AnsiString(Bindings[I].VChar));  //string typecast
                DataSize := 2;
              end;
            vtWideChar:
              begin
                DataPtr := PAnsiChar(UTF8Encode(WideString(Bindings[I].VWideChar)));
                DataSize := -1;
              end;
          else
            raise ESQLiteUnknownStringType.Create('Unknown string-type');
          end;
          if sqlite3_bind_text(Stmt, I + 1, DataPtr, DataSize, SQLITE_STATIC) <> SQLITE_OK then
            RaiseError('Could not bind text', 'BindData');
        end;
      vtInteger:
        if sqlite3_bind_int(Stmt, I + 1, Bindings[I].VInteger) <> SQLITE_OK then
          RaiseError('Could not bind integer', 'BindData');
      vtInt64:
        if sqlite3_bind_int64(Stmt, I + 1, Bindings[I].VInt64^) <> SQLITE_OK then
          RaiseError('Could not bind int64', 'BindData');
      vtExtended:
        if sqlite3_bind_double(Stmt, I + 1, Bindings[I].VExtended^) <> SQLITE_OK then
          RaiseError('Could not bind extended', 'BindData');
      vtBoolean:
        if sqlite3_bind_int(Stmt, I + 1, Integer(Bindings[I].VBoolean)) <> SQLITE_OK then
          RaiseError('Could not bind boolean', 'BindData');
      vtPointer:
        begin
          if (Bindings[I].VPointer = nil) then
          begin
            if sqlite3_bind_null(Stmt, I + 1) <> SQLITE_OK then
              RaiseError('Could not bind null', 'BindData');
          end
          else
            raise ESQLiteUnhandledPointer.Create('Unhandled pointer (<> nil)');
        end;
      vtObject:
        begin
          if (Bindings[I].VObject is TCustomMemoryStream) then
          begin
            BlobMemStream := TCustomMemoryStream(Bindings[I].VObject);
            if (sqlite3_bind_blob(Stmt, I + 1, @PAnsiChar(BlobMemStream.Memory)[BlobMemStream.Position], BlobMemStream.Size
              - BlobMemStream.Position, SQLITE_STATIC) <> SQLITE_OK) then
              RaiseError('Could not bind BLOB', 'BindData');
          end
          else if (Bindings[I].VObject is TStream) then
          begin
            BlobStdStream := TStream(Bindings[I].VObject);
            DataSize := BlobStdStream.Size;

            GetMem(DataPtr, DataSize);
            if (DataPtr = nil) then
              raise ESQLiteBlob.Create('Error getting memory to save BLOB');
            BlobStdStream.Position := 0;
            BlobStdStream.Read(DataPtr^, DataSize);

            if (sqlite3_bind_blob(Stmt, I + 1, DataPtr, DataSize, @DisposePointer) <> SQLITE_OK) then
              RaiseError('Could not bind BLOB', 'BindData');
          end
          else
            raise ESQLiteUnhandledObjectType.Create('Unhandled object-type in binding');
        end
    else
      begin
        raise ESQLiteUnhandledBinding.Create('Unhandled binding');
      end;
    end;
  end;
end;

procedure TSQLiteDatabase.ExecSQL(const SQL: string);
begin
  ExecSQL(SQL, []);
end;

procedure TSQLiteDatabase.ExecSQL(const SQL: string; const Bindings: array of const);
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: PAnsiChar;
  iStepResult: Integer;
begin
  try
    if Sqlite3_Prepare_v2(FDBInstance, PAnsiChar(AnsiString(SQL)), -1, Stmt, NextSQLStatement) <> SQLITE_OK then
      RaiseError('Error executing SQL', SQL);
    if Stmt = nil then
      RaiseError('Could not prepare SQL statement', SQL);
    DoQuery(SQL);
    SetParams(Stmt);
    BindData(Stmt, Bindings);

    iStepResult := Sqlite3_step(Stmt);
    if (iStepResult <> SQLITE_DONE) then
    begin
      SQLite3_reset(Stmt);
      RaiseError('Error executing SQL statement', SQL);
    end;
  finally
    if Assigned(Stmt) then
      Sqlite3_Finalize(Stmt);
  end;
end;

procedure TSQLiteDatabase.ExecSQL(Query: TSQLiteQuery);
var
  iStepResult: integer;
begin
  if Assigned(Query.Statement) then
  begin
    iStepResult := Sqlite3_step(Query.Statement);
    if (iStepResult <> SQLITE_DONE) then
    begin
      SQLite3_reset(Query.Statement);
      RaiseError('Error executing prepared SQL statement', Query.SQL);
    end;
    Sqlite3_Reset(Query.Statement);
  end;
end;

function TSQLiteDatabase.PrepareSQL(const SQL: string): TSQLiteQuery;
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: PAnsiChar;
begin
  Result.SQL := SQL;
  Result.Statement := nil;

  if Sqlite3_Prepare(FDBInstance, PAnsiChar(AnsiString(SQL)), -1, Stmt, NextSQLStatement) <> SQLITE_OK then
    RaiseError('Error executing SQL', SQL)
  else
    Result.Statement := Stmt;

  if (Result.Statement = nil) then
    RaiseError('Could not prepare SQL statement', SQL);
  DoQuery(SQL);
end;

procedure TSQLiteDatabase.BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: Integer);
begin
  if Assigned(Query.Statement) then
    SQLite3_Bind_Int(Query.Statement, Index, Value)
  else
    RaiseError('Could not bind integer to prepared SQL statement', Query.SQL);
end;

procedure TSQLiteDatabase.BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: string);
begin
  if Assigned(Query.Statement) then
    SQLite3_Bind_Text(Query.Statement, Index, PAnsiChar(AnsiString(Value)), Length(Value), Pointer(SQLITE_STATIC))
  else
    RaiseError('Could not bind string to prepared SQL statement', Query.SQL);
end;

procedure TSQLiteDatabase.ReleaseSQL(Query: TSQLiteQuery);
begin
  if Assigned(Query.Statement) then
  begin
    SQLite3_Finalize(Query.Statement);
    Query.Statement := nil;
  end
  else
    RaiseError('Could not release prepared SQL statement', Query.SQL);
end;

procedure TSQLiteDatabase.UpdateBlob(const SQL: string; BlobData: TStream);
var
  iSize: integer;
  ptr: pointer;
  Stmt: TSQLiteStmt;
  NextSQLStatement: PAnsiChar;
begin
  //expects SQL of the form 'UPDATE MYTABLE SET MYFIELD = ? WHERE MYKEY = 1'
  if Pos('?', SQL) = 0 then
    RaiseError('SQL must include a "?" parameter', SQL);
  try
    if Sqlite3_Prepare_v2(FDBInstance, PAnsiChar(AnsiString(SQL)), -1, Stmt, NextSQLStatement) <> SQLITE_OK then
      RaiseError('Could not prepare SQL statement', SQL);

    if Stmt = nil then
      RaiseError('Could not prepare SQL statement', SQL);

    DoQuery(SQL);

    //now bind the blob data
    iSize := BlobData.size;
    GetMem(ptr, iSize);
    if ptr = nil then
      raise ESQLiteBlob.CreateFmt('Error getting memory to save BLOB', [SQL, 'Error']);

    BlobData.Position := 0;
    BlobData.Read(ptr^, iSize);

    if SQLite3_Bind_Blob(Stmt, 1, ptr, iSize, @DisposePointer) <> SQLITE_OK then
      RaiseError('Error binding blob to database', SQL);

    if SQLite3_Step(Stmt) <> SQLITE_DONE then
    begin
      SQLite3_reset(Stmt);
      RaiseError('Error executing SQL statement', SQL);
    end;
  finally
    begin
      if Assigned(Stmt) then
        SQLite3_Finalize(Stmt);
    end;
  end;
end;

function TSQLiteDatabase.Query(const SQL: string): TSQLiteTable;
begin
  Result := TSQLiteTable.Create(Self, SQL);
end;

function TSQLiteDatabase.Query(const SQL: string; const Bindings: array of const): TSQLiteTable;
begin
  Result := TSQLiteTable.Create(Self, SQL, Bindings);
end;

function TSQLiteDatabase.GetUniTable(const SQL: string): TSQLiteUniTable;
begin
  Result := TSQLiteUniTable.Create(Self, SQL);
end;

function TSQLiteDatabase.GetUniTable(const SQL: string; const Bindings: array of const): TSQLiteUniTable;
begin
  Result := TSQLiteUniTable.Create(Self, SQL, Bindings);
end;

function TSQLiteDatabase.GetTableValue(const SQL: string): int64;
begin
  Result := GetTableValue(SQL, []);
end;

function TSQLiteDatabase.GetTableValue(const SQL: string; const Bindings: array of const): int64;
var
  Table: TSQLiteUniTable;
begin
  Result := -1;
  Table := GetUniTable(SQL, Bindings);
  try
    if not Table.EoF then
      Result := Table.FieldAsInteger(0);
  finally
    Table.Free;
  end;
end;

function TSQLiteDatabase.GetTableString(const SQL: string): string;
begin
  Result := GetTableString(SQL, []);
end;

function TSQLiteDatabase.GetTable(const SQL: string; const Bindings: array of const): TSQLiteTable;
begin
  Result := Query(SQL, Bindings);
end;

function TSQLiteDatabase.GetTable(const SQL: string): TSQLiteTable;
begin
  Result := Query(SQL);
end;

function TSQLiteDatabase.GetTableString(const SQL: string; const Bindings: array of const): string;
var
  Table: TSQLiteUniTable;
begin
  Result := '';
  Table := GetUniTable(SQL, Bindings);
  try
    if not Table.EoF then
      Result := Table.FieldAsString(0);
  finally
    Table.Free;
  end;
end;

function TSQLiteDatabase.GetTableStrings(const SQL: string; const Value: TStrings; FieldNum: Integer; const Bindings: array of const): Boolean;
var
  Table: TSQLiteUniTable;
begin
  Value.Clear;
  Table := GetUniTable(SQL, Bindings);
  try
    while not Table.EoF do
    begin
      Value.Add(Table.FieldAsString(FieldNum));
      Table.Next;
    end;
  finally
    Table.Free;
  end;
  Result := Value.Count > 0;
end;

function TSQLiteDatabase.GetTableStrings(const SQL: string; const Value: TStrings): Boolean;
begin
  Result := GetTableStrings(SQL, Value, 0, []);
end;

function TSQLiteDatabase.Backup(TargetDB, TargetName, SourceName: string): Integer;
var
  Target: TSQLiteDatabase;
begin
  Target := TSQLiteDatabase.Create(TargetDB);
  try
    Result := Backup(Target, TargetName, SourceName);
  finally
    Target.Free;
  end;
end;

function TSQLiteDatabase.Backup(TargetDB: string): Integer;
begin
  Result := Backup(TargetDB, 'main', 'main');
end;

procedure TSQLiteDatabase.BeginTransaction;
begin
  if not FInTrans then
  begin
    ExecSQL('BEGIN TRANSACTION');
    FInTrans := True;
  end
  else
    raise ESQLiteTransaction.Create('Transaction already open');
end;

procedure TSQLiteDatabase.Commit;
begin
  ExecSQL('COMMIT');
  FInTrans := False;
end;

procedure TSQLiteDatabase.Rollback;
begin
  ExecSQL('ROLLBACK');
  FInTrans := False;
end;

function TSQLiteDatabase.TableExists(TableName: string): Boolean;
var
  SQL: string;
  DS: TSQLiteTable;
begin
  //returns true if table exists in the database
  SQL := 'select [sql] from sqlite_master where [type] = ''table'' and lower(name) = ''' + LowerCase(TableName) + ''' ';
  DS := Query(SQL);
  try
    Result := DS.Count > 0;
  finally
    DS.Free;
  end;
end;

procedure TSQLiteDatabase.SetTimeout(Value: integer);
begin
  SQLite3_BusyTimeout(FDBInstance, Value);
  FTimeout := Value;
end;

function TSQLiteDatabase.Version: string;
begin
  Result := string(SQLite3_Version);
end;

procedure TSQLiteDatabase.AddCustomCollate(name: string; Compare: TCollateCompare);
begin
  SQLite3_Create_Collation(FDBInstance, PAnsiChar(AnsiString(name)), SQLITE_UTF8, nil, Compare);
end;

procedure TSQLiteDatabase.AddSystemCollate;
begin
  SQLite3_Create_Collation(FDBInstance, 'SYSTEM', SQLITE_UTF16LE, nil, @SystemCollate);
end;

procedure TSQLiteDatabase.AttachDatabase(const FileName, Alias: string);
begin
  ExecSQL('ATTACH DATABASE ? AS ?', [AnsiString(FileName), AnsiString(Alias)]);
end;

procedure TSQLiteDatabase.DetachDatabase(const Alias: string);
begin
  ExecSQL('DETACH DATABASE ?', [AnsiString(Alias)]);
end;

procedure TSQLiteDatabase.SetParams(Stmt: TSQLiteStmt);
var
  i: integer;
  Param: TSQLiteParam;
begin
  try
    for Param in FParams do
    begin
      i := SQLite3_Bind_Parameter_index(Stmt, PAnsiChar(AnsiString(Param.Name)));
      if i > 0 then
      begin
        case Param.ValueType of
          SQLITE_INTEGER:
            SQLite3_Bind_Int64(Stmt, i, Param.ValueInteger);
          SQLITE_FLOAT:
            SQLite3_Bind_Double(Stmt, i, Param.ValueFloat);
          SQLITE_TEXT:
            SQLite3_Bind_Text(Stmt, i, PAnsiChar(AnsiString(Param.ValueData)), Length(Param.ValueData), SQLITE_TRANSIENT);
          SQLITE_NULL:
            SQLite3_Bind_Null(Stmt, i);
        end;
      end;
    end;
  finally
    FParams.Clear;
  end;
end;

function TSQLiteDatabase.GetRowsChanged: Integer;
begin
  Result := SQLite3_Changes(FDBInstance);
end;

function TSQLiteDatabase.GetSQLofTable(const TableName: string; DataBase: string = ''): string;
begin
  if not DataBase.IsEmpty then
    DataBase := DataBase + '.';
  Result := GetTableString('select sql from ' + DataBase + 'sqlite_master where name = ?', [AnsiString(TableName)]);
end;

procedure TSQLiteDatabase.DoQuery(Value: string);
begin
  if Assigned(OnQuery) then
    OnQuery(Self, Value);
end;

function TSQLiteDatabase.Backup(TargetDB: TSQLiteDatabase; TargetName: string; SourceName: string): Integer;
var
  Backup: TSQLiteBackup;
begin
  Backup := SQLite3_Backup_Init(TargetDB.Instance, PAnsiChar(AnsiString(TargetName)), Instance, PAnsiChar(AnsiString(SourceName)));
  if Backup = nil then
    raise ESQLiteInitializeBackup.Create('Could not initialize backup')
  else
  begin
    try
      Result := SQLITE3_Backup_Step(Backup, -1); //copies entire db
    finally
      SQLite3_Backup_Finish(Backup);
    end;
  end;
end;

function TSQLiteDatabase.Backup(TargetDB: TSQLiteDatabase): Integer;
begin
  Result := Backup(TargetDB, 'main', 'main');
end;

{TSQLiteTable}

constructor TSQLiteTable.Create(DB: TSQLiteDatabase; const SQL: string);
begin
  Create(DB, SQL, []);
end;

constructor TSQLiteTable.Create(DB: TSQLiteDatabase; const SQL: string; const Bindings: array of const);
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: PAnsiChar;
  StepResult: Integer;
  iNumBytes: Integer;
  thisBlobValue: TMemoryStream;
  thisStringValue: PString;
  thisDoubleValue: PDouble;
  thisIntValue: PInt64;
  thisColType: Integer;
  i: Integer;
  DeclaredColType: PAnsiChar;
  ptrValue: PAnsiChar;
begin
  inherited Create;
  try
    FRowCount := 0;
    FColCount := 0;
    //if there are several SQL statements in SQL, NextSQLStatment points to the
    //beginning of the next one. Prepare only prepares the first SQL statement.
    if SQLite3_Prepare_v2(DB.FDBInstance, PAnsiChar(AnsiString(SQL)), -1, Stmt, NextSQLStatement) <> SQLITE_OK then
      DB.RaiseError('Error executing SQL', SQL);
    if Stmt = nil then
      DB.RaiseError('Could not prepare SQL statement', SQL);
    DB.DoQuery(SQL);
    DB.SetParams(Stmt);
    DB.BindData(Stmt, Bindings);

    StepResult := SQLite3_Step(Stmt);
    while StepResult <> SQLITE_DONE do
    begin
      case StepResult of
        SQLITE_ROW:
          begin
            Inc(FRowCount);
            if FRowCount = 1 then
            begin
              FCols := TStringList.Create;
              FColTypes := TList<Integer>.Create;
              FColCount := SQLite3_ColumnCount(Stmt);
              for i := 0 to Pred(FColCount) do
                FCols.Add(AnsiUpperCase(string(SQLite3_ColumnName(Stmt, i))));
              for i := 0 to Pred(FColCount) do
              begin
                DeclaredColType := SQLite3_ColumnDeclType(Stmt, i);
                if DeclaredColType = nil then
                  thisColType := SQLite3_ColumnType(Stmt, i)
                else if (DeclaredColType = SQLITE_INTEGER_STR) or (DeclaredColType = SQLITE_BOOLEAN_STR) then
                  thisColType := SQLITE_INTEGER
                else if (DeclaredColType = SQLITE_NUMERIC_STR) or (DeclaredColType = SQLITE_FLOAT_STR) or (DeclaredColType = SQLITE_DOUBLE_STR) or (DeclaredColType = SQLITE_REAL_STR) then
                  thisColType := SQLITE_FLOAT
                else if DeclaredColType = SQLITE_BLOB_STR then
                  thisColType := SQLITE_BLOB
                else
                  thisColType := SQLITE_TEXT;
                FColTypes.Add(thisColType);
              end;
              FResults := TList.Create;
            end;

            //get column values
            for i := 0 to Pred(ColCount) do
            begin
              case SQLite3_ColumnType(Stmt, i) of
                SQLITE_NULL:
                  begin
                    FResults.Add(nil);
                  end;
                SQLITE_INTEGER:
                  begin
                    New(thisIntValue);
                    thisIntValue^ := SQLite3_ColumnInt64(Stmt, i);
                    FResults.Add(thisIntValue);
                  end;
                SQLITE_FLOAT:
                  begin
                    New(thisDoubleValue);
                    thisDoubleValue^ := SQLite3_ColumnDouble(Stmt, i);
                    FResults.Add(thisDoubleValue);
                  end;
                SQLITE_BLOB:
                  begin
                    iNumBytes := SQLite3_ColumnBytes(Stmt, i);
                    if iNumBytes = 0 then
                      thisBlobValue := nil
                    else
                    begin
                      thisBlobValue := TMemoryStream.Create;
                      thisBlobValue.position := 0;
                      thisBlobValue.WriteBuffer(Sqlite3_ColumnBlob(Stmt, i)^, iNumBytes);
                    end;
                    FResults.Add(thisBlobValue);
                  end;
                SQLITE_TEXT:
                  begin
                    New(thisStringValue);
                    ptrValue := Sqlite3_ColumnText(Stmt, i);
                    SetString(thisStringValue^, ptrValue, Length(ptrValue));
                    FResults.Add(thisStringValue);
                  end;
              end;
            end;
          end;
        SQLITE_BUSY:
          raise ESQLiteIsBusy.CreateFmt('Could not prepare SQL statement. %s. %s.', [SQL, 'SQLite is Busy']);
      else
        begin
          SQLite3_Reset(Stmt);
          DB.RaiseError('Could not retrieve data', SQL);
        end;
      end;
      StepResult := SQLite3_Step(Stmt);
    end;
    FRow := 0;
  finally
    if Assigned(Stmt) then
      SQLite3_Finalize(Stmt);
  end;
end;

destructor TSQLiteTable.Destroy;
var
  i: Cardinal;
  iColNo: Integer;
begin
  if Assigned(FResults) then
  begin
    for i := 0 to FResults.Count - 1 do
    begin
     //check for blob type
      iColNo := (i mod FColCount);
      case FColTypes[iColNo] of
        SQLITE_BLOB:
          if Assigned(TMemoryStream(FResults[i])) then
            TMemoryStream(FResults[i]).Free;
        SQLITE_TEXT:
          if FResults[i] <> nil then
          begin
            SetString(string(FResults[i]^), nil, 0);
            Dispose(FResults[i]);
          end;
      else
        Dispose(FResults[i]);
      end;
    end;
    FResults.Free;
  end;
  if Assigned(FCols) then
    FCols.Free;
  if Assigned(FColTypes) then
    FColTypes.Free;
  inherited;
end;

function TSQLiteTable.GetColumns(I: Integer): string;
begin
  Result := FCols[I];
end;

function TSQLiteTable.GetCountResult: Integer;
begin
  if not EoF then
    Result := StrToInt(Fields[0])
  else
    Result := 0;
end;

function TSQLiteTable.GetCount: Integer;
begin
  Result := FRowCount;
end;

function TSQLiteTable.GetEOF: Boolean;
begin
  Result := FRow >= FRowCount;
end;

function TSQLiteTable.GetBOF: Boolean;
begin
  Result := FRow <= 0;
end;

function TSQLiteTable.GetFieldByName(FieldName: string): string;
begin
  Result := GetFields(GetFieldIndex(FieldName));
end;

function TSQLiteTable.GetFieldIndex(FieldName: string): Integer;
begin
  if FCols = nil then
  begin
    raise ESQLiteFieldNotFound.Create('Field ' + FieldName + ' Not found. Empty dataset');
  end;

  if FCols.count = 0 then
  begin
    raise ESQLiteFieldNotFound.Create('Field ' + FieldName + ' Not found. Empty dataset');
  end;

  Result := FCols.IndexOf(AnsiUpperCase(FieldName));
  if Result < 0 then
  begin
    raise ESQLiteFieldNotFound.Create('Field not found in dataset: ' + FieldName)
  end;
end;

function TSQLiteTable.GetFields(i: Cardinal): string;
var
  StringValue: PString;
begin
  Result := '';
  if EoF then
    raise ESQLiteTableEOF.Create(ERROR_EOF);

  case FColTypes[i] of
    SQLITE_TEXT:
      begin
        StringValue := FResults[(FRow * FColCount) + i];
        if StringValue <> nil then
          Result := StringValue^
        else
          Result := '';
      end;
    SQLITE_INTEGER:
      Result := IntToStr(FieldAsInteger(i));
    SQLITE_FLOAT:
      Result := FloatToStr(FieldAsDouble(i));
    SQLITE_BLOB:
      Result := FieldAsBlobText(i);
  else
    Result := 'DATA';
  end;
end;

function TSQLiteTable.FieldAsBlob(i: Cardinal): TMemoryStream;
begin
  if EoF then
    raise ESQLiteTableEOF.Create(ERROR_EOF);
  if FResults[(FRow * FColCount) + i] = nil then
    Result := nil
  else if FColTypes[i] = SQLITE_BLOB then
    Result := TMemoryStream(FResults[(FRow * FColCount) + i])
  else
    raise ESQLiteBlob.Create('Not a BLOB field');
end;

function TSQLiteTable.FieldAsBlobText(i: Cardinal): string;
var
  MemStream: TMemoryStream;
    {$IFDEF UNICODE}
  Buffer: PAnsiChar;
    {$ELSE}
  Buffer: PWideChar;
    {$ENDIF}
begin
  Result := '';
  MemStream := FieldAsBlob(i);
  if MemStream <> nil then
    if MemStream.Size > 0 then
    begin
      MemStream.Position := 0;
   {$IFDEF UNICODE}
      Buffer := AnsiStrAlloc(MemStream.Size + 1);
   {$ELSE}
      Buffer := StrAlloc(MemStream.Size + 1);
   {$ENDIF}
      MemStream.readbuffer(Buffer[0], MemStream.Size);
      (Buffer + MemStream.Size)^ := Chr(0);
      SetString(Result, Buffer, MemStream.size);
      StrDispose(Buffer);
    end; //do not free the TMemoryStream here; it is freed when //TSqliteTable is destroyed
end;

function TSQLiteTable.FieldAsBoolean(i: Cardinal): Boolean;
begin
  if EoF then
    raise ESQLiteTableEOF.Create(ERROR_EOF);
  if FResults[(FRow * FColCount) + i] = nil then
    Result := False
  else if FColTypes[i] = SQLITE_INTEGER then
    Result := Boolean(pInt64(FResults[(FRow * FColCount) + i])^)
  else if FColTypes[i] = SQLITE_FLOAT then
    Result := Boolean(Trunc(strtofloat(pString(FResults[(FRow * FColCount) + i])^)))
  else
    raise ESQLiteTypeError.Create('Not an Integer or Numeric field');
end;

function TSqliteTable.FieldAsInteger(i: Cardinal): Int64;
begin
  if EoF then
    raise ESQLiteTableEOF.Create(ERROR_EOF);
  if FResults[(FRow * FColCount) + i] = nil then
    Result := 0
  else if FColTypes[i] = SQLITE_INTEGER then
    Result := pInt64(FResults[(FRow * FColCount) + i])^
  else if FColTypes[i] = SQLITE_FLOAT then
    Result := Trunc(strtofloat(pString(FResults[(FRow * FColCount) + i])^))
  else
    raise ESQLiteTypeError.Create('Not an Integer or Numeric field');
end;

function TSQLiteTable.FieldAsDateTime(i: Cardinal): TDateTime;
begin
  Result := FieldAsDouble(i);
end;

function TSQLiteTable.FieldAsDouble(i: Cardinal): Double;
begin
  if EoF then
    raise ESQLiteTableEOF.Create(ERROR_EOF);
  if FResults[(FRow * FColCount) + i] = nil then
    Result := 0
  else if FColTypes[i] = SQLITE_INTEGER then
    Result := pInt64(FResults[(FRow * FColCount) + i])^
  else if FColTypes[i] = SQLITE_FLOAT then
    Result := pDouble(FResults[(FRow * FColCount) + i])^
  else
    raise ESQLiteTypeError.Create('Not an Integer or Numeric field');
end;

function TSQLiteTable.FieldAsString(i: Cardinal): string;
begin
  if EoF then
    raise ESQLiteTableEOF.Create(ERROR_EOF);
  if FResults[(FRow * FColCount) + i] = nil then
    Result := ''
  else
    Result := GetFields(i);
end;

function TSQLiteTable.FieldIsNull(i: Cardinal): Boolean;
var
  Value: Pointer;
begin
  if EoF then
    raise ESQLiteTableEOF.Create(ERROR_EOF);
  Value := FResults[(FRow * FColCount) + i];
  Result := Value = nil;
end;

function TSQLiteTable.FieldAsBlob(FieldName: string): TMemoryStream;
begin
  Result := FieldAsBlob(GetFieldIndex(FieldName));
end;

function TSQLiteTable.FieldAsBlobText(FieldName: string): string;
begin
  Result := FieldAsBlobText(GetFieldIndex(FieldName));
end;

function TSQLiteTable.FieldAsBoolean(FieldName: string): Boolean;
begin
  Result := FieldAsBoolean(GetFieldIndex(FieldName));
end;

function TSQLiteTable.FieldAsDateTime(FieldName: string): TDateTime;
begin
  Result := FieldAsDateTime(GetFieldIndex(FieldName));
end;

function TSQLiteTable.FieldAsDouble(FieldName: string): Double;
begin
  Result := FieldAsDouble(GetFieldIndex(FieldName));
end;

function TSQLiteTable.FieldAsInteger(FieldName: string): Int64;
begin
  Result := FieldAsInteger(GetFieldIndex(FieldName));
end;

function TSQLiteTable.FieldAsString(FieldName: string): string;
begin
  Result := FieldAsString(GetFieldIndex(FieldName));
end;

function TSQLiteTable.FieldIsNull(FieldName: string): Boolean;
begin
  Result := FieldIsNull(GetFieldIndex(FieldName));
end;

function TSQLiteTable.ToArray: TArray<TArray<string>>;
begin
  MoveFirst;
  Result := [];
  while not EoF do
  begin
    Result := Result + [FieldsToArray];
    Next;
  end;
  MoveFirst;
end;

function TSQLiteTable.FieldsToArray: TArray<string>;
var
  i: Integer;
begin
  Result := [];
  for i := 0 to ColCount - 1 do
    Result := Result + [FieldAsString(i)];
end;

function TSQLiteTable.Next: Boolean;
begin
  Result := False;
  if not EoF then
  begin
    Inc(FRow);
    Result := True;
  end;
end;

function TSQLiteTable.Previous: Boolean;
begin
  Result := False;
  if not BoF then
  begin
    Dec(FRow);
    Result := True;
  end;
end;

function TSQLiteTable.MoveFirst: Boolean;
begin
  Result := False;
  if FRowCount > 0 then
  begin
    FRow := 0;
    Result := True;
  end;
end;

function TSQLiteTable.MoveLast: Boolean;
begin
  Result := False;
  if FRowCount > 0 then
  begin
    FRow := FRowCount - 1;
    Result := True;
  end;
end;

function TSQLiteTable.MoveTo(Position: Cardinal): Boolean;
begin
  Result := False;
  if (FRowCount > 0) and (FRowCount > Position) then
  begin
    FRow := Position;
    Result := True;
  end;
end;

{ TSQLiteUniTable }

constructor TSQLiteUniTable.Create(DB: TSQLiteDatabase; const SQL: string);
begin
  Create(DB, SQL, []);
end;

constructor TSQLiteUniTable.Create(DB: TSQLiteDatabase; const SQL: string; const Bindings: array of const);
var
  NextSQLStatement: PAnsiChar;
  i: Integer;
begin
  inherited Create;
  FDB := DB;
  FEOF := False;
  FRow := 0;
  FColCount := 0;
  FSQL := SQL;
  if SQLite3_Prepare_v2(DB.FDBInstance, PAnsiChar(AnsiString(SQL)), -1, FStmt, NextSQLStatement) <> SQLITE_OK then
    DB.RaiseError('Error executing SQL', SQL);
  if FStmt = nil then
    DB.RaiseError('Could not prepare SQL statement', SQL);
  DB.DoQuery(SQL);
  DB.SetParams(FStmt);
  DB.BindData(FStmt, Bindings);

  //get data types
  FCols := TStringList.Create;
  FColCount := SQLite3_ColumnCount(FStmt);
  for i := 0 to Pred(FColCount) do
    FCols.Add(AnsiUpperCase(string(SQLite3_ColumnName(FStmt, i))));
  Next;
end;

destructor TSQLiteUniTable.Destroy;
begin
  if Assigned(FStmt) then
    SQLite3_Finalize(FStmt);
  if Assigned(FCols) then
    FCols.Free;
  inherited;
end;

function TSQLiteUniTable.FieldAsBlob(i: Cardinal): TMemoryStream;
var
  iNumBytes: Integer;
begin
  Result := TMemoryStream.Create;
  iNumBytes := SQLite3_ColumnBytes(FStmt, i);
  if iNumBytes > 0 then
  begin
    Result.WriteBuffer(SQLite3_ColumnBlob(FStmt, i)^, iNumBytes);
    Result.Position := 0;
  end;
end;

function TSQLiteUniTable.FieldAsBlob(FieldName: string): TMemoryStream;
begin
  Result := FieldAsBlob(GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.FieldAsBlobPtr(i: Cardinal; out iNumBytes: Integer): Pointer;
begin
  iNumBytes := SQLite3_ColumnBytes(FStmt, i);
  Result := SQLite3_ColumnBlob(FStmt, i);
end;

function TSQLiteUniTable.FieldAsBlobText(FieldName: string): string;
begin
  Result := FieldAsBlobText(GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.FieldAsBlobText(i: Cardinal): string;
var
  MemStream: TMemoryStream;
    {$IFDEF UNICODE}
  Buffer: PAnsiChar;
    {$ELSE}
  Buffer: PWideChar;
    {$ENDIF}
begin
  Result := '';
  MemStream := Self.FieldAsBlob(i);
  if MemStream <> nil then
  try
    if MemStream.Size > 0 then
    begin
      MemStream.Position := 0;
     {$IFDEF UNICODE}
      Buffer := AnsiStrAlloc(MemStream.Size + 1);
     {$ELSE}
      Buffer := StrAlloc(MemStream.Size + 1);
     {$ENDIF}
      MemStream.ReadBuffer(Buffer[0], MemStream.Size);
      (Buffer + MemStream.Size)^ := Chr(0);
      SetString(Result, Buffer, MemStream.size);
      StrDispose(Buffer);
    end;
  finally
    MemStream.Free;
  end
end;

function TSQLiteUniTable.FieldAsDouble(i: Cardinal): Double;
begin
  Result := SQLite3_ColumnDouble(FStmt, i);
end;

function TSQLiteUniTable.FieldAsInteger(i: Cardinal): Int64;
begin
  Result := SQLite3_ColumnInt64(FStmt, i);
end;

function TSQLiteUniTable.FieldAsString(i: Cardinal): string;
begin
  Result := Self.GetFields(i);
end;

function TSQLiteUniTable.FieldIsNull(i: Cardinal): Boolean;
begin
  Result := SQLite3_ColumnText(FStmt, i) = nil;
end;

function TSQLiteUniTable.GetColumns(i: Integer): string;
begin
  Result := FCols[i];
end;

function TSQLiteUniTable.GetFieldByName(FieldName: string): string;
begin
  Result := GetFields(Self.GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.GetFieldIndex(FieldName: string): Integer;
begin
  if FCols = nil then
    raise ESQLiteFieldNotFound.Create('Field ' + FieldName + ' Not found. Empty dataset');
  if FCols.count = 0 then
    raise ESQLiteFieldNotFound.Create('Field ' + FieldName + ' Not found. Empty dataset');
  Result := FCols.IndexOf(AnsiUpperCase(FieldName));
  if Result < 0 then
    raise ESQLiteFieldNotFound.Create('Field not found in dataset: ' + FieldName)
end;

function TSQLiteUniTable.GetFields(i: Cardinal): string;
begin
  Result := string(SQLite3_ColumnText(FStmt, i));
end;

function TSQLiteUniTable.Next: Boolean;
begin
  FEOF := True;
  case SQLite3_Step(FStmt) of
    SQLITE_ROW:
      begin
        FEOF := False;
        Inc(FRow);
      end;
    SQLITE_DONE:
      begin
        // we are on the end of dataset
        // return EOF=true only
      end;
  else
    begin
      SQLite3_Reset(FStmt);
      FDB.RaiseError('Could not retrieve data', FSQL);
    end;
  end;
  Result := not FEOF;
end;

function TSQLiteUniTable.FieldAsBoolean(FieldName: string): Boolean;
begin
  Result := FieldAsBoolean(GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.FieldAsBoolean(i: Cardinal): Boolean;
begin
  Result := Boolean(Trunc(SQLite3_ColumnInt(FStmt, i)));
end;

function TSQLiteUniTable.FieldAsDateTime(i: Cardinal): TDateTime;
begin
  Result := FieldAsDouble(i);
end;

function TSQLiteUniTable.FieldAsDateTime(FieldName: string): TDateTime;
begin
  Result := FieldAsDateTime(GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.FieldAsDouble(FieldName: string): Double;
begin
  Result := FieldAsDouble(GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.FieldAsInteger(FieldName: string): Int64;
begin
  Result := FieldAsInteger(GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.FieldAsString(FieldName: string): string;
begin
  Result := FieldAsString(GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.FieldIsNull(FieldName: string): Boolean;
begin
  Result := FieldIsNull(GetFieldIndex(FieldName));
end;

{ TSQLiteSynchronousHelper }

function TSQLiteSynchronousHelper.ToString: string;
begin
  case Self of
    ssOFF:
      Result := 'OFF';
    ssNORMAL:
      Result := 'NORMAL';
    ssFULL:
      Result := 'FULL';
    ssEXTRA:
      Result := 'EXTRA';
  end;
end;

{TSQLiteParams}

procedure TSQLiteParams.Add(Name: string; Value: Int64);
var
  Param: TSQLiteParam;
begin
  Param := TSQLiteParam.Create;
  Param.Name := Name;
  Param.ValueType := SQLITE_INTEGER;
  Param.ValueInteger := Value;
  Add(Param);
end;

procedure TSQLiteParams.Add(Name: string; Value: Double);
var
  Param: TSQLiteParam;
begin
  Param := TSQLiteParam.Create;
  Param.Name := Name;
  Param.ValueType := SQLITE_FLOAT;
  Param.ValueFloat := Value;
  Add(Param);
end;

procedure TSQLiteParams.Add(Name: string; Value: string);
var
  Param: TSQLiteParam;
begin
  Param := TSQLiteParam.Create;
  Param.Name := Name;
  Param.ValueType := SQLITE_TEXT;
  Param.ValueData := Value;
  Add(Param);
end;

procedure TSQLiteParams.Add(Name: string);
var
  Param: TSQLiteParam;
begin
  Param := TSQLiteParam.Create;
  Param.Name := Name;
  Param.ValueType := SQLITE_NULL;
  Add(Param);
end;

initialization

finalization
  if TSQLiteDatabase.FIsInit then
    UnInitSQL;

end.

