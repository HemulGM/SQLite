unit SQLiteTable3;

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
   3. Исправлены ошибки форматировния и наведён косметический вид
}

interface

{$UNDEF UNICODE}

{$IFDEF FPC}
  {$MODE Delphi}{$H+}
{$ENDIF}

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  SQLite3, Classes, SysUtils;

const

  dtInt     = 1;
  dtNumeric = 2;
  dtStr     = 3;
  dtBlob    = 4;
  dtNull    = 5;

type

  ESQLiteException           = class(Exception)end;
  ESQLiteBlob                = class(ESQLiteException)end;
  ESQLiteOpen                = class(ESQLiteException)end;
  ESQLiteQueryError          = class(ESQLiteException)end;
  ESQLiteUnknown             = class(ESQLiteException)end;
  ESQLiteUnknownStringType   = class(ESQLiteException)end;
  ESQLiteUnhandledPointer    = class(ESQLiteException)end;
  ESQLiteUnhandledObjectType = class(ESQLiteException)end;
  ESQLiteUnhandledBinding    = class(ESQLiteException)end;
  ESQLiteTransaction         = class(ESQLiteException)end;
  ESQLiteInitializeBackup    = class(ESQLiteException)end;
  ESQLiteIsBusy              = class(ESQLiteException)end;
  ESQLiteFieldNotFound       = class(ESQLiteException)end;
  ESQLiteTableEOF            = class(ESQLiteException)end;
  ESQLiteTypeError           = class(ESQLiteException)end;

  TSQliteParam = class
   public
    Name:string;
    ValueType:Integer;
    ValueInteger:Int64;
    ValueFloat:Double;
    ValueData:string;
  end;

  THookQuery = procedure(Sender:TObject; SQL:string) of object;

  TSQLiteQuery = record
   SQL:String;
   Statement:TSQLiteStmt;
  end;
  TSQLiteDatabase = class;
  TSQLiteTable = class;
  TSQLiteUniTable = class;
  TSQLErrorHandle = procedure(Sender:TSQLiteDatabase; ErrorCode:Integer; Msg:string; var Handle:Boolean) of object;
  TSQLiteDatabase = class
   private
    fDB:TSQLiteDB;
    fInTrans:Boolean;
    FOnQuery:THookQuery;
    fParams:TList;
    fSync:Boolean;
    FSQLErrorHandle:TSQLErrorHandle;
    function GetRowsChanged:Integer;
    procedure BindData(Stmt:TSQLiteStmt; const Bindings:array of const);
    procedure RaiseError(s:string; SQL:string);
    procedure SetParams(Stmt:TSQLiteStmt);
   protected
    procedure DoQuery(Value: string);
    procedure SetSynchronised(Value:Boolean);
   public
    function Backup(TargetDB:TSQLiteDatabase):Integer; Overload;
    function Backup(TargetDB:TSQLiteDatabase; targetName:Ansistring; sourceName:Ansistring):Integer; Overload;
    function GetLastChangedRows:Int64;
    function GetLastInsertRowID:Int64;
    function GetTable(const SQL:Ansistring):TSQLiteTable; overload;
    function GetTable(const SQL:Ansistring; const Bindings:array of const):TSQLiteTable; overload;
    function GetTableString(const SQL:Ansistring): string; overload;
    function GetTableString(const SQL:Ansistring; const Bindings:array of const):string; overload;
    function GetTableValue(const SQL:Ansistring):int64; overload;
    function GetTableValue(const SQL:Ansistring; const Bindings:array of const):int64; overload;
    function GetUniTable(const SQL:Ansistring):TSQLiteUniTable; overload;
    function GetUniTable(const SQL:Ansistring; const Bindings:array of const):TSQLiteUniTable; overload;
    function PrepareSQL(const SQL:Ansistring):TSQLiteQuery;
    function TableExists(TableName:string):Boolean;
    function Version:string;
    //adds collate named SYSTEM for correct data sorting by user's locale
    procedure AddCustomCollate(Name:string; xCompare: TCollateXCompare);
    procedure AddParamFloat(Name:string; Value:Double);
    procedure AddParamInt(Name:string; Value:Int64);
    procedure AddParamNull(Name:string);
    procedure AddParamText(Name:string; Value:string);
    Procedure AddSystemCollate;
    procedure BeginTransaction;
    procedure BindSQL(Query:TSQLiteQuery; const Index:Integer; const Value:Integer); overload;
    procedure BindSQL(Query:TSQLiteQuery; const Index:Integer; const Value:string); overload;
    procedure Commit;
    procedure ExecSQL(const SQL:Ansistring); overload;
    procedure ExecSQL(const SQL:Ansistring; const Bindings:array of const); overload;
    procedure ExecSQL(Query:TSQLiteQuery); overload;
    procedure GetTableStrings(const SQL:Ansistring; const Value:TStrings; FieldNum:Integer = 0);
    procedure ParamsClear;
    procedure ReleaseSQL(Query:TSQLiteQuery);
    procedure Rollback;
    procedure SetTimeout(Value:Integer);
    procedure UpdateBlob(const SQL:Ansistring; BlobData:TStream);
    property DB:TSQLiteDB read fDB;
    constructor Create(const FileName:string);
    destructor Destroy; override;
  published
    property IsTransactionOpen:Boolean read fInTrans;
    property OnQuery:THookQuery read FOnQuery write FOnQuery;
    property ErrorHandler:TSQLErrorHandle read FSQLErrorHandle write FSQLErrorHandle;
    //database rows that were changed (or inserted or deleted) by the most recent SQL statement
    property RowsChanged:Integer read getRowsChanged;
    property Synchronised:Boolean read FSync write SetSynchronised;
  end;

  TSQLiteTable = class
   private
    fColCount:Cardinal;
    fCols:TStringList;
    fColTypes:TList;
    fResults:TList;
    fRow:Cardinal;
    fRowCount:Cardinal;
    function GetBOF:Boolean;
    function GetColumns(i:integer):string;
    function GetCount:Integer;
    function GetCountResult:Integer;
    function GetEOF:Boolean;
    function GetFieldByName(FieldName:string):string;
    function GetFieldIndex(FieldName:string):Integer;
    function GetFields(i:Cardinal):string;
   public
    function FieldAsBlob(i:Cardinal):TMemoryStream;
    function FieldAsBlobText(i:Cardinal):string;
    function FieldAsDouble(i:Cardinal):Double;
    function FieldAsDateTime(i:Cardinal):TDateTime;
    function FieldAsInteger(i:Cardinal):Int64;
    function FieldAsBoolean(i:Cardinal):Boolean;
    function FieldAsString(i:Cardinal):string;
    function FieldIsNull(i:Cardinal):Boolean;
    function MoveFirst:Boolean;
    function MoveLast:Boolean;
    function MoveTo(Position:Cardinal):Boolean;
    function Next:Boolean;
    function Previous:Boolean;
    property BOF:Boolean read GetBOF;
    property ColCount:Cardinal read fColCount;
    property Columns[i:Integer]:string read GetColumns;
    property Count:Integer read GetCount;
    // The property CountResult is used when you execute count(*) queries.
    // It returns 0 if the result set is empty or the value of the
    // first field as an integer.
    property CountResult:Integer read GetCountResult;
    property EOF:Boolean read GetEOF;
    property FieldByName[FieldName:string]:string read GetFieldByName;
    property FieldIndex[FieldName:string]:integer read GetFieldIndex;
    property Fields[i:Cardinal]:string read GetFields;
    property Row:Cardinal read fRow;
    property RowCount:Cardinal read fRowCount;
    constructor Create(DB:TSQLiteDatabase; const SQL:Ansistring); overload;
    constructor Create(DB:TSQLiteDatabase; const SQL:Ansistring; const Bindings:array of const); overload;
    destructor Destroy; override;
  end;

  TSQLiteUniTable = class
  private
    fColCount:Cardinal;
    fCols:TStringList;
    fDB:TSQLiteDatabase;
    fEOF:Boolean;
    fRow:Cardinal;
    fSQL:string;
    fStmt:TSQLiteStmt;
    function GetColumns(i:Integer):string;
    function GetFieldByName(FieldName:string):string;
    function GetFieldIndex(FieldName:string):Integer;
    function GetFields(i:Cardinal):string;
  public
    function FieldAsBlob(i:Cardinal):TMemoryStream;
    function FieldAsBlobPtr(i:Cardinal; out iNumBytes: integer): Pointer;
    function FieldAsBlobText(i:Cardinal):string;
    function FieldAsDouble(i:Cardinal):Double;
    function FieldAsInteger(i:Cardinal):Int64;
    function FieldAsString(i:Cardinal):string;
    function FieldIsNull(i:Cardinal):Boolean;
    function Next:Boolean;
    property ColCount:Cardinal read fColCount;
    property Columns[i:Integer]:string read GetColumns;
    property EOF:Boolean read FEOF;
    property FieldByName[FieldName:string]:string read GetFieldByName;
    property FieldIndex[FieldName:string]:Integer read GetFieldIndex;
    property Fields[i:Cardinal]:string read GetFields;
    property Row:Cardinal read fRow;
    constructor Create(DB:TSQLiteDatabase; const SQL:Ansistring); overload;
    constructor Create(DB:TSQLiteDatabase; const SQL:Ansistring; const Bindings:array of const); overload;
    destructor Destroy; override;
  end;

procedure DisposePointer(ptr: pointer); cdecl;

{$IFDEF WIN32}
function SystemCollate(Userdta: pointer; Buf1Len: integer; Buf1: pointer; Buf2Len: integer; Buf2: pointer): integer; cdecl;
{$ENDIF}

implementation

procedure DisposePointer(ptr:Pointer); cdecl;
begin
 if Assigned(ptr) then FreeMem(ptr);
end;

{$IFDEF WIN32}
function SystemCollate(Userdta: pointer; Buf1Len: integer; Buf1: pointer; Buf2Len: integer; Buf2: pointer): integer; cdecl;
begin
 Result:=CompareStringW(LOCALE_USER_DEFAULT, 0, PWideChar(Buf1), Buf1Len, PWideChar(Buf2), Buf2Len) - 2;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// TSQLiteDatabase
//------------------------------------------------------------------------------

constructor TSQLiteDatabase.Create(const FileName: string);
var Msg:PAnsiChar;
    iResult:Integer;
    utf8FileName:UTF8string;
begin
 inherited Create;
 fParams:=TList.Create;
 Self.fInTrans:=False;
 Msg:=nil;
 try
  utf8FileName:=UTF8String(FileName);
  iResult:=SQLite3_Open(PAnsiChar(utf8FileName), Fdb);
  if iResult <> SQLITE_OK then
   begin
    if Assigned(Fdb) then
     begin
      Msg:= Sqlite3_ErrMsg(Fdb);
      raise ESQLiteOpen.CreateFmt('Failed to open database "%s" : %s', [FileName, Msg]);
     end
    else raise ESQLiteOpen.CreateFmt('Failed to open database "%s" : unknown error', [FileName]);
   end;

//set a few configs
//L.G. Do not call it here. Because busy handler is not setted here,
// any share violation causing exception!

//    self.ExecSQL('PRAGMA SYNCHRONOUS=NORMAL;');
//    self.ExecSQL('PRAGMA temp_store = MEMORY;');

 finally
  if Assigned(Msg) then SQLite3_Free(Msg);
 end;
end;

//..............................................................................

destructor TSQLiteDatabase.Destroy;
begin
 if Self.fInTrans then Self.Rollback;  //assume rollback
 if Assigned(fDB) then SQLite3_Close(fDB);
 ParamsClear;
 fParams.Free;
 inherited;
end;

function TSQLiteDatabase.GetLastInsertRowID:Int64;
begin
 Result:=Sqlite3_LastInsertRowID(self.fDB);
end;

function TSQLiteDatabase.GetLastChangedRows:Int64;
begin
 Result:=SQLite3_TotalChanges(self.fDB);
end;

//..............................................................................

procedure TSQLiteDatabase.RaiseError(s:string; SQL:string);
var Msg:string;
    ErrCode:Integer;
    Handled:Boolean;
begin
 ErrCode:=SQLite3_ErrCode(self.fDB);
 Msg:=SQLite3_ErrMsg(self.fDB);
 if Assigned(FSQLErrorHandle) then
  begin
   FSQLErrorHandle(Self, ErrCode, Msg, Handled);
   if Handled then Exit;
  end;
 if ErrCode <> SQLITE_OK then
  begin
   case ErrCode of
    SQLITE_NOTADB:raise ESQLiteOpen.CreateFmt('Failed to open database: %s ', [Msg]);
    SQLITE_ERROR :raise ESQLiteQueryError.CreateFmt('SQL error or missing database: %s', [Msg]);
    SQLITE_BUSY  :raise ESQLiteIsBusy.Create('Database is locked');
   end;
  end;

 if Msg <> '' then
  raise ESQLiteUnknown.CreateFmt(s +'.'#13'Error [%d]: %s.'#13'"%s": %s', [ErrCode, SQLiteErrorStr(ErrCode), SQL, Msg])
 else raise ESQLiteUnknown.CreateFmt(s, [SQL, 'No message']);
end;

procedure TSQLiteDatabase.SetSynchronised(Value:Boolean);
begin
 if Value <> fSync then
  begin
   if Value then
        ExecSQL('PRAGMA synchronous = ON;')
   else ExecSQL('PRAGMA synchronous = OFF;');
   fSync:= Value;
  end;
end;

procedure TSQLiteDatabase.BindData(Stmt:TSQLiteStmt; const Bindings:array of const);
var BlobMemStream:TCustomMemoryStream;
    BlobStdStream:TStream;
    DataPtr:Pointer;
    DataSize:Integer;
    AnsiStr:AnsiString;
    AnsiStrPtr:PAnsiString;
    I:Integer;
begin
 for I:= Low(Bindings) to High(Bindings) do
  begin
   case Bindings[I].VType of
    vtString,
    vtAnsiString,
    vtPChar,
    vtWideString,
    vtPWideChar,
    vtChar,
    vtWideChar:
     begin
      case Bindings[I].VType of
       vtString:
        begin // ShortString
         AnsiStr:= Bindings[I].VString^;
         DataPtr:= PAnsiChar(AnsiStr);
         DataSize:= Length(AnsiStr)+1;
        end;
       vtPChar:
        begin
         DataPtr:= Bindings[I].VPChar;
         DataSize:= -1;
        end;
       vtAnsiString:
        begin
         AnsiStrPtr:= PAnsiString(@Bindings[I].VAnsiString);
         DataPtr:= PAnsiChar(AnsiStrPtr^);
         DataSize:= Length(AnsiStrPtr^)+1;
        end;
       vtPWideChar:
        begin
         DataPtr:= PAnsiChar(UTF8Encode(WideString(Bindings[I].VPWideChar)));
         DataSize:= -1;
        end;
       vtWideString:
        begin
         DataPtr:= PAnsiChar(UTF8Encode(PWideString(@Bindings[I].VWideString)^));
         DataSize:= -1;
        end;
       vtChar:
        begin
         DataPtr:= PAnsiChar(String(Bindings[I].VChar));
         DataSize:= 2;
        end;
       vtWideChar:
        begin
         DataPtr:= PAnsiChar(UTF8Encode(WideString(Bindings[I].VWideChar)));
         DataSize:= -1;
        end;
      else
       raise ESQLiteUnknownStringType.Create('Unknown string-type');
      end;
      if (sqlite3_bind_text(Stmt, I+1, DataPtr, DataSize, SQLITE_STATIC) <> SQLITE_OK) then RaiseError('Could not bind text', 'BindData');
     end;
    vtInteger:  if (sqlite3_bind_int(Stmt, I+1, Bindings[I].VInteger) <> SQLITE_OK) then RaiseError('Could not bind integer', 'BindData');
    vtInt64:    if (sqlite3_bind_int64(Stmt, I+1, Bindings[I].VInt64^) <> SQLITE_OK) then RaiseError('Could not bind int64', 'BindData');
    vtExtended: if (sqlite3_bind_double(Stmt, I+1, Bindings[I].VExtended^) <> SQLITE_OK) then RaiseError('Could not bind extended', 'BindData');
    vtBoolean:  if (sqlite3_bind_int(Stmt, I+1, Integer(Bindings[I].VBoolean)) <> SQLITE_OK) then RaiseError('Could not bind boolean', 'BindData');
    vtPointer:
     begin
      if (Bindings[I].VPointer = nil) then
       begin
        if (sqlite3_bind_null(Stmt, I+1) <> SQLITE_OK) then RaiseError('Could not bind null', 'BindData');
       end
      else raise ESQLiteUnhandledPointer.Create('Unhandled pointer (<> nil)');
     end;
    vtObject:
     begin
      if (Bindings[I].VObject is TCustomMemoryStream) then
       begin
        BlobMemStream := TCustomMemoryStream(Bindings[I].VObject);
        if (sqlite3_bind_blob(Stmt, I+1, @PAnsiChar(BlobMemStream.Memory)[BlobMemStream.Position],
            BlobMemStream.Size-BlobMemStream.Position, SQLITE_STATIC) <> SQLITE_OK)
        then RaiseError('Could not bind BLOB', 'BindData');
       end
      else
      if (Bindings[I].VObject is TStream) then
       begin
        BlobStdStream:=TStream(Bindings[I].VObject);
        DataSize:=BlobStdStream.Size;

        GetMem(DataPtr, DataSize);
        if (DataPtr = nil) then  raise ESQLiteBlob.Create('Error getting memory to save BLOB');
        BlobStdStream.Position:= 0;
        BlobStdStream.Read(DataPtr^, DataSize);

        if (sqlite3_bind_blob(stmt, I+1, DataPtr, DataSize, @DisposePointer) <> SQLITE_OK) then RaiseError('Could not bind BLOB', 'BindData');
       end
      else raise ESQLiteUnhandledObjectType.Create('Unhandled object-type in binding');
     end
   else
    begin
     raise ESQLiteUnhandledBinding.Create('Unhandled binding');
    end;
   end;
  end;
end;

procedure TSQLiteDatabase.ExecSQL(const SQL:AnsiString);
begin
 ExecSQL(SQL, []);
end;

procedure TSQLiteDatabase.ExecSQL(const SQL:AnsiString; const Bindings:array of const);
var Stmt: TSQLiteStmt;
    NextSQLStatement:PAnsiChar;
    iStepResult:Integer;
begin
 try
  if Sqlite3_Prepare_v2(Self.fDB, PAnsiChar(SQL), -1, Stmt, NextSQLStatement) <> SQLITE_OK then RaiseError('Error executing SQL', SQL);
  if Stmt = nil then RaiseError('Could not prepare SQL statement', SQL);
  DoQuery(SQL);
  SetParams(Stmt);
  BindData(Stmt, Bindings);

  iStepResult:=Sqlite3_step(Stmt);
  if (iStepResult <> SQLITE_DONE) then
   begin
    SQLite3_reset(Stmt);
    RaiseError('Error executing SQL statement', SQL);
   end;
 finally
  if Assigned(Stmt) then Sqlite3_Finalize(Stmt);
 end;
end;

{$WARNINGS OFF}
procedure TSQLiteDatabase.ExecSQL(Query: TSQLiteQuery);
var iStepResult: integer;
begin
 if Assigned(Query.Statement) then
  begin
   iStepResult:=Sqlite3_step(Query.Statement);
   if (iStepResult <> SQLITE_DONE) then
    begin
     SQLite3_reset(Query.Statement);
     RaiseError('Error executing prepared SQL statement', Query.SQL);
    end;
   Sqlite3_Reset(Query.Statement);
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function TSQLiteDatabase.PrepareSQL(const SQL: Ansistring): TSQLiteQuery;
var Stmt:TSQLiteStmt;
    NextSQLStatement:PAnsiChar;
begin
 Result.SQL:=SQL;
 Result.Statement:=nil;

 if Sqlite3_Prepare(self.fDB, PAnsiChar(SQL), -1, Stmt, NextSQLStatement) <> SQLITE_OK
 then RaiseError('Error executing SQL', SQL)
 else Result.Statement:=Stmt;

 if (Result.Statement = nil) then RaiseError('Could not prepare SQL statement', SQL);
 DoQuery(SQL);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSQLiteDatabase.BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: Integer);
begin
 if Assigned(Query.Statement)
 then SQLite3_Bind_Int(Query.Statement, Index, Value)
 else RaiseError('Could not bind integer to prepared SQL statement', Query.SQL);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSQLiteDatabase.BindSQL(Query: TSQLiteQuery; const Index: Integer; const Value: String);
begin
 if Assigned(Query.Statement)
 then SQLite3_Bind_Text(Query.Statement, Index, PAnsiChar(Value), Length(Value), Pointer(SQLITE_STATIC))
 else RaiseError('Could not bind string to prepared SQL statement', Query.SQL);
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSQLiteDatabase.ReleaseSQL(Query: TSQLiteQuery);
begin
 if Assigned(Query.Statement) then
  begin
   SQLite3_Finalize(Query.Statement);
   Query.Statement:=nil;
  end
 else RaiseError('Could not release prepared SQL statement', Query.SQL);
end;
{$WARNINGS ON}

procedure TSQLiteDatabase.UpdateBlob(const SQL: Ansistring; BlobData: TStream);
var iSize: integer;
    ptr: pointer;
    Stmt: TSQLiteStmt;
    Msg: PAnsiChar;
    NextSQLStatement: PAnsiChar;
    iStepResult: integer;
    iBindResult: integer;
begin
 //expects SQL of the form 'UPDATE MYTABLE SET MYFIELD = ? WHERE MYKEY = 1'
 if Pos('?', SQL) = 0 then RaiseError('SQL must include a "?" parameter', SQL);
 Msg:=nil;
 try
  if Sqlite3_Prepare_v2(self.fDB, PAnsiChar(SQL), -1, Stmt, NextSQLStatement) <> SQLITE_OK
  then RaiseError('Could not prepare SQL statement', SQL);

  if Stmt = nil then RaiseError('Could not prepare SQL statement', SQL);
  DoQuery(SQL);

  //now bind the blob data
  iSize:=BlobData.size;
  GetMem(ptr, iSize);
  if ptr = nil then raise ESQLiteBlob.CreateFmt('Error getting memory to save BLOB', [SQL, 'Error']);

  BlobData.Position:=0;
  BlobData.Read(ptr^, iSize);

  iBindResult:=SQLite3_Bind_Blob(Stmt, 1, ptr, iSize, @DisposePointer);
  if iBindResult <> SQLITE_OK then RaiseError('Error binding blob to database', SQL);

  iStepResult:=SQLite3_Step(Stmt);
  if iStepResult <> SQLITE_DONE then
   begin
    SQLite3_reset(Stmt);
    RaiseError('Error executing SQL statement', SQL);
   end;
 finally
  begin
   if Assigned(Stmt) then SQLite3_Finalize(Stmt);
   if Assigned(Msg)  then SQLite3_Free(Msg);
  end;
 end;
end;

//..............................................................................

function TSQLiteDatabase.GetTable(const SQL: Ansistring): TSQLiteTable;
begin
 Result:=TSQLiteTable.Create(Self, SQL);
end;

function TSQLiteDatabase.GetTable(const SQL: Ansistring; const Bindings: array of const): TSQLiteTable;
begin
 Result:=TSQLiteTable.Create(Self, SQL, Bindings);
end;

function TSQLiteDatabase.GetUniTable(const SQL: Ansistring): TSQLiteUniTable;
begin
 Result:=TSQLiteUniTable.Create(Self, SQL);
end;

function TSQLiteDatabase.GetUniTable(const SQL: Ansistring; const Bindings: array of const): TSQLiteUniTable;
begin
 Result:=TSQLiteUniTable.Create(Self, SQL, Bindings);
end;

function TSQLiteDatabase.GetTableValue(const SQL: Ansistring): int64;
begin
 Result:=GetTableValue(SQL, []);
end;

function TSQLiteDatabase.GetTableValue(const SQL: Ansistring; const Bindings: array of const): int64;
var Table:TSQLiteUniTable;
begin
 Result:=-1;
 Table:=Self.GetUniTable(SQL, Bindings);
 try
  if not Table.EOF then Result:=Table.FieldAsInteger(0);
 finally
  Table.Free;
 end;
end;

function TSQLiteDatabase.GetTableString(const SQL: Ansistring): String;
begin
 Result:=GetTableString(SQL, []);
end;

function TSQLiteDatabase.GetTableString(const SQL: Ansistring; const Bindings: array of const): String;
var Table:TSQLiteUniTable;
begin
 Result:='';
 Table:=Self.GetUniTable(SQL, Bindings);
 try
  if not Table.EOF then Result:=Table.FieldAsString(0);
 finally
  Table.Free;
 end;
end;

procedure TSQLiteDatabase.GetTableStrings(const SQL:Ansistring; const Value:TStrings; FieldNum:Integer = 0);
var Table:TSQLiteUniTable;
begin
 Value.Clear;
 Table:=Self.GetUniTable(SQL);
 try
  while not Table.EOF do
   begin
    Value.Add(Table.FieldAsString(FieldNum));
    Table.Next;
   end;
 finally
  Table.Free;
 end;
end;

procedure TSQLiteDatabase.BeginTransaction;
begin
 if not Self.fInTrans then
  begin
   Self.ExecSQL('BEGIN TRANSACTION');
   Self.fInTrans:=True;
  end
 else raise ESQLiteTransaction.Create('Transaction already open');
end;

procedure TSQLiteDatabase.Commit;
begin
 Self.ExecSQL('COMMIT');
 Self.fInTrans:=False;
end;

procedure TSQLiteDatabase.Rollback;
begin
 Self.ExecSQL('ROLLBACK');
 Self.fInTrans:=False;
end;

function TSQLiteDatabase.TableExists(TableName:string):Boolean;
var SQL:string;
    DS:TSQLiteTable;
begin
 //returns true if table exists in the database
 SQL:='select [sql] from sqlite_master where [type] = ''table'' and lower(name) = '''+lowercase(TableName)+''' ';
 DS:=Self.GetTable(SQL);
 try
  Result:=(DS.Count > 0);
 finally
  DS.Free;
 end;
end;

procedure TSQLiteDatabase.SetTimeout(Value: integer);
begin
 SQLite3_BusyTimeout(Self.fDB, Value);
end;

function TSQLiteDatabase.Version: string;
begin
 Result:=SQLite3_Version;
end;

procedure TSQLiteDatabase.AddCustomCollate(name:string; xCompare:TCollateXCompare);
begin
 SQLite3_Create_Collation(fdb, PAnsiChar(name), SQLITE_UTF8, nil, xCompare);
end;

procedure TSQLiteDatabase.AddSystemCollate;
begin
 {$IFDEF WIN32}
 SQLite3_Create_Collation(fdb, 'SYSTEM', SQLITE_UTF16LE, nil, @SystemCollate);
 {$ENDIF}
end;

procedure TSQLiteDatabase.ParamsClear;
var n:Integer;
begin
 for n:=fParams.Count-1 downto 0 do TSQliteParam(fparams[n]).Free;
 fParams.Clear;
end;

procedure TSQLiteDatabase.AddParamInt(Name:string; Value:Int64);
var Param:TSQliteParam;
begin
 Param:=TSQliteParam.Create;
 Param.Name:=Name;
 Param.ValueType:=SQLITE_INTEGER;
 Param.ValueInteger:=Value;
 fParams.Add(Param);
end;

procedure TSQLiteDatabase.AddParamFloat(Name:string; Value:Double);
var Param:TSQliteParam;
begin
 Param:=TSQliteParam.Create;
 Param.Name:=Name;
 Param.ValueType:=SQLITE_FLOAT;
 Param.ValueFloat:=Value;
 fParams.Add(Param);
end;

procedure TSQLiteDatabase.AddParamText(Name:string; Value:string);
var Param:TSQliteParam;
begin
 Param:=TSQliteParam.Create;
 Param.Name:=Name;
 Param.ValueType:=SQLITE_TEXT;
 Param.ValueData:=Value;
 fParams.Add(Param);
end;

procedure TSQLiteDatabase.AddParamNull(Name:string);
var Param:TSQliteParam;
begin
 Param:=TSQliteParam.Create;
 Param.Name:=Name;
 Param.ValueType:=SQLITE_NULL;
 fParams.Add(Param);
end;

procedure TSQLiteDatabase.SetParams(Stmt: TSQLiteStmt);
var n, i:integer;
    Param:TSQliteParam;
begin
 try
  for n:=0 to fParams.Count-1 do
   begin
    Param:=TSQliteParam(fParams[n]);
    i:=SQLite3_Bind_Parameter_index(Stmt, PAnsiChar(AnsiString(Param.Name)));
    if i > 0 then
     begin
      case Param.ValueType of
       SQLITE_INTEGER:SQLite3_Bind_Int64(Stmt, i, Param.ValueInteger);
       SQLITE_FLOAT:  SQLite3_Bind_Double(Stmt, i, Param.ValueFloat);
       SQLITE_TEXT:   SQLite3_Bind_Text(Stmt, i, PAnsiChar(AnsiString(Param.ValueData)), Length(Param.ValueData), SQLITE_TRANSIENT);
       SQLITE_NULL:   SQLite3_Bind_Null(Stmt, i);
      end;
     end;
   end;
 finally
  ParamsClear;
 end;
end;

//database rows that were changed (or inserted or deleted) by the most recent SQL statement
function TSQLiteDatabase.GetRowsChanged:Integer;
begin
 Result:=SQLite3_Changes(Self.fDB);
end;

procedure TSQLiteDatabase.DoQuery(Value:string);
begin
 if Assigned(OnQuery) then OnQuery(Self, Value);
end;

//returns result of SQLITE3_Backup_Step
function TSQLiteDatabase.Backup(TargetDB:TSQLiteDatabase; targetName:AnsiString; sourceName:AnsiString):Integer;
var pBackup:TSQLiteBackup;
begin
 pBackup:=SQLite3_Backup_Init(TargetDB.DB, PAnsiChar(targetName), Self.DB, PAnsiChar(sourceName));
 if pBackup = nil then raise ESQLiteInitializeBackup.Create('Could not initialize backup') else
  begin
   try
    Result:=SQLITE3_Backup_Step(pBackup, -1); //copies entire db
   finally
    SQLite3_Backup_Finish(pBackup);
   end;
  end;
end;

function TSQLiteDatabase.Backup(TargetDB: TSQLiteDatabase):Integer;
begin
 Result:=Self.Backup(TargetDB, 'main', 'main');
end;

//------------------------------------------------------------------------------
// TSQLiteTable
//------------------------------------------------------------------------------

constructor TSQLiteTable.Create(DB:TSQLiteDatabase; const SQL:Ansistring);
begin
 Create(DB, SQL, []);
end;

constructor TSQLiteTable.Create(DB:TSQLiteDatabase; const SQL:Ansistring; const Bindings:array of const);
var Stmt:TSQLiteStmt;
    NextSQLStatement:PAnsiChar;
    iStepResult:Integer;
    ptr:Pointer;
    iNumBytes:Integer;
    thisBlobValue:TMemoryStream;
    thisStringValue:PString;
    thisDoubleValue:PDouble;
    thisIntValue:PInt64;
    thisColType:PInteger;
    i:Integer;
    DeclaredColType:PAnsiChar;
    ActualColType:Integer;
    ptrValue:PAnsiChar;
begin
 inherited create;
 try
  Self.fRowCount:=0;
  Self.fColCount:=0;
  //if there are several SQL statements in SQL, NextSQLStatment points to the
  //beginning of the next one. Prepare only prepares the first SQL statement.
  if SQLite3_Prepare_v2(DB.fDB, PAnsiChar(SQL), -1, Stmt, NextSQLStatement) <> SQLITE_OK  then DB.RaiseError('Error executing SQL', SQL);
  if Stmt = nil then DB.RaiseError('Could not prepare SQL statement', SQL);
  DB.DoQuery(SQL);
  DB.SetParams(Stmt);
  DB.BindData(Stmt, Bindings);

  iStepResult:=SQLite3_Step(Stmt);
  while iStepResult <> SQLITE_DONE do
   begin
    case iStepResult of
     SQLITE_ROW:
      begin
       Inc(fRowCount);
       if fRowCount = 1 then
        begin
         //get data types
         fCols:=TStringList.Create;
         fColTypes:=TList.Create;
         fColCount:=SQLite3_ColumnCount(Stmt);
         for i:=0 to Pred(fColCount) do fCols.Add(AnsiUpperCase(SQLite3_ColumnName(Stmt, i)));
         for i:=0 to Pred(fColCount) do
          begin
           New(thisColType);
           DeclaredColType:=SQLite3_ColumnDeclType(Stmt, i);
           if DeclaredColType = nil then
            thisColType^:=SQLite3_ColumnType(Stmt, i) //use the actual column type instead
           else //seems to be needed for last_insert_rowid
           if (DeclaredColType = 'INTEGER') or (DeclaredColType = 'BOOLEAN') then 
            thisColType^:=dtInt
           else 
           if (DeclaredColType = 'NUMERIC') or (DeclaredColType = 'FLOAT') or (DeclaredColType = 'DOUBLE') or (DeclaredColType = 'REAL') then
            thisColType^:=dtNumeric
           else 
           if DeclaredColType='BLOB' then
            thisColType^:=dtBlob
           else thisColType^:=dtStr;
           fColTypes.Add(thisColType);
          end;
         fResults:=TList.Create;
        end;

          //get column values
       for i:=0 to Pred(ColCount) do
        begin
         ActualColType:=SQLite3_ColumnType(Stmt, i);
         if ActualColType = SQLITE_NULL then
          fResults.Add(nil)
         else 
          if pInteger(fColTypes[i])^ = dtInt then
           begin
            New(thisIntValue);
            thisIntValue^:=SQLite3_ColumnInt64(Stmt, i);
            fResults.Add(thisIntValue);
           end
          else 
           if pInteger(fColTypes[i])^ = dtNumeric then
            begin
             New(thisDoubleValue);
             thisDoubleValue^:=SQLite3_ColumnDouble(Stmt, i);
             fResults.Add(thisDoubleValue);
            end
           else 
            if pInteger(fColTypes[i])^ = dtBlob then
             begin
              iNumBytes:=SQLite3_ColumnBytes(Stmt, i);
              if iNumBytes = 0 then thisBlobValue:=nil else
               begin
                thisBlobValue:=TMemoryStream.Create;
                thisBlobValue.position:=0;
                ptr:=Sqlite3_ColumnBlob(Stmt, i);
                thisBlobValue.WriteBuffer(ptr^, iNumBytes);
               end;
              fResults.Add(thisBlobValue);
             end
            else
             begin
              New(thisStringValue);
              ptrValue:=Sqlite3_ColumnText(Stmt, i);
              SetString(thisStringValue^, ptrValue, Length(ptrValue));
              fResults.Add(thisStringValue);
             end;
        end;
      end;
     SQLITE_BUSY: raise ESQLiteIsBusy.CreateFmt('Could not prepare SQL statement. %s. %s.', [SQL, 'SQLite is Busy']);
    else
     begin
      SQLite3_Reset(Stmt);
      DB.RaiseError('Could not retrieve data', SQL);
     end;
    end;
    iStepResult:=SQLite3_Step(Stmt);
   end;
  fRow:=0;
 finally
  if Assigned(Stmt) then SQLite3_Finalize(Stmt);
 end;
end;

//..............................................................................

destructor TSQLiteTable.Destroy;
var i:Cardinal;
    iColNo:Integer;
begin
 if Assigned(fResults) then
  begin
   for i:=0 to fResults.Count-1 do
    begin
     //check for blob type
     iColNo:=(i mod fColCount);
     case pInteger(Self.fColTypes[iColNo])^ of
      dtBlob: TMemoryStream(fResults[i]).Free;
      dtStr:
       if fResults[i] <> nil then
        begin
         SetString(string(fResults[i]^), nil, 0);
         Dispose(fResults[i]);
        end;
     else
      Dispose(fResults[i]);
     end;
    end;
   fResults.Free;
  end;
 if Assigned(fCols) then fCols.Free;
 if Assigned(fColTypes) then for i:=0 to fColTypes.Count-1 do Dispose(fColTypes[i]);
 fColTypes.Free;
 inherited;
end;

//..............................................................................

function TSQLiteTable.GetColumns(I:Integer):string;
begin
 Result:=fCols[I];
end;

//..............................................................................

function TSQLiteTable.GetCountResult:Integer;
begin
 if not EOF then Result:=StrToInt(Fields[0]) else Result:=0;
end;

function TSQLiteTable.GetCount:Integer;
begin
 Result:=FRowCount;
end;

//..............................................................................

function TSQLiteTable.GetEOF:Boolean;
begin
 Result:=fRow >= fRowCount;
end;

function TSQLiteTable.GetBOF:Boolean;
begin
 Result:=fRow <= 0;
end;

//..............................................................................

function TSQLiteTable.GetFieldByName(FieldName:string):string;
begin
 Result:=GetFields(Self.GetFieldIndex(FieldName));
end;

function TSQLiteTable.GetFieldIndex(FieldName:string):Integer;
begin
 if fCols = nil then
  begin
   raise ESQLiteFieldNotFound.Create('Field '+FieldName+' Not found. Empty dataset');
   Exit;
  end;

 if fCols.count = 0 then
  begin
   raise ESQLiteFieldNotFound.Create('Field '+FieldName+' Not found. Empty dataset');
   Exit;
  end;

 Result:=fCols.IndexOf(AnsiUpperCase(FieldName));
 if Result < 0 then
  begin
   raise ESQLiteFieldNotFound.Create('Field not found in dataset: '+FieldName)
  end;
end;

//..............................................................................

function TSQLiteTable.GetFields(i:Cardinal):string;
var thisvalue:PString;
    thistype:Integer;
begin
 Result:='';
 if EOF then raise ESQLiteTableEOF.Create('Table is at End of File');
  //integer types are not stored in the resultset
  //as strings, so they should be retrieved using the type-specific
  //methods
 thistype:=PInteger(Self.fColTypes[i])^;
 case thistype of
  dtStr:
   begin
    thisvalue:=Self.fResults[(Self.frow * Self.fColCount) + i];
    if thisvalue <> nil then 
             Result:=thisvalue^ 
    else     Result:='';
   end;
  dtInt:     Result:=IntToStr(Self.FieldAsInteger(i));
  dtNumeric: Result:=FloatToStr(Self.FieldAsDouble(i));
  dtBlob:    Result:=Self.FieldAsBlobText(i);
 end;
end;

function TSQLiteTable.FieldAsBlob(i:Cardinal):TMemoryStream;
begin
 if EOF then raise ESQLiteTableEOF.Create('Table is at End of File');
 if Self.fResults[(Self.fRow * Self.fColCount)+i] = nil then 
  Result:=nil
 else 
  if pInteger(Self.fColTypes[i])^ = dtBlob then 
   Result:=TMemoryStream(Self.fResults[(Self.fRow*Self.fColCount)+i])
  else raise ESQLiteBlob.Create('Not a BLOB field');
end;

function TSQLiteTable.FieldAsBlobText(i:Cardinal):string;
var MemStream:TMemoryStream;
    {$IFDEF UNICODE}
     Buffer:PAnsiChar;
    {$ELSE}
     Buffer:PWideChar;
    {$ENDIF}
begin
 Result:='';
 MemStream:=Self.FieldAsBlob(i);
 if MemStream <> nil then
  if MemStream.Size > 0 then
  begin
   MemStream.Position:=0;
   {$IFDEF UNICODE}
   Buffer:=AnsiStrAlloc(MemStream.Size + 1);
   {$ELSE}
   Buffer:=StrAlloc(MemStream.Size + 1);
   {$ENDIF}
   MemStream.readbuffer(Buffer[0], MemStream.Size);
   (Buffer+MemStream.Size)^:=Chr(0);
   SetString(Result, Buffer, MemStream.size);
   StrDispose(Buffer);
  end; //do not free the TMemoryStream here; it is freed when //TSqliteTable is destroyed
end;
   
function TSQLiteTable.FieldAsBoolean(i: Cardinal): Boolean;
begin
 if EOF then raise ESQLiteTableEOF.Create('Table is at End of File');
 if Self.fResults[(Self.fRow * Self.fColCount)+i] = nil then
  Result:=False
 else
  if pInteger(Self.fColTypes[i])^ = dtInt then
   Result:=Boolean(pInt64(Self.fResults[(Self.fRow * Self.fColCount)+i])^)
  else
   if pInteger(Self.fColTypes[i])^ = dtNumeric then
    Result:=Boolean(Trunc(strtofloat(pString(Self.fResults[(Self.fRow * Self.fColCount)+i])^)))
   else
    raise ESQLiteTypeError.Create('Not an Integer or Numeric field');
end;

function TSqliteTable.FieldAsInteger(i:Cardinal):Int64;
begin
 if EOF then raise ESQLiteTableEOF.Create('Table is at End of File');
 if Self.fResults[(Self.fRow * Self.fColCount)+i] = nil then
  Result:=0
 else
  if pInteger(Self.fColTypes[i])^ = dtInt then
   Result:=pInt64(Self.fResults[(Self.fRow * Self.fColCount)+i])^
  else
   if pInteger(Self.fColTypes[i])^ = dtNumeric then
    Result:=Trunc(strtofloat(pString(Self.fResults[(Self.fRow * Self.fColCount)+i])^))
   else
    raise ESQLiteTypeError.Create('Not an Integer or Numeric field');
end;

function TSQLiteTable.FieldAsDateTime(i: Cardinal): TDateTime;
begin
 Result:=FieldAsDouble(i);
end;

function TSQLiteTable.FieldAsDouble(i:Cardinal):Double;
begin
 if EOF then raise ESQLiteTableEOF.Create('Table is at End of File');
 if Self.fResults[(Self.fRow * Self.fColCount)+i] = nil then
  Result:=0
 else 
  if pInteger(Self.fColTypes[i])^ = dtInt then
   Result:=pInt64(Self.fResults[(Self.fRow * Self.fColCount)+i])^
  else 
   if pInteger(Self.fColTypes[i])^ = dtNumeric then
    Result:=pDouble(Self.fResults[(Self.fRow * Self.fColCount)+i])^
   else 
    raise ESQLiteTypeError.Create('Not an Integer or Numeric field');
end;

function TSQLiteTable.FieldAsString(i:Cardinal):string;
begin
 if EOF then raise ESQLiteTableEOF.Create('Table is at End of File');
 if Self.fResults[(Self.fRow * Self.fColCount)+i] = nil then 
  Result:=''
 else 
  Result:=Self.GetFields(i);
end;

function TSQLiteTable.FieldIsNull(i:Cardinal):Boolean;
var Value:Pointer;
begin
 if EOF then raise ESQLiteTableEOF.Create('Table is at End of File');
 Value:=Self.fResults[(Self.fRow * Self.fColCount)+i];
 Result:=Value = nil;
end;

//..............................................................................

function TSQLiteTable.Next:Boolean;
begin
 Result:=False;
 if not EOF then
  begin
   Inc(fRow);
   Result:=True;
  end;
end;

function TSQLiteTable.Previous:Boolean;
begin
 Result:=False;
 if not BOF then
  begin
   Dec(fRow);
   Result:=True;
  end;
end;

function TSQLiteTable.MoveFirst:Boolean;
begin
 Result:=False;
 if self.fRowCount > 0 then
  begin
   fRow:=0;
   Result:=True;
  end;
end;

function TSQLiteTable.MoveLast:Boolean;
begin
 Result:=False;
 if self.fRowCount > 0 then
  begin
   fRow:=fRowCount-1;
   Result:=True;
  end;
end;

{$WARNINGS OFF}
function TSQLiteTable.MoveTo(Position:Cardinal):Boolean;
begin
 Result:=False;
 if (Self.fRowCount > 0) and (Self.fRowCount > Position) then
  begin
   fRow:=Position;
   Result:=True;
  end;
end;
{$WARNINGS ON}
      
{ TSQLiteUniTable }

constructor TSQLiteUniTable.Create(DB:TSQLiteDatabase; const SQL:Ansistring);
begin
 Create(DB, SQL, []);
end;

constructor TSQLiteUniTable.Create(DB:TSQLiteDatabase; const SQL:Ansistring; const Bindings:array of const);
var NextSQLStatement:PAnsiChar;
    i:Integer;
begin
 inherited Create;
 Self.fDB:=DB;
 Self.fEOF:=False;
 Self.fRow:=0;
 Self.fColCount:=0;
 Self.fSQL:=SQL;
 if SQLite3_Prepare_v2(DB.fDB, PAnsiChar(SQL), -1, fStmt, NextSQLStatement) <> SQLITE_OK then DB.RaiseError('Error executing SQL', SQL);
 if fStmt = nil then DB.RaiseError('Could not prepare SQL statement', SQL);
 DB.DoQuery(SQL);
 DB.SetParams(fStmt);
 DB.BindData(fStmt, Bindings);

  //get data types
 fCols:=TStringList.Create;
 fColCount:=SQLite3_ColumnCount(fstmt);
 for i:=0 to Pred(fColCount) do fCols.Add(AnsiUpperCase(SQLite3_ColumnName(fstmt, i)));
 Next;
end;

destructor TSQLiteUniTable.Destroy;
begin
 if Assigned(fStmt) then SQLite3_Finalize(fstmt);
 if Assigned(fCols) then fCols.Free;
 inherited;
end;

function TSQLiteUniTable.FieldAsBlob(i:Cardinal):TMemoryStream;
var iNumBytes:Integer;
    ptr:Pointer;
begin
 Result:=TMemoryStream.Create;
 iNumBytes:=SQLite3_ColumnBytes(fstmt, i);
 if iNumBytes > 0 then
  begin
   ptr:=SQLite3_ColumnBlob(fstmt, i);
   Result.WriteBuffer(ptr^, iNumBytes);
   Result.Position:=0;
  end;
end;

function TSQLiteUniTable.FieldAsBlobPtr(i:Cardinal; out iNumBytes:Integer):Pointer;
begin
 iNumBytes:=SQLite3_ColumnBytes(fstmt, i);
 Result:=SQLite3_ColumnBlob(fstmt, i);
end;

function TSQLiteUniTable.FieldAsBlobText(i:Cardinal):string;
var MemStream:TMemoryStream;
    {$IFDEF UNICODE}
     Buffer:PAnsiChar;
    {$ELSE}
     Buffer:PWideChar;
    {$ENDIF}
begin
 Result:='';
 MemStream:=Self.FieldAsBlob(i);
 if MemStream <> nil then
  try
   if MemStream.Size > 0 then
    begin
     MemStream.Position:=0;
     {$IFDEF UNICODE}
     Buffer:=AnsiStrAlloc(MemStream.Size + 1);
     {$ELSE}
     Buffer:=StrAlloc(MemStream.Size + 1);
     {$ENDIF}
     MemStream.ReadBuffer(Buffer[0], MemStream.Size);
     (Buffer+MemStream.Size)^:=Chr(0);
     SetString(Result, Buffer, MemStream.size);
     StrDispose(Buffer);
    end;
  finally
   MemStream.Free;
  end
end;

function TSQLiteUniTable.FieldAsDouble(i:Cardinal):Double;
begin
 Result:=SQLite3_ColumnDouble(fstmt, i);
end;

function TSQLiteUniTable.FieldAsInteger(i:Cardinal):Int64;
begin
 Result:=SQLite3_ColumnInt64(fstmt, i);
end;

function TSQLiteUniTable.FieldAsString(i:Cardinal):string;
begin
 Result:=Self.GetFields(i);
end;

function TSQLiteUniTable.FieldIsNull(i:Cardinal):Boolean;
begin
 Result:=SQLite3_ColumnText(fstmt, i) = nil;
end;

function TSQLiteUniTable.GetColumns(i:Integer):string;
begin
 Result:=fCols[i];
end;

function TSQLiteUniTable.GetFieldByName(FieldName:string):string;
begin
 Result:=GetFields(Self.GetFieldIndex(FieldName));
end;

function TSQLiteUniTable.GetFieldIndex(FieldName:string):Integer;
begin
 if fCols = nil then
  begin
   raise ESQLiteFieldNotFound.Create('Field '+FieldName+' Not found. Empty dataset');
   Exit;
  end;
 if fCols.count = 0 then
  begin
   raise ESQLiteFieldNotFound.Create('Field '+FieldName+' Not found. Empty dataset');
   Exit;
  end;
 Result:=fCols.IndexOf(AnsiUpperCase(FieldName));
 if Result < 0 then
  begin
   raise ESQLiteFieldNotFound.Create('Field not found in dataset: '+FieldName)
  end;
end;

function TSQLiteUniTable.GetFields(i:Cardinal):string;
begin
 Result:=SQLite3_ColumnText(fstmt, i);
end;

function TSQLiteUniTable.Next:Boolean;
var iStepResult:Integer;
begin
 fEOF:=True;
 iStepResult:=SQLite3_Step(fStmt);
 case iStepResult of
  SQLITE_ROW:
   begin
    fEOF:=False;
    Inc(fRow);
   end;
  SQLITE_DONE:
      // we are on the end of dataset
      // return EOF=true only
   ;
 else
  begin
   SQLite3_Reset(fStmt);
   fDB.RaiseError('Could not retrieve data', fSQL);
  end;
 end;
 Result:=not fEOF;
end;

end.

