unit Unit1;
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, System.JSON, REST.Types,
  System.NetEncoding, Vcl.ComCtrls,
  System.IniFiles,
  System.IOUtils;

const
  INI_SECTION_WINDOW = 'Window';
  INI_KEY_LEFT       = 'Left';
  INI_KEY_TOP        = 'Top';
  INI_KEY_WIDTH      = 'Width';
  INI_KEY_HEIGHT     = 'Height';

  INI_SECTION_USER = 'User';
  INI_KEY_USERNAME = 'Username';
  DEFAULT_USERNAME = 'maxth7';

  DEFAULT_WINDOW_LEFT = 100;
  DEFAULT_WINDOW_TOP  = 100;
  DEFAULT_WINDOW_WIDTH  = 638;
  DEFAULT_WINDOW_HEIGHT = 479;

  MSG_SETTINGS_FILE_NOT_FOUND = 'Settings file not found. Using default settings.';
  MSG_FILE_IS_NOT_SELECTED    = 'The file is not selected.';
  MSG_FILE_LIST_IS_EMPTY      = 'The file list is empty.';
  MSG_NOTHING_IS_SELECTED     =  'Nothing is selected!';

  MSG_FILE_DEDUG_LOG='debug.log';
   const
        GitHubAPIUrl = 'https://api.github.com/';
type
  TForm1 = class(TForm)
    EditUsername: TEdit;
    Label1:       TLabel;
    LabelRepo:    TLabel;
    LabelFiles:   TLabel;
    LabelContet:  TLabel;

    ListBoxRepo:  TListBox;
    ListBoxFiles: TListBox;

    ButtonGetRepo: TButton;

    RichEditFileContent: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    type
       TAppSettings = record
       WindowLeft:  Integer;
       WindowTop:   Integer;
       WindowWidth: Integer;
       WindowHeight:Integer;
       Username:    string;
    end ;

    procedure ButtonGetRepoClick(Sender: TObject);
    procedure ListBoxRepoClick(Sender: TObject);
    procedure ListBoxFilesClick(Sender: TObject);
    procedure ListBoxRepoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

  private
   procedure GetGitHubRepos(const UserName: string);
   procedure GetFilesForUserInRepo(const UserName: string; const RepoName: string);
   procedure GetReadmeContent(const UserName: string;const Filename: string);

   function GetSelectedItem(ListBox: TListBox): string;
   function ExecuteGitHubAPIRequest(const Resource: string;
                               var ResponseContent: string): Integer;
   procedure LogError(const Message: string);
   function IsStringEmpty(const S: string): Boolean;

   procedure SaveSettings(const Settings: TAppSettings);
   procedure LoadSettings(var Settings: TAppSettings);

  public
  end;

var
  Form1: TForm1;
  RepoName: string;

implementation

{$R *.dfm}

procedure TForm1.ButtonGetRepoClick(Sender: TObject);
begin
ButtonGetRepo.Enabled:=False;
 LogError('Start ButtonGetRepoClick ');
  try
   GetGitHubRepos(TrimRight(EditUsername.Text));
   except
    on E: Exception do
      begin
      LogError('Error in ButtonGetRepoClick: ' + E.Message);
      ShowMessage('Error: ' + E.Message);
    end;
  end;
  ButtonGetRepo.Enabled:=true;
end;
procedure TForm1.GetGitHubRepos(const UserName: string);
var
  RESTClient:   TRESTClient;
  RESTRequest:  TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONValue:    TJSONValue;
  RepoArray:    TJSONArray;
  i: Integer;
begin

  RESTClient := TRESTClient.Create(GitHubAPIUrl
   +'users/'
   + UserName + '/repos');
  RESTRequest := TRESTRequest.Create(RESTClient);
  RESTResponse := TRESTResponse.Create(RESTRequest);
  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmGET;
    RESTRequest.Execute;
  if RESTResponse.StatusCode = 200 then
  begin
    JSONValue := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        if Assigned(JSONValue) then
        begin
          RepoArray := JSONValue as TJSONArray;
          ListBoxRepo.Clear;
          for i := 0 to RepoArray.Count - 1 do
          begin
          ListBoxRepo.AddItem(RepoArray.Items[i].GetValue<string>('name'),nil );
          end;
        end;
      finally
        JSONValue.Free;
      end;
    end
    else
    begin
      ListBoxRepo.AddItem('Error: ' + IntToStr(RESTResponse.StatusCode)
                           + ' - '  + RESTResponse.StatusText,nil);
    end;
    finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
    end;
end;

 function TForm1.GetSelectedItem(ListBox: TListBox): string;
var
  SelectedIndex: Integer;
begin
  Result := '';
  SelectedIndex := ListBox.ItemIndex;
  if SelectedIndex <> -1 then
  begin
    Result := ListBox.Items[SelectedIndex];
  end
  else
  begin
    ShowMessage(MSG_NOTHING_IS_SELECTED );
  end;
end;
//---1-----------------------------------
procedure TForm1.ListBoxFilesClick(Sender: TObject);
var
  SelectedItem: string;
begin
  SelectedItem := GetSelectedItem(ListBoxFiles);
  if SelectedItem <> '' then
  begin
    RichEditFileContent.Clear;
    GetReadmeContent(TrimRight(EditUsername.Text), SelectedItem);
  end;
end;

procedure TForm1.ListBoxRepoClick(Sender: TObject);
var
  SelectedItem: string;
begin
  if ListBoxRepo.Items.Count = 0 then
  begin
   ShowMessage(MSG_FILE_LIST_IS_EMPTY);
   Exit;
  end;

  SelectedItem := GetSelectedItem(ListBoxRepo);
  if SelectedItem <> '' then
  begin
    RepoName := TrimRight(SelectedItem);
    GetFilesForUserInRepo(TrimRight(EditUsername.Text), SelectedItem);
  end;
end;

  procedure TForm1.ListBoxRepoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
if ListBoxRepo.count=0 then
   ShowMessage(MSG_FILE_LIST_IS_EMPTY);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  AppSettings: TAppSettings;
begin
  AppSettings.WindowLeft := Left;
  AppSettings.WindowTop := Top;
  AppSettings.WindowWidth := Width;
  AppSettings.WindowHeight := Height;
  SaveSettings(AppSettings);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AppSettings: TAppSettings;
begin
  LoadSettings(AppSettings);
  SetBounds(AppSettings.WindowLeft, AppSettings.WindowTop,
           AppSettings.WindowWidth, AppSettings.WindowHeight);
end;
//---2-----------------------------------
function TForm1.ExecuteGitHubAPIRequest(const Resource: string; var ResponseContent: string): Integer;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create(GitHubAPIUrl);
  RESTRequest := TRESTRequest.Create(RESTClient);
  RESTResponse := TRESTResponse.Create(RESTRequest);
  try
    RESTRequest.Resource := Resource;
    RESTRequest.Method := rmGET;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Execute;

    ResponseContent := RESTResponse.Content;
    Result := RESTResponse.StatusCode;
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

procedure TForm1.GetFilesForUserInRepo(const UserName: string; const RepoName: string);
var
  ResponseContent: string;
  StatusCode: Integer;
  JSONValue: TJSONValue;
  FileList: TJSONArray;
  I: Integer;
begin
  StatusCode := ExecuteGitHubAPIRequest(Format('repos/%s/%s/contents', [UserName, RepoName]), ResponseContent);

  if StatusCode = 200 then
  begin
    JSONValue := TJSONObject.ParseJSONValue(ResponseContent);
    try
      if Assigned(JSONValue) and (JSONValue is TJSONArray) then
      begin
        FileList := TJSONArray(JSONValue);
        ListBoxFiles.Clear;
        for I := 0 to FileList.Count - 1 do
        begin
          ListBoxFiles.AddItem(FileList.Items[I].GetValue<string>('name'), nil);
        end;
      end;
    finally
      JSONValue.Free;
    end;
  end
  else
  begin
    LogError('Error in GetFilesForUserInRepo: ' + IntToStr(StatusCode)
      + ' - ' + ResponseContent);
    ListBoxFiles.AddItem('Error receiving files: '
      + IntToStr(StatusCode)
      + ' - ' + ResponseContent, nil);
  end;
end;
//
procedure OutputDebugString(const S: string);
var
  LogFile: TextFile;
begin
  // Вывод в файл журнала
  AssignFile(LogFile,MSG_FILE_DEDUG_LOG );
  if FileExists(MSG_FILE_DEDUG_LOG) then
    Append(LogFile)
  else
    Rewrite(LogFile);
  try
    WriteLn(LogFile, DateTimeToStr(Now) + ': ' + S);
  finally
    CloseFile(LogFile);
  end;

end;

procedure TForm1.LogError(const Message: string);
begin
  OutputDebugString(PChar(Message));
end;

function TForm1.IsStringEmpty(const S: string): Boolean;
begin
  Result := (S = '') or (Length(S) = 0);
end;

procedure TForm1.GetReadmeContent(const UserName: string; const FileName: string);
var
  ResponseContent: string;
  StatusCode:      Integer;
  JSONValue:       TJSONValue;
  Base64Content:   string;
  DecodedContent:  string;
begin
  StatusCode := ExecuteGitHubAPIRequest(Format('repos/%s/%s/contents/%s', [UserName, RepoName, FileName]), ResponseContent);

  if StatusCode = 200 then
  begin
    JSONValue := TJSONObject.ParseJSONValue(ResponseContent);
    try
      if Assigned(JSONValue) then
      begin
        Base64Content := JSONValue.GetValue<string>('content');
        if not IsStringEmpty(Base64Content) then
        begin
          try
            DecodedContent := TNetEncoding.Base64.Decode(Base64Content);
            LabelContet.Caption := 'File Contents: ' + FileName;
            RichEditFileContent.Lines.Add(DecodedContent);
          except
            on E: Exception do
            begin
              LogError('Error decoding Base64 content: ' + E.Message);
              RichEditFileContent.Lines.Add('Error decoding Base64 content: ' + E.Message);
            end;
          end;
        end
        else
        begin
          RichEditFileContent.Lines.Add('File content is empty.');
        end;
      end;
    finally
      JSONValue.Free;
    end;
  end
  else
  begin
    LogError('Error in GetReadmeContent: ' + IntToStr(StatusCode)
      + ' - ' + ResponseContent);
    RichEditFileContent.Lines.Add('Error when receiving content: '
      + FileName
      + IntToStr(StatusCode)
      + ' - ' + ResponseContent);
  end;
end;
// 3-----------------------------
procedure TForm1.SaveSettings(const Settings: TAppSettings);
   var
     IniFile: TIniFile;
     FilePath: string;
   begin
     FilePath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'AppSettings.ini');
     IniFile := TIniFile.Create(FilePath);
     try
       IniFile.WriteInteger(INI_SECTION_WINDOW, INI_KEY_LEFT, Settings.WindowLeft);
       IniFile.WriteInteger(INI_SECTION_WINDOW, INI_KEY_TOP, Settings.WindowTop);
       IniFile.WriteInteger(INI_SECTION_WINDOW, INI_KEY_WIDTH, Settings.WindowWidth);
       IniFile.WriteInteger(INI_SECTION_WINDOW, INI_KEY_HEIGHT, Settings.WindowHeight);
       IniFile.WriteString(INI_SECTION_USER, INI_KEY_USERNAME, TrimRight(EditUsername.Text));
     except
       on E: Exception do
         LogError('Error saving settings: ' + E.Message);
         end;

       IniFile.Free;

   end;

procedure TForm1.LoadSettings(var Settings: TAppSettings);
   var
     IniFile: TIniFile;
     FilePath: string;
   begin
     FilePath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'AppSettings.ini');
     Settings.WindowLeft := DEFAULT_WINDOW_LEFT;
     Settings.WindowTop := DEFAULT_WINDOW_TOP;
     Settings.WindowWidth := DEFAULT_WINDOW_WIDTH;
     Settings.WindowHeight := DEFAULT_WINDOW_HEIGHT;
     EditUsername.Text := DEFAULT_USERNAME;

     if not FileExists(FilePath) then
     begin
       ShowMessage(MSG_SETTINGS_FILE_NOT_FOUND);
       Exit;
     end;

     IniFile := TIniFile.Create(FilePath);
     try
       Settings.WindowLeft :=  IniFile.ReadInteger(INI_SECTION_WINDOW, INI_KEY_LEFT, DEFAULT_WINDOW_LEFT);
       Settings.WindowTop :=   IniFile.ReadInteger(INI_SECTION_WINDOW, INI_KEY_TOP, DEFAULT_WINDOW_TOP);
       Settings.WindowWidth:=  IniFile.ReadInteger(INI_SECTION_WINDOW, INI_KEY_WIDTH, DEFAULT_WINDOW_WIDTH);
       Settings.WindowHeight :=IniFile.ReadInteger(INI_SECTION_WINDOW, INI_KEY_HEIGHT, DEFAULT_WINDOW_HEIGHT);
       EditUsername.Text :=    IniFile.ReadString(INI_SECTION_USER, INI_KEY_USERNAME, DEFAULT_USERNAME);
     except
       on E: Exception do
         LogError('Error loading settings: ' + E.Message);
         end;

       IniFile.Free;

   end;

end.
