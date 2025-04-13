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
type
  TForm1 = class(TForm)
    EditUsername: TEdit;
    Label1: TLabel;
    ListBoxRepo: TListBox;
    LabelRepo: TLabel;
    ListBoxFiles: TListBox;
    LabelFiles: TLabel;
    ButtonGetRepo: TButton;
    LabelContet: TLabel;
    RichEditFileContent: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    const
        GitHubAPIUrl = 'https://api.github.com/';
type
 TAppSettings = record
   WindowLeft: Integer;
   WindowTop: Integer;
   WindowWidth: Integer;
   WindowHeight: Integer;
   Username: string;
end;
    procedure ButtonGetRepoClick(Sender: TObject);
    procedure ListBoxRepoClick(Sender: TObject);
    procedure ListBoxFilesClick(Sender: TObject);
    procedure ListBoxRepoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
   procedure GetGitHubRepos(const UserName: string);
   procedure GetFilesForUserInRepo(const UserName: string; const RepoName: string);
   procedure GetReadmeContent(const UserName: string;const Filename: string);
   procedure SaveSettings(const Settings: TAppSettings);
   procedure LoadSettings(var Settings: TAppSettings);
   function GetSelectedItem(ListBox: TListBox): string;
   function ExecuteGitHubAPIRequest(const Resource: string;
                               var ResponseContent: string): Integer;
   procedure LogError(const Message: string);
   function IsStringEmpty(const S: string): Boolean;


  public
  end;

var
  Form1: TForm1;
  RepoName: string;

implementation

{$R *.dfm}

procedure TForm1.ButtonGetRepoClick(Sender: TObject);
begin
  try
   GetGitHubRepos(TrimRight(EditUsername.Text));
   except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;
procedure TForm1.GetGitHubRepos(const UserName: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONValue: TJSONValue;
  RepoArray: TJSONArray;
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
    ShowMessage('Nothing is selected!');
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
    ShowMessage('You clicked on an empty list. Click the "Get Repository Names" button.');
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
   ShowMessage('You clicked on an empty list. Click the "Get Repository Names" button.');
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
  SetBounds(AppSettings.WindowLeft, AppSettings.WindowTop, AppSettings.WindowWidth, AppSettings.WindowHeight);
end;
//---2-----------------------------------
function TForm1.ExecuteGitHubAPIRequest(const Resource:
                           string; var ResponseContent: string): Integer;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create(GitHubAPIUrl); // Используем константу GitHubAPIUrl
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
    ListBoxFiles.AddItem('Error receiving files: '
      + IntToStr(StatusCode)
      + ' - ' + ResponseContent, nil);
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
  StatusCode: Integer;
  JSONValue: TJSONValue;
  Base64Content: string;
  DecodedContent: string;
begin
  StatusCode := ExecuteGitHubAPIRequest(Format('repos/%s/%s/contents/%s', [UserName, RepoName, FileName]), ResponseContent);

  if StatusCode = 200 then
  begin
    JSONValue := TJSONObject.ParseJSONValue(ResponseContent);
    try
      if Assigned(JSONValue) then
      begin
        Base64Content := JSONValue.GetValue<string>('content');
//        if notIsNullOrEmpty(Base64Content) then
//         if not TStringHelper.IsNullOrEmpty(Base64Content) then // Используем TStringHelper.IsNullOrEmpty
        if not IsStringEmpty(Base64Content) then // Используем IsStringEmpty
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
       IniFile.WriteInteger('Window', 'Left', Settings.WindowLeft);
       IniFile.WriteInteger('Window', 'Top', Settings.WindowTop);
       IniFile.WriteInteger('Window', 'Width', Settings.WindowWidth);
       IniFile.WriteInteger('Window', 'Height', Settings.WindowHeight);
       IniFile.WriteString( 'User',   'Username', TrimRight(EditUsername.Text));
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
       IniFile := TIniFile.Create('AppSettings.ini');
   if not FileExists(FilePath) then
    begin
     ShowMessage('Settings file not found. Using default settings.');
   end;
      IniFile := TIniFile.Create(FilePath);
     try
       Settings.WindowLeft :=  IniFile.ReadInteger('Window', 'Left', 100);
       Settings.WindowTop :=   IniFile.ReadInteger('Window', 'Top', 100);
       Settings.WindowWidth:=  IniFile.ReadInteger('Window', 'Width', 638);
       Settings.WindowHeight :=IniFile.ReadInteger('Window', 'Height', 479);
       EditUsername.Text :=    IniFile.ReadString( 'User',   'Username', 'maxth7');
     finally
       IniFile.Free;
     end;
   end;
end.
