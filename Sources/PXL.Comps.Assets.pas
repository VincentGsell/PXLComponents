unit PXL.Comps.Assets;
{ -----------------------------------------------------------------------------
    This program is free software: Under statement of join file README - LGPL.txt
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-----------------------------------------------------------------------------
 Unit Name : PXL.Comps.Assets
 Author    : Vincent Gsell
 Url       : http://grids.systems
 Purpose   : Keep binary data in a repository.
 Date:     : 20180105
 History   :
 20180105 - Retaken from OO lib.
 20110330 - Create.
 20110412 - Fix SetFile call on loading witch reloaded each time file.
-----------------------------------------------------------------------------}

interface

uses sysutils,
     classes,
     PXL.Comps.Core;

Type

TPXLAssets = class;

TPXLAssetUnitType = ( Unknown,
                      ImageBMP,
                      ImageJPG,
                      ImagePNG,
                      Sound,
                      Object3D
                    );

TPXLAssetUnitTypeHelper = Record Helper for TPXLAssetUnitType
  Function IsImageType : Boolean;
  Function ExtToType(const aExt : String) : TPXLAssetUnitType;
  Function TypeToExt : String;
End;

TPXLAssetUnit = Class(TCollectionItem)
private
Protected
  FFile: String;
  FStream: TMemoryStream;
  FUsageName: String;
  FAssetType: TPXLAssetUnitType;
  function GetStreamSize: Int64;
  procedure SetFile(const Value: String);
  procedure SetStream(const Value: TMemoryStream);
  procedure SetUsageName(const Value: String);
  function GetDisplayName: string; Override;
  function GetAssetExtention: String;
Public
  Constructor Create(Collection: TCollection); Override;

  procedure WriteStr (Stream: TStream);
  procedure ReadStr (Stream: TStream);
  procedure readAssetType(Reader: TReader);
  procedure writeAssetType(Writer: TWriter);

  procedure DefineProperties (Filer: TFiler); OVerride;

  Property AvailableStream : TMemoryStream read FStream Write SetStream;
Published
  Property StreamSize : Int64 read GetStreamSize;
  Property FileToLoad : String read FFile Write SetFile;
  Property UsageName : String read FUsageName Write SetUsageName;
  Property AssetType : TPXLAssetUnitType read FAssetType;
  Property AssetExtention : String read GetAssetExtention;
End;

TPXLAssetCollection = class (TCollection)
private
  FComp: TPXLAssets;
public
  constructor Create (CollOwner: TComponent);
  function GetOwner: TPersistent; override;

  Function IsUsageNameOk(aIndex : Integer; aUSageName : string) : Boolean;
end;

TPXLAssets = Class(TPXLComponent)
Private
Protected
  FAsset : TPXLAssetCollection;
  function GetAssetsCount: Integer;
Public
  Constructor Create(aowner : TComponent); OVerride;
  Destructor Destroy; Override;

  Function GetImageString : String;
  Function LoadAsset(aFileName : string) : String;
  Function TotalMemory : Int64;
  Function TotalMemoryAsString : String;

  function GetAssetFromName(Const aAssetName : String; Out aAsset : TPXLAssetUnit) : boolean;

Published
  Property Assets : TPXLAssetCollection read FAsset write FAsset;
  Property AssetsCount : Integer read GetAssetsCount;
End;

Function ConvertMemoryAmountAsString(amount : Int64) : String;

Procedure Register;

implementation

Procedure Register;
begin
  RegisterComponents('PXL',[TPXLAssets]);
end;



  function ConvertMemoryAmountAsString(amount : Int64) : String;
  var a : Int64;
      b : string;
      f : integer;
  begin
    a := amount;
    b := ' bytes';
    Result := intToStr(a)+b;
    f := 1;

    if a > 1024 then //kb
    begin
      f := 1024;
      b := ' KBytes';
      if a > 1024 * 1024 then //Mb
      begin
        f := 1024 * 1024;
        b := ' MBytes';
      end;
      Result := Format('%8.2f %s',[a/f,b]);
    end;
  end;


{ TPXLAssetCollection }

constructor TPXLAssetCollection.Create(CollOwner: TComponent);
begin
  Assert(CollOwner is TPXLAssets);
  inherited Create(TPXLAssetUnit);
  FComp := TPXLAssets(CollOwner);
end;

function TPXLAssetCollection.GetOwner: TPersistent;
begin
  Result := FComp;
end;

function TPXLAssetCollection.IsUsageNameOk(aIndex : integer; aUsageName : String) : Boolean;
var i : integer;
    c1 : String;
begin
  result := true;
  c1 := UpperCase(aUSageName);
  for I := 0 to Count - 1 do
  begin
    if (Uppercase(TPXLAssetUnit(Items[i]).UsageName) = c1) And (aIndex <> i) then
      Result := False;
  end;
end;

{ TPXLAssetUnit }

constructor TPXLAssetUnit.Create(Collection: TCollection);
begin
  inherited;
  FStream := TMemoryStream.Create;
end;

procedure TPXLAssetUnit.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('AvailableStream',readStr,WriteStr,True);
  Filer.DefineProperty('DummyAssetType',readAssetType,writeAssetType,True);
end;

function TPXLAssetUnit.GetAssetExtention: String;
begin
  Result := FAssetType.TypeToExt;
end;

function TPXLAssetUnit.GetDisplayName: string;
begin
  Result := 'Asset "' + FUsageName + '" - ['+ConvertMemoryAmountAsString(StreamSize)+'] - ('+AssetExtention+')';
end;

function TPXLAssetUnit.GetStreamSize: Int64;
begin
  result := FStream.Size;
end;

procedure TPXLAssetUnit.readAssetType(Reader: TReader);
begin
  FAssetType := TPXLAssetUnitType(Reader.ReadInteger);
end;

procedure TPXLAssetUnit.ReadStr(Stream: TStream);
begin
  FStream.LoadFromStream(Stream);
end;

procedure TPXLAssetUnit.SetFile(const Value: String);
begin
  FFile := Value;

  if (csLoading in TComponent(Collection.Owner).ComponentState) then
    Exit; //Do not perform load in the loading state : No disc access, all is already in property.

  if FileExists(FFile) then
  begin
    FStream.Clear;
    FStream.LoadFromFile(FFile);
    if UsageName = '' then
      UsageName := ExtractFileName(FFile); //We use UsageName to use the setter.
    FAssetType := FAssetType.ExtToType(ExtractFileExt(FFile));
    FFile := EmptyStr; //Delete file name. No need anymore. This property only used for design purpose.
  end;
end;

procedure TPXLAssetUnit.SetStream(const Value: TMemoryStream);
begin
  FStream := Value;
end;

procedure TPXLAssetUnit.SetUsageName(const Value: String);
var Inter, aOldUsageName : String;
    ctr : Integer;
begin
  inter := Value;
  ctr := 1;
  while not(TPXLAssetCollection(Collection).IsUsageNameOk(Index, inter)) do
  begin
    inter := 'Copy of '+ inter;

    //Prevent from locking.
    inc(ctr);
    if ctr >= 50 then
    begin
      raise Exception.Create('Unable to find asset usage name for asset "'+Value+'".');
    end;
  end;
  aOldUsageName := FUsageName;
  FUsageName := Inter;
end;

procedure TPXLAssetUnit.writeAssetType(Writer: TWriter);
begin
  Writer.WriteInteger(Integer(FAssetType));
end;

procedure TPXLAssetUnit.WriteStr(Stream: TStream);
begin
  FStream.SaveToStream(Stream);
end;

{ TPXLAssets }

constructor TPXLAssets.Create(aowner: TComponent);
begin
  inherited;
  FAsset := TPXLAssetCollection.Create(Self);
end;

destructor TPXLAssets.Destroy;
begin
  FAsset.Free;
  inherited;
end;

function TPXLAssets.GetAssetFromName(const aAssetName: String;
  out aAsset: TPXLAssetUnit): boolean;
var i : integer;
    lAsset : TPXLAssetUnit;
begin
  aAsset := Nil;
  Result := False;
  for I := 0 to Assets.Count-1 do
  begin
    lAsset := TPXLAssetUnit(Assets.Items[i]);
    if lowercase(lAsset.UsageName) = LowerCase(aAssetName) then
    begin
      aAsset := lAsset;
      Result := true;
    end;
  end;
end;

function TPXLAssets.GetAssetsCount: Integer;
begin
  Result := FAsset.Count;
end;

function TPXLAssets.GetImageString: String;
var i : integer;
begin
  result := '';
  for I := 0 to Assets.Count - 1 do
  begin
    if Result <> '' then
    begin
      Result := result + #13#10;
    end;
    Result := Result + TPXLAssetUnit(Assets.Items[i]).UsageName;
  end;
end;

function TPXLAssets.LoadAsset(aFileName: string): String;
var a : TPXLAssetUnit;
begin
  result := '';
  if FileExists(aFileName) then
  begin
    a := TPXLAssetUnit(Assets.Add);
    a.FileToLoad := aFileName;
    Result := a.UsageName;
  end;
end;

function TPXLAssets.TotalMemory: Int64;
var i : integer;
begin
  Result := 0;
  for i := 0 to Assets.Count - 1 do
  begin
    Result := Result + TPXLAssetUnit(Assets.Items[i]).GetStreamSize;
  end;
end;

function TPXLAssets.TotalMemoryAsString : String;
begin
  result :=ConvertMemoryAmountAsString(TotalMemory)
end;

{ TPXLAssetUnitTypeHelper }

function TPXLAssetUnitTypeHelper.ExtToType(
  const aExt: String): TPXLAssetUnitType;
var l : string;
begin
  Result := Unknown;
  l := UpperCase(aExt);
  if  l = '.BMP' then
    Result := ImageBMP;
  if  l = '.JPG' then
    Result := ImageJPG;
  if  l = '.PNG' then
    Result := ImagePNG;
end;

function TPXLAssetUnitTypeHelper.TypeToExt: String;
begin
  Result := EmptyStr;
  case Self of
    ImageBMP : Result := '.BMP';
    ImageJPG : Result := '.JPG';
    ImagePNG : Result := '.PNG';
  end;
end;


function TPXLAssetUnitTypeHelper.IsImageType: Boolean;
begin
  Result := (Integer(ImageBMP) = Pred(Integer(self))) Or
            (Integer(ImageJPG) = Pred(Integer(self))) Or
            (Integer(ImagePNG) = Pred(Integer(self)));
end;


end.

