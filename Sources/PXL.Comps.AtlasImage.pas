unit PXL.Comps.AtlasImage;
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
}
interface

Uses
  System.SysUtils, System.Variants, System.Classes,
  PXL.Images,
  PXL.Surfaces,
  PXL.Comps.Core,
  PXL.Comps.Assets,
  PXL.Comps.Surfaces;

Type

TCustomPXLAtlasImage = Class(TPXLComponent)
Private
Protected
  FEngine: TCustomPXLEngine;
  FAssets: TPXLAssets;
  FUsualName: string;
  FAtlasImage : TAtlasImage;

  function GetAtlasReady: Boolean;
  Procedure InternalCheckAssets;

  procedure SetEngine(const Value: TCustomPXLEngine);
  procedure SetAsset(const Value: TPXLAssets);
  procedure SetUsualName(const Value: string);

  function GetTextureCount: Cardinal;

  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
Public
  Constructor Create(AOwner: TComponent); Override;
  Destructor Destroy; Override;
  Procedure Loaded; Override;

  //This will give you an fully founctionnal atlas, ti use with UseImage PXL Drawing methods.
  Function GetAtlas : TAtlasImage;

  //This will give you an fully founctionnal atlas, independant from the component.
  //Use it for optimize memory usage.
  //Typical scenario :
  // - Use an Asset and a Atlas compoment for setting ressource,
  // - Get all atlas you need as copy.
  // - Release Atlas and Asset component --> Huge useless memory is done !
  Function GetAtlasCopy : TAtlasImage;

Published
  Property Engine : TCustomPXLEngine read FEngine Write SetEngine;
  Property Assets : TPXLAssets read FAssets Write SetAsset;
  property AssetUsualName : string read FUsualName Write SetUsualName;
  Property IsAtlasReady : Boolean read GetAtlasReady;
  Property TextureCount : Cardinal read GetTextureCount;
End;

Implementation


{ TCustomPXLAtlasImage }

constructor TCustomPXLAtlasImage.Create(AOwner: TComponent);
begin
  inherited;
  FAtlasImage := Nil;
  FEngine := Nil;
  FAssets := Nil;
  FUsualName := EmptyStr;
end;

destructor TCustomPXLAtlasImage.Destroy;
begin

  inherited;
end;

function TCustomPXLAtlasImage.GetAtlas: TAtlasImage;
begin
  Assert(IsAtlasReady);
  Result := FAtlasImage;
end;

function TCustomPXLAtlasImage.GetAtlasCopy: TAtlasImage;
var lAsset : TPXLAssetUnit;
begin
  Assert(IsAtlasReady);
  Result := TAtlasImage.Create(FEngine.DevicePXL);
  if Assigned(FAssets) then
  begin
    if FAssets.GetAssetFromName(FUsualName,lAsset) then
    begin
      if lAsset.StreamSize>0 then
      begin
        FAtlasImage.LoadFromStream(lAsset.AssetType.TypeToExt,lAsset.AvailableStream);
      end;
    end;
  end;
end;

function TCustomPXLAtlasImage.GetAtlasReady: Boolean;
begin
  Result := Assigned(FAtlasImage) And Assigned(FEngine);
end;

function TCustomPXLAtlasImage.GetTextureCount: Cardinal;
begin
  Result := 0;
  if IsAtlasReady then
  begin
    Result := FAtlasImage.TextureCount;
  end;
end;

procedure TCustomPXLAtlasImage.InternalCheckAssets;
var i : integer;
    lPXLAssetUnit : TPXLAssetUnit;
begin
  if Assigned(FAtlasImage) then
  begin
    FreeAndNil(FAtlasImage);
  end;

  if Assigned(FEngine) then
  begin
    if Assigned(FAssets) then
    begin
      If FAssets.GetAssetFromName(FUsualName,lPXLAssetUnit) then
      begin
        if lPXLAssetUnit.StreamSize>0 then
        begin
          FAtlasImage := TAtlasImage.Create(FEngine.DevicePXL);
          lPXLAssetUnit.AvailableStream.Position := 0;
          if lPXLAssetUnit.AssetType.IsImageType then
          begin
            FAtlasImage.LoadFromStream(lPXLAssetUnit.AssetType.TypeToExt,lPXLAssetUnit.AvailableStream);
            FUsualName := lPXLAssetUnit.UsageName; //Carrect case.
          end;
        end;
      end;
    end;
  end;
end;

procedure TCustomPXLAtlasImage.Loaded;
begin
  inherited;
  InternalCheckAssets;
end;

procedure TCustomPXLAtlasImage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  Inherited; //! do not forget ;)
  if (Operation = opRemove) Then
  begin
    if (AComponent Is TCustomPXLEngine) then
    begin
      AssetUsualName := EmptyStr; //FreeAtlas.
      FEngine := Nil;
    end;

    if (AComponent Is TPXLAssets) then
    begin
      AssetUsualName := EmptyStr; //FreeAtlas.
      FAssets := Nil;
    end;
  end;
end;


procedure TCustomPXLAtlasImage.SetAsset(const Value: TPXLAssets);
begin
  FAssets := Value;
  if Not(assigned(Assets)) then
  begin
    AssetUsualName := EmptyStr; //Will clean Atals is available.
  end;
end;


procedure TCustomPXLAtlasImage.SetEngine(const Value: TCustomPXLEngine);
begin
  FEngine := Value;
  if IsAtlasReady then
  begin
    //Since atlas is linked to device, we have to clean assetname.
    AssetUsualName := EmptyStr; //Will clean atlas if available.
  end;
end;

procedure TCustomPXLAtlasImage.SetUsualName(const Value: string);
begin
  FUsualName := Value;
  InternalCheckAssets;
end;

End.



