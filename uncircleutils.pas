unit unCircleUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unHelperInterfaces;

type

  { TCircleUtils }

  TCircleUtils = class
  public
    class function GetPathPartForCircleID(const intfList: IPathPartList;
      const iCircleID: integer): IPathPart;

    class function GetCircleId(const intfCircle : ICircle): Integer;

  end;

implementation

class function TCircleUtils.GetPathPartForCircleID(const intfList: IPathPartList;
  const iCircleID: integer): IPathPart;
var
  i: integer;
begin
  Result := nil;
  i := 0;
  while (i < intfList.Count) do
  begin
    if getCircleId(intfList[i].Circle) = iCircleID then
    begin
      Result := intfList[i];
      exit;
    end;
    Inc(i);
  end;

end;

class function TCircleUtils.GetCircleId(const intfCircle: ICircle): Integer;
var
  intfIdentity : IIdentity;
begin
  if supports(intfCircle, IIdentity, intfIdentity) then
    RESULT := intfIdentity.Id
  else
    Raise Exception.Create('Could not obtain ICircle');
end;

end.
