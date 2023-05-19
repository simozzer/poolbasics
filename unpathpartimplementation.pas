unit unPathPartImplementation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unHelperInterfaces;

type

  { TPathPart }

  TPathPart = class(TInterfacedObject, IPathPart)
  private
    FintfCircle: ICircle;
    FintfVector: IBasicVector;
  protected
    function GetCircle: ICircle;
    function GetVector: IBasicVector;
    function ToString(): string;
  public
    constructor Create(const intfCircle: ICircle; const intfVector: IBasicVector);
  end;

  { TPathPartList }

  TPathPartList = class(TInterfacedObject, IPathPartList)
  private
    FList: TObject;
  protected
    function getItem(const iIndex: integer): IPathPart;
    function GetCount: cardinal;
    procedure Clear;
    procedure Add(const intfPathPart: IPathPart);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses fgl;

type
  TIntPathPartList = specialize TFPGInterfacedObjectList<IPathPart>;

{ TPathPartList }

function TPathPartList.getItem(const iIndex: integer): IPathPart;
begin
  Result := TIntPathPartList(FList)[iIndex];
end;

function TPathPartList.GetCount: cardinal;
begin
  Result := TIntPathPartList(FList).Count;
end;

procedure TPathPartList.Clear;
begin
  TIntPathPartList(FList).Clear;
end;

procedure TPathPartList.Add(const intfPathPart: IPathPart);
begin
  TIntPathPartList(FList).Add(intfPathPart);
end;

constructor TPathPartList.Create;
begin
  FList := TIntPathPartList.Create;
end;

destructor TPathPartList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{ TPathPart }

function TPathPart.GetCircle: ICircle;
begin
  Result := FintfCircle;
end;

function TPathPart.GetVector: IBasicVector;
begin
  Result := FintfVector;
end;

function TPathPart.ToString: string;
begin
  Result := Format('Part: Origin: (%f,%f), Angle: %f, Velocity: %f, %s',
    [FintfVector.GetOrigin.x, FintfVector.GetOrigin.Y, FintfVector.Angle,
    FintfVector.InitialVelocity, FintfCircle.ToString()]);
end;

constructor TPathPart.Create(const intfCircle: ICircle; const intfVector: IBasicVector);
begin
  FintfCircle := intfCircle;
  FintfVector := intfVector.Clone;
end;

end.
