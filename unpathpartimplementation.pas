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
    function GetCircle : ICircle;
    function GetVector : IBasicVector;
    function ToString(): String;
    property Circle : ICircle read GetCircle;
    property Vector : IBasicVector read GetVector;
  public
    constructor Create(const intfCircle : ICircle; const intfVector :IBasicVector);
  end;

  { TPathPartList }

  TPathPartList = class(TInterfacedObject, IPathPartList)
  private
    FList : TObject;
  protected
    function getItem(const iIndex: Integer):IPathPart;
    function GetCount:Cardinal;
    procedure Clear;
    procedure Add(const intfPathPart : IPathPart);
    property Count: Cardinal read GetCount;
    property Item[const iIndex: Integer]: IPathPart read GetItem; default;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses fgl, unCirclePhysics;

type TIntPathPartList= specialize TFPGInterfacedObjectList<IPathPart>;

{ TPathPartList }

function TPathPartList.getItem(const iIndex: Integer): IPathPart;
begin
  RESULT := TIntPathPartList(FList)[iIndex];
end;

function TPathPartList.GetCount: Cardinal;
begin
   RESULT := TIntPathPartList(FList).Count;
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
  RESULT := FintfCircle;
end;

function TPathPart.GetVector: IBasicVector;
begin
  RESULT := FintfVector;
end;

function TPathPart.ToString: String;
begin
  RESULT := Format('Part: Origin: (%f,%f), Angle: %f, Velocity: %f', [FintfVector.GetOrigin.x, FintfVector.GetOrigin.Y, FintfVector.Angle, FintfVector.InitialVelocity]);
end;

constructor TPathPart.Create(const intfCircle: ICircle;
  const intfVector: IBasicVector);
begin
   FintfCircle := intfCircle;
   FintfVector := intfVector.Clone;
end;

end.

