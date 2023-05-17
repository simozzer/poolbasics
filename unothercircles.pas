unit unOtherCircles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Fgl, unCirclePhysics, unHelperInterfaces, types;

type

  { TBaseCircle }

  TBaseCircle = class(TInterfacedObject, ICircle, IIdentity)
  private
    FdRadius: double;
    FdMass: double;
    FclrBrush: TColor;
    FclrPen: TColor;
    FiID: cardinal;
  protected
    function GetRadius: double;
    function GetBrushColor: TColor;
    procedure SetBrushColor(const clr: TColor);
    function GetPenColor: TColor;
    procedure SetPenColor(const clr: TColor);
    function GetMass: double;
    function GetId: cardinal;
    function ToString: ansistring; override;
  public
    constructor Create(const dRadius, dMass: double);
  end;

  { TMovingCircle }

  TMovingCircle = class(TBaseCircle, IObjectWithVector)
  private
    FBasicVector: IBasicVector;
    function GetBasicVector: IBasicVector;
    function Clone: IUnknown;
  public
    property Vector: IBasicVector read GetBasicVector;
    constructor Create(const ptOrigin: TPointF; const dRadius, dMass: double);
    destructor Destroy; override;
  end;


  { TCirclesList }

  TCirclesList = class(TInterfacedObject, ICirclesList)
  private
    FList: TObject;
  protected
    function GetItem(const iIndex: integer): ICircle;
    function GetCount: cardinal;
    procedure Clear;
    procedure Add(const intfCircle: ICircle);
  public
    property Count: cardinal read GetCount;
    property Item[const iIndex: integer]: ICircle read GetItem; default;
    constructor Create;
    destructor Destroy; override;
  end;

  { TCircleInterfaceAccess }

  TCircleInterfaceAccess = class
  public
    class function GetVectorFromCircle(const intfCircle: ICircle): IBasicVector;
  end;


implementation

type
  // Contains a list of ICircle
  TInternalCirclesList = specialize TFPGInterfacedObjectList<ICircle>;

var
  miCircleIdSequence: cardinal;

{ TCircleInterfaceAccess }

class function TCircleInterfaceAccess.GetVectorFromCircle(
  const intfCircle: ICircle): IBasicVector;
var
  intfVectorAccess: IObjectWithVector;
begin
  Result := nil;
  if not supports(intfCircle, IObjectWithVector, intfVectorAccess) then
    raise Exception.Create('Could not obtain IObjectWithVector')
  else
    Result := intfVectorAccess.Vector;
end;


{ TCirclesList }

function TCirclesList.GetItem(const iIndex: integer): ICircle;
begin
  Result := TInternalCirclesList(FList)[iIndex];
end;

function TCirclesList.GetCount: cardinal;
begin
  Result := TInternalCirclesList(FList).Count;
end;

procedure TCirclesList.Clear;
begin
  TInternalCirclesList(FList).Clear;
end;

procedure TCirclesList.Add(const intfCircle: ICircle);
begin
  TInternalCirclesList(FList).Add(intfCircle);
end;

constructor TCirclesList.Create;
begin
  FList := TInternalCirclesList.Create;
end;

destructor TCirclesList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{ TMovingCircle }

function TMovingCircle.GetBasicVector: IBasicVector;
begin
  Result := FBasicVector;
end;

function TMovingCircle.Clone: IUnknown;
var
  Acircle: TMovingCircle;
begin
  Acircle := TMovingCircle.Create(FBasicVector.Origin, GetRadius, GetMass);
  Acircle.SetBrushColor(GetBrushColor);
  Acircle.SetPenColor(GetPenColor);
  Acircle.FBasicVector := GetBasicVector.Clone;
  Result := Acircle;
end;

constructor TMovingCircle.Create(const ptOrigin: TPointF; const dRadius, dMass: double);
begin
  inherited Create(dRadius, dMass);
  FBasicVector := TBasicVector.Create(ptOrigin, 0, 0, 0);
end;

destructor TMovingCircle.Destroy;
begin
  FBasicVector := nil;
  inherited Destroy;
end;


{ TBaseCircle }

function TBaseCircle.GetRadius: double;
begin
  Result := FdRadius;
end;

function TBaseCircle.GetBrushColor: TColor;
begin
  Result := FclrBrush;
end;

procedure TBaseCircle.SetBrushColor(const clr: TColor);
begin
  FclrBrush := clr;
end;

function TBaseCircle.GetPenColor: TColor;
begin
  Result := FclrPen;
end;

procedure TBaseCircle.SetPenColor(const clr: TColor);
begin
  FclrPen := clr;
end;


function TBaseCircle.GetMass: double;
begin
  Result := FdMass;
end;


function TBaseCircle.GetId: cardinal;
begin
  Result := FiId;
end;

function TBaseCircle.ToString: ansistring;
begin
  Result := Format('Radius: %f, Mass: %f', [FdRadius, FdMass]);
end;

constructor TBaseCircle.Create(const dRadius, dMass: double);
begin
  FdRadius := dRadius;
  FclrBrush := clWhite;
  FclrPen := clBlack;
  FdMass := dMass;
  miCircleIdSequence := miCircleIdSequence + 1;
  FiID := miCircleIdSequence;
end;


initialization
  miCircleIdSequence := 1;

end.
