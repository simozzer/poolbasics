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
    FdMass: Double;
    FclrBrush: TColor;
    FclrPen: TColor;
    FbStationary: boolean;
  protected
    function GetRadius: double;
    function GetBrushColor: TColor;
    procedure SetBrushColor(const clr: TColor);
    function GetPenColor: TColor;
    procedure SetPenColor(const clr: TColor);
    function GetStationary: boolean;
    function GetMass : Double;
    procedure SetStationary(const bStationary: boolean);
    function GetId :Cardinal;
    function ToString: ansistring; override;
  public
    constructor Create(const dRadius, dMass: double);
  end;

  { TMovingCircle }

  TMovingCircle = class(TBaseCircle, IObjectWithVector)
  private
     FBasicVector: IBasicVector;
     function GetBasicVector : IBasicVector;
     function Clone: ICircle;
  public
    property Vector : IBasicVector read GetBasicVector;
    constructor Create(const ptOrigin: TPointF; const dRadius, dMass: double);
    destructor Destroy; override;
  end;


  { TCirclesList }

  TCirclesList = class(TInterfacedObject, ICirclesList)
  private
    FList : TObject;
  protected
    function GetItem(const iIndex : Integer): ICircle;
    function GetCount:Cardinal;
    procedure Clear;
    procedure Add(const intfCircle : ICircle);
  public
    property Count: Cardinal read GetCount;
    property Item[const iIndex: Integer]: ICircle read GetItem; default;
    constructor Create;
    destructor Destroy; override;
  end;


implementation

type
    // Contains a list of ICircle
  TInternalCirclesList = specialize TFPGInterfacedObjectList<ICircle>;

{ TCirclesList }

function TCirclesList.GetItem(const iIndex: Integer): ICircle;
begin
  RESULT := TInternalCirclesList(FList)[iIndex];
end;

function TCirclesList.GetCount: Cardinal;
begin
  RESULT := TInternalCirclesList(FList).Count;
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
  RESULT := FBasicVector;
end;

function TMovingCircle.Clone: ICircle;
var
  Acircle : TMovingCircle;
begin
  Acircle := TMovingCircle.Create(FBasicVector.Origin,GetRadius, GetMass);
  Acircle.SetBrushColor(GetBrushColor);
  Acircle.SetPenColor(GetPenColor);
  Acircle.FBasicVector := GetBasicVector.Clone;
  Result := Acircle;
end;

constructor TMovingCircle.Create(const ptOrigin: TPointF; const dRadius, dMass: double);
begin
  inherited Create(dRadius, dMass);
  FBasicVector := TBasicVector.Create(ptOrigin,0,0,0);
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

function TBaseCircle.GetStationary: boolean;
begin
  Result := FbStationary;
end;

function TBaseCircle.GetMass: Double;
begin
  RESULT := FdMass;
end;

procedure TBaseCircle.SetStationary(const bStationary: boolean);
begin
  FbStationary := bStationary;
end;

function TBaseCircle.GetId: Cardinal;
begin
  RESULT := Cardinal(@Self);
end;

function TBaseCircle.ToString: ansistring;
begin
  Result:=Format('Radius: %f, Mass: %f',[FdRadius, FdMass]);
end;


constructor TBaseCircle.Create(const dRadius, dMass: double);
begin
  FdRadius := dRadius;
  FclrBrush := clWhite;
  FclrPen := clBlack;
  FdMass:= dMass;
  FbStationary := True;
end;

{
function TBaseCircle.Distance(const dOtherCenterX, dOtherCenterY: double): double;
begin
  Result := Sqrt(Sqr(dOtherCenterX - FdCenterX) + Sqr(dOtherCenterY - FdCenterY));
end;
}

{
procedure TBaseCircle.Render(const ACanvas: TCanvas);
begin
  ACanvas.Brush.color := FclrBrush;
  ACanvas.Pen.Color := FclrPen;
  ACanvas.Ellipse(Round(FdCenterX - FdRadius), Round(FdCenterY - FdRadius),
    Round(FdCenterX + FdRadius), Round(FdCenterY + FdRadius));
end;
}

end.
